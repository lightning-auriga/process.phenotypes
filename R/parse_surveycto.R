#' Apply standard replacements for certain deprecated terms
#'
#' @param vec character vector
#' @return character vector with replacements applied
apply.replacements <- function(vec) {
  res <- gsub("tribes", "ancestries", vec, ignore.case = TRUE)
  res <- gsub("tribe", "ancestry", res, ignore.case = TRUE)
  res
}

#' Determine from a set of variables what the last
#' variable number was and return as integer
#'
#' @param variables list of previously added variables,
#' with names of the format "TAG#####.*"
#' @return numeric part of final named entry as integer
get.last.variable.number <- function(variables) {
  last.name <- names(variables)[length(names(variables))]
  last.num <- stringr::str_replace(last.name, "^.*(\\d{5}).*$", "\\1")
  as.integer(last.num)
}

#' Render SurveyCTO configuration "choices" tab
#' as a shared_models style yaml list
#'
#' @param df data.frame, input choices tab from SurveyCTO spreadsheet
#' with column headers "list_name", "value", and "label"
#' at least present
#' @param survey.type character vector; type column entries
#' from "survey" tab of CTO form configuration
#' @param na.values character vector; factor levels that should
#' be treated as NA
#' @return list of choice information as shared_models
#' yaml list, with all variables configured as categoricals
populate.choices <- function(df, survey.type, na.values) {
  stopifnot(
    ncol(df) >= 3,
    c("list_name", "value", "label") %in% colnames(df)
  )
  ## fix: apparently there can be blank lines in the choices tab
  df <- df[!is.na(df[, "list_name"]) & !is.na(df[, "value"]) & !is.na(df[, "label"]), ]
  df[, "list_name"] <- apply.replacements(df[, "list_name"])
  df[, "value"] <- apply.replacements(df[, "value"])
  df[, "label"] <- apply.replacements(df[, "label"])
  unique.list.names <- unique(df[, "list_name"])
  res <- list()
  for (list.name in unique.list.names) {
    list.name.values <- df[df[, "list_name"] == list.name, "value"]
    value.is.number <- stringr::str_detect(list.name.values, "^[0-9]+\\.0$")
    list.name.values[value.is.number] <- stringr::str_replace(
      list.name.values[value.is.number],
      "^([0-9]+)\\.0$",
      "\\1"
    )
    list.name.labels <- df[df[, "list_name"] == list.name, "label"]

    ## handle situations with irreconciliable factor levels
    ## if the shared model is invoked as both a multiple and single
    ## response, it is impossible to encode with a single factor
    ## specification due to how CTO encodes one hot responses
    ## to multiple selection variables
    variable.selections <- survey.type[stringr::str_detect(
      survey.type,
      paste(" ", list.name, "$", sep = "")
    )]
    variable.select.multiple <- variable.selections[stringr::str_detect(
      variable.selections,
      "^select_multiple "
    )]

    ## deal with the factor levels in CTO configuration not
    ## being unique, due to filter restrictions that are not
    ## represented in the output csv
    list.name.labels <- list.name.labels[!duplicated(list.name.values)]
    list.name.values <- list.name.values[!duplicated(list.name.values)]

    ## remove embedded newlines
    list.name.labels <- stringr::str_replace_all(list.name.labels, "\\r|\\n", "")
    list.name.values <- stringr::str_replace_all(list.name.values, "\\r|\\n", "")

    var.levels <- list()
    found.names <- c()
    na.levels <- c()
    for (i in seq_len(length(list.name.values))) {
      if (list.name.labels[i] %in% found.names) {
        next
      } else {
        found.names <- c(found.names, list.name.labels[i])
      }
      if (length(variable.select.multiple) == 0 &&
        tolower(list.name.labels[i]) %in% tolower(na.values)) {
        na.levels <- c(na.levels, list.name.values[i])
        next
      }
      lvl.tag <- paste("lvl", length(var.levels) + 1, sep = "")
      var.levels[[lvl.tag]] <- list(
        "name" = list.name.labels[i]
      )
      alternate.patterns <- list.name.values[list.name.labels == list.name.labels[i]]

      if (length(alternate.patterns) > 1) {
        ## try to deal with invalid level configurations
        if (length(variable.select.multiple) > 0) {
          ## if the variable is always a multiple response, then even the bad
          ## levels should be reported out separately, because due to the
          ## CTO one hot convention, it is never actually used as a categorical
          ## encoding
          found.names <- found.names[-length(found.names)]
          alternate.patterns <- list.name.values[i]
          if (length(variable.select.multiple) != length(variable.selections)) {
            warning(
              "for shared model ", list.name, " invalid factor levels exist ",
              "that can not be reconciled with automated configuration without ",
              "editing the CSV; please correct manually before generating phenotype report"
            )
          }
        }
      }
      ## escape regex special characters in alternate patterns
      alternate.patterns <- stringr::str_replace_all(alternate.patterns, "(\\(|\\)|\\|)|\\[|\\]", "\\\\\\1")
      var.levels[[lvl.tag]][["alternate_patterns"]] <- alternate.patterns
    }
    var.model <- list(
      "type" = "categorical",
      "levels" = var.levels
    )
    if (length(na.levels) > 0) {
      if (length(na.levels) == 1) {
        na.levels <- list(na.levels)
      }
      var.model[["na-values"]] <- na.levels
    }
    res[[list.name]] <- var.model
  }
  list("models" = res)
}

#' Initialize dataset yaml configuration
#' with basic default entries
#'
#' @param dataset.tag character vector; tag
#' for dataset
#' @return list representing dataset yaml
#' with a single placeholder variable
create.config <- function(dataset.tag) {
  res <- list()
  res[["tag"]] <- dataset.tag
  res[["globals"]] <- list(
    "min_age_for_inclusion" = 18,
    "max_invalid_datatypes_per_subject" = 10,
    "consent_inclusion_file" = NULL,
    "consent_exclusion_file" = NULL
  )
  res$variables <- list()
  res$variables[[paste(dataset.tag,
    stringr::str_pad("1", 5, pad = "0"),
    sep = ""
  )]] <- list(
    "name" = "SubmissionDate",
    "type" = "string",
    "suppress_reporting" = TRUE,
    "canonical_name" = "SubmissionDate"
  )
  res
}

#' Populate variables list with multiple response onehot variables
#'
#' @param choice.list data.frame; xlsx choices tab
#' @param shared.model character; name of categorical model from xlsx choices tab
#' @param varname character; harmonized variable name for multiple response group
#' @param name.value character; name entry for variable from xlsx survey tab
#' @param label.value character; label entry for variable from xlsx survey tab
#' @param res list; output yaml list in progress of being built
#' @return list; input res variable with variables extended with new entries
handle.multiple.levels <- function(choice.list, shared.model, varname, name.value, label.value, res) {
  for (level.num in seq_len(length(names(choice.list$models[[shared.model]]$levels)))) {
    ## due to possible removal of redundancy in name/alternate_patterns of shared.models,
    ## assign name as the tag but only if alternate_patterns is null
    lvl.tag <- choice.list$models[[shared.model]]$levels[[level.num]][["name"]]
    if (!is.null(choice.list$models[[shared.model]]$levels[[level.num]][["alternate_patterns"]])) {
      lvl.tag <- choice.list$models[[shared.model]]$levels[[level.num]][["alternate_patterns"]][1]
    }
    sub.varname <- paste(varname,
      lvl.tag,
      sep = "_"
    )
    res$variables[[sub.varname]] <- list(
      "name" = paste(name.value,
        lvl.tag,
        sep = "_"
      ),
      "shared_model" = "yesno",
      "canonical_name" = paste(label.value,
        ", indicator response for level ",
        choice.list$models[[shared.model]]$levels[[level.num]][["name"]],
        sep = ""
      )
    )
  }
  res
}

#' Construct variable annotation according to type information
#' from a SurveyCTO configuration row
#'
#' @param type.value character vector; entry from "type" column
#' @param name.value character vector; entry from "name" column
#' @param label.value character vector; entry from "label" column
#' @param choice.list list; shared model data for questionnaire
#' @param varname character vector; constructed name of variable (e.g. DT00001)
#' @return list; contents of variable summary for this variable, under
#' variable.summary$variables\[\[varname\]\]
build.variable.data <- function(type.value, name.value, label.value, choice.list, varname) {
  ## SurveyCTO apparently recognizes a series of builtin types
  ## that can be directly referenced and aliased
  cto.specials <- c(
    "start", "end", "deviceid", "subscriberid",
    "simserial", "phonenumber", "username",
    "caseid", "image"
  )
  res <- list(variables = list())
  if (length(type.value) < 1 || is.na(type.value) || type.value == "note" ||
    type.value == "begin group" || type.value == "end group") {
    res <- NULL
  } else {
    if (type.value %in% c(cto.specials, "text", "datetime")) {
      ## these all are ignorable strings for the moment
      res$variables[[varname]] <- list(
        "name" = name.value,
        "type" = "string",
        "suppress_reporting" = TRUE,
        "canonical_name" = ifelse(label.value == "", NA, label.value)
      )
    } else if (type.value %in% c("calculate", "date", "integer", "decimal")) {
      ## calculate variables have variable type,
      ## and will be set by default to string
      yaml.type <- ifelse(type.value == "calculate", "string",
        ifelse(type.value == "date", "date", "numeric")
      )
      res$variables[[varname]] <- list(
        "name" = name.value,
        "type" = yaml.type,
        "canonical_name" = label.value
      )
    } else if (stringr::str_detect(type.value, "^select_one")) {
      shared.model <- stringr::str_replace(type.value, "^select_one +([^ ]+) *$", "\\1")
      res$variables[[varname]] <- list(
        "name" = name.value,
        "shared_model" = shared.model,
        "canonical_name" = label.value
      )
    } else if (stringr::str_detect(type.value, "^select_multiple")) {
      shared.model <- stringr::str_replace(type.value, "^select_multiple +([^ ]+) *$", "\\1")
      ## select_multiple variables have nlevels+1 variable columns:
      ## one for all answers space delimited, and one each for each level as a binary outcome
      res$variables[[varname]] <- list(
        "name" = name.value,
        "type" = "string",
        "suppress_reporting" = TRUE,
        "canonical_name" = label.value
      )
      res <- handle.multiple.levels(choice.list, shared.model, varname, name.value, label.value, res)
    } else {
      warning(
        "unrecognized CTO type flag detected: \"", type.value,
        "\", for name \"", name.value, "\" and label \"", label.value, "\""
      )
      res <- NULL
    }
  }
  res
}

#' Add fixed trailing annotations to questionnaire configuration
#'
#' @details Survey responses have fixed output metadata columns
#' appended. The actual columns included seem to be pulled from
#' a somewhat amorpheous pool of options; more may need to be
#' added to the pool.
#'
#' @param out.yaml list; partially constructed variable summary
#' information for questionnaire
#' @param dataset.tag character vector; tag for this dataset
#' @param responses character vector; column names from actual
#' completed survey data
#' @return list; input yaml configuration with additional
#' variable information appended
add.trailing.metadata <- function(out.yaml, dataset.tag, responses) {
  varnames <- c(
    "instanceID",
    "instanceName",
    "formdef_version",
    "KEY",
    "review_quality",
    "review_comments",
    "review_status",
    "review_corrections"
  )
  response.indices <- seq_len(length(responses))
  names(response.indices) <- responses
  varnames <- varnames[varnames %in% responses]
  if (length(varnames) > 0) {
    required.tags <- sort(response.indices[varnames])
    for (varname in names(required.tags)) {
      out.yaml$variables[[paste(dataset.tag,
        stringr::str_pad(get.last.variable.number(out.yaml$variables) + 1, 5, pad = "0"),
        sep = ""
      )]] <- list(
        "name" = varname,
        "type" = "string",
        "suppress_reporting" = TRUE,
        "canonical_name" = varname
      )
    }
  }
  out.yaml
}

#' Create variable annotations for repeat variable blocks
#'
#' @param out.yaml list; partially constructed variable summary
#' @param cur.varname character vector; constructed name of repeat start variable
#' @param name.value character vector; name column entry for repeat start variable
#' @param label.value character vector; label column entry for repeat start variable
#' @param survey data.frame; survey tab from SurveyCTO configuration xlsx file
#' @param dataset.tag character vector; tag for current dataset
#' @param responses character vector; column names of completed questionnaire csv
#' @param choice.list list; shared model data
#' @param i index of repeat start variable in survey configuration table
#' @return list; input variable yaml configuration with repeat variable block
#' data added as entry "out.yaml", and incremented global counter as entry "i"
handle.repeat.variables <- function(out.yaml, cur.varname, name.value,
                                    label.value, survey, dataset.tag, responses,
                                    choice.list, i) {
  count.var.present <- length(which(responses == paste(name.value, "count", sep = "_"))) > 0
  if (count.var.present) {
    out.yaml$variables[[paste(cur.varname, "count", sep = "_")]] <- list(
      "name" = paste(name.value, "count", sep = "_"),
      "type" = "numeric",
      "canonical_name" = paste(label.value,
        ", count of responses",
        sep = ""
      )
    )
  }
  repeat.variables <- list(variables = list())
  query.varname <- NULL
  repeat.increment <- 0
  while (TRUE) {
    i <- i + 1
    repeat.increment <- repeat.increment + 1
    type.value <- survey[i, "type"]
    if (type.value == "end repeat") break
    name.value <- survey[i, "name"]
    label.value <- survey[i, "label"]
    cur.varname <- paste(dataset.tag,
      stringr::str_pad(get.last.variable.number(out.yaml$variables) + repeat.increment,
        5,
        pad = "0"
      ),
      sep = ""
    )
    if (is.null(query.varname)) {
      query.varname <- name.value
    } # else if () {

    # }
    variable.data <- build.variable.data(type.value, name.value, label.value, choice.list, cur.varname)
    for (variable in names(variable.data$variables)) {
      repeat.variables$variables[[variable]] <- variable.data$variables[[variable]]
    }
  }
  ## initial logic here was not complex enough to handle multiple response variables
  ## embedded in a repeat block. this now pulls all instances of initial variable
  ## of a repeat block, does some string reduction, and then parses the repeats from there
  repeat.obs <- responses[stringr::str_detect(responses, paste("^", query.varname, "_", sep = ""))]
  ## if there are names in repeat blocks that are subsets of one another, this will mess
  ## with the repeat count tracking (e.g. varA_1, varA_2, varA_other_1 will resolve to expecting
  ## only a single repeat of the varA-containing block).  This logic attempts to restrict the
  ## name matching by filtering out names of which the query is a subset.
  filter.values <- survey$name[stringr::str_detect(survey$name, paste("^", query.varname, sep = ""))]
  filter.values <- filter.values[!(filter.values %in% query.varname)]
  for (f in filter.values) {
    repeat.obs <- repeat.obs[!stringr::str_detect(repeat.obs, paste("^", f, sep = ""))]
  }
  n.repeats <- as.integer(stringr::str_replace(repeat.obs[length(repeat.obs)], "^.*_([0-9]+)$", "\\1"))
  if (length(n.repeats) > 0) {
    for (n.repeat in seq_len(n.repeats)) {
      for (repeat.variable in names(repeat.variables$variables)) {
        repeat.data <- repeat.variables$variables[[repeat.variable]]
        repeat.data$name <- paste(repeat.data$name, n.repeat, sep = "_")
        repeat.data[["canonical_name"]] <- paste(repeat.data[["canonical_name"]],
          ", repeat observation ", n.repeat,
          sep = ""
        )
        out.yaml$variables[[paste(repeat.variable, n.repeat, sep = "_")]] <- repeat.data
      }
    }
  }
  list(
    out.yaml = out.yaml,
    i = i
  )
}

#' Add flags for subject ID and age variables
#'
#' @param out.yaml list; constructed variable configuration data
#' @param subject.id.name character vector; expected name of subject ID variable
#' @param age.name character vector; expected name of age variable
#' @return list; input yaml with flags added to appropriate variables
flag.required.variables <- function(out.yaml, subject.id.name, age.name) {
  subject.found <- FALSE
  age.found <- FALSE
  for (varname in names(out.yaml$variables)) {
    if (out.yaml$variables[[varname]]$name == subject.id.name) {
      if (subject.found) {
        stop("duplicate apparent subject ID columns; check survey name column")
      }
      subject.found <- TRUE
      out.yaml$variables[[varname]][["subject_id"]] <- TRUE
    } else if (out.yaml$variables[[varname]]$name == age.name) {
      if (age.found) {
        stop("duplicate apparent subject age columns; check survey name column")
      }
      age.found <- TRUE
      out.yaml$variables[[varname]][["subject_age"]] <- TRUE
    }
  }
  if (!subject.found || !age.found) {
    stop("unable to find subject ID/age in variable names")
  }
  out.yaml
}

#' Convert SurveyCTO configuration data into yaml configuration for this package
#'
#' @param in.form.filename character vector; name of xlsx survey
#' configuration file from SurveyCTO
#' @param in.response.filename character vector; name of csv
#' response data from SurveyCTO in wide format
#' @param dataset.tag character vector; tag for this dataset,
#' which is used as prefix for synthetic variable names
#' @param out.yaml.filename character vector; destination filename
#' for variable configuration data
#' @param out.shared.models character vector; destination filename
#' for shared categorical model data for this questionnaire
#' @param subject.id.name character vector; name of variable containing subject ID
#' @param age.name character vector; name of variable containing subject age
#' @param na.values character vector; factor levels that should be treated as NA
#' @export
parse.surveycto <- function(in.form.filename, in.response.filename, dataset.tag, out.yaml.filename, out.shared.models,
                            subject.id.name = "pid", age.name = "age",
                            na.values = c("I don't know/not sure", "Prefer not to answer")) {
  survey <- openxlsx::read.xlsx(in.form.filename, sheet = "survey")
  stopifnot(c("type", "name", "label") %in% colnames(survey))
  ## as we don't know the formatting requirements for form specifications, the set of entries
  ## that can be safely skipped keeps changing. an "end repeat" entry with blank "name" entry has
  ## been detected in a valid survey configuration; therefore, at least the repeat block delimiters
  ## seem to permit null names. intuitively, this probably also extends to groups, as they seem
  ## to work somewhat similarly (in the sense that their name is not actually used?)
  survey <- survey[(!is.na(survey$type) & !is.na(survey$name)) |
    (!is.na(survey$type) & survey$type %in% c(
      "begin repeat", "end repeat",
      "begin group", "end group"
    )), ]
  survey$name <- apply.replacements(survey$name)
  survey$name <- stringr::str_trim(survey$name, side = "right")
  survey$type <- apply.replacements(survey$type)
  survey$label <- apply.replacements(survey$label)
  choices <- openxlsx::read.xlsx(in.form.filename, sheet = "choices")
  choice.list <- populate.choices(choices, survey$type, na.values)
  responses <- colnames(read.table(in.response.filename,
    sep = ",", comment.char = "",
    quote = "\"", header = TRUE, nrows = 1, check.names = FALSE
  ))
  responses <- apply.replacements(responses)
  out.yaml <- create.config(dataset.tag)
  i <- 1
  while (i <= nrow(survey)) {
    type.value <- survey[i, "type"]
    name.value <- survey[i, "name"]
    label.value <- survey[i, "label"]
    cur.varname <- paste(dataset.tag,
      stringr::str_pad(get.last.variable.number(out.yaml$variables) + 1, 5, pad = "0"),
      sep = ""
    )
    if (type.value == "begin repeat") {
      repeat.result <- handle.repeat.variables(
        out.yaml, cur.varname, name.value,
        label.value, survey, dataset.tag, responses,
        choice.list, i
      )
      out.yaml <- repeat.result$out.yaml
      i <- repeat.result$i
    } else {
      variable.data <- build.variable.data(type.value, name.value, label.value, choice.list, cur.varname)
      if (is.null(variable.data)) {
        i <- i + 1
        next
      }
      for (varname in names(variable.data$variables)) {
        out.yaml$variables[[varname]] <- variable.data$variables[[varname]]
      }
    }
    i <- i + 1
  }
  out.yaml <- add.trailing.metadata(out.yaml, dataset.tag, responses)
  ## sanity check: output content should match response headers
  output.predicted.headers <- unname(sapply(out.yaml$variables, function(i) {
    i$name
  }))
  if (!identical(output.predicted.headers, responses)) {
    headers.not.present <- responses[!(responses %in% output.predicted.headers)]
    excess.headers.in.prediction <- output.predicted.headers[!(output.predicted.headers %in% responses)]
    if (length(headers.not.present) > 0) {
      print("computed result variables missing real output variables")
      print(headers.not.present)
    }
    if (length(excess.headers.in.prediction) > 0) {
      print("computed result variables are not present in real data")
      print(excess.headers.in.prediction)
    }
    if (length(excess.headers.in.prediction) == 0 &&
      length(headers.not.present) == 0) {
      print("output variables are correct but in the wrong order")
      aligned <- cbind(responses, output.predicted.headers)
      print(aligned[aligned[, 1] != aligned[, 2], ])
    }
    stop("output variable prediction has failed")
  }
  out.yaml <- flag.required.variables(out.yaml, subject.id.name, age.name)
  yaml::write_yaml(out.yaml, out.yaml.filename,
    fileEncoding = "UTF-8",
    handlers = list(seq = function(x) x)
  )
  ## after column name resolution is complete, only then set categorical
  ## levels to lowercase versions
  for (model.name in names(choice.list$models)) {
    for (model.lvl in names(choice.list$models[[model.name]]$levels)) {
      choice.list$models[[model.name]]$levels[[model.lvl]]$alternate_patterns <-
        ifelse(length(choice.list$models[[model.name]]$levels[[model.lvl]]$alternate_patterns) == 1,
          list(tolower(choice.list$models[[model.name]]$levels[[model.lvl]]$alternate_patterns)),
          tolower(choice.list$models[[model.name]]$levels[[model.lvl]]$alternate_patterns)
        )
    }
  }
  yaml::write_yaml(choice.list, out.shared.models,
    fileEncoding = "UTF-8",
    handlers = list(seq = function(x) x)
  )
}



#' For repeat expansion, handle the situation where there
#' is a repeat variable that was not present in any form
#' in the original configuration file.
#'
#' @param varname string; formatted variable name for predicted
#' insertion point
#' @param regenerated.yaml list; loaded dataset yaml corresponding
#' to current actual wide csv export from SurveyCTO. Expected to
#' be the loaded output of parse.surveycto
#' @param max.existing.number numeric; maximum variable number,
#' expecting variable names of the format "HW#####"
#' @param res.variables list; accumulator of output variable
#' configuration with repeats added or extended as needed
#' @param missing.colname character; encountered column name
#' that is missing from current configuration. Expected to be
#' correctly predicted and present in parse.surveycto configuration
#' based on the current form definition and csv export
#' @return list; res.variables, updated version of input parameter
#' with the new resolved variable configuration entry added; max.existing.number,
#' updated version of input parameter incremented to reflect variable addition
handle.missing.block <- function(varname,
                                 regenerated.yaml,
                                 max.existing.number,
                                 res.variables,
                                 missing.colname) {
  match.first.name <- names(regenerated.yaml$variables)[lapply(regenerated.yaml$variables, function(i) {
    i$name
  }) == missing.colname]
  stopifnot("handle.missing.block found irreconcilable variable" = length(match.first.name) == 1)

  data <- regenerated.yaml$variables[[match.first.name]]

  max.existing.number <- max.existing.number + 1
  new.varname <- paste(stringr::str_replace(
    varname,
    "^([^\\d]+)\\d+.*$",
    "\\1"
  ),
  stringr::str_pad(max.existing.number, 5, pad = "0"),
  stringr::str_replace(match.first.name, "^[^_]+(_.*)$", "\\1"),
  sep = ""
  )
  res.variables[[new.varname]] <- data

  list(
    res.variables = res.variables,
    max.existing.number = max.existing.number
  )
}


#' For repeat expansion, handle the situation where there
#' is a repeat variable that was present
#' in the original configuration file, but that has a higher
#' number repeat present in the current SurveyCTO export
#'
#' @param var.prefix character; shared common prefix of the dataset
#' yaml configuration for this repeat variable. So for example,
#' if the existing repeat block was "HW00001_1", "HW00001_2", "HW00001_3",
#' this would be "HW00001_"
#' @param trailing.num numeric; instance of the repeat observed
#' in the new dataset but absent from original configuration
#' @param res.variables list; accumulator of output variable
#' configuration with repeats added or extended as needed
#' @param missing.colname character; encountered column name
#' that is missing from current configuration. Expected to be
#' correctly predicted and present in parse.surveycto configuration
#' based on the current form definition and csv export
#' @return list; updated version of input parameter 'res.variables'
#' with the new resolved variable configuration entry added
handle.existing.block <- function(var.prefix,
                                  trailing.num,
                                  res.variables,
                                  missing.colname) {
  first.entry <- paste(var.prefix, "1", sep = "")
  name.match <- lapply(res.variables, function(i) {
    i$name
  }) == first.entry
  match.first.name <- names(res.variables)[name.match]
  stopifnot("handle.existing.block found irreconcilable variable" = length(match.first.name) == 1)
  data <- res.variables[[match.first.name]]
  data$name <- missing.colname
  data$canonical_name <- stringr::str_replace(
    data$canonical_name,
    "repeat observation 1$",
    paste("repeat observation", trailing.num)
  )
  res.variables <- c(res.variables, list(data))
  names(res.variables)[length(res.variables)] <- stringr::str_replace(
    match.first.name,
    "\\d+$",
    as.character(trailing.num)
  )
  res.variables
}


#' For repeat expansion: run an internal pass
#' of parse.surveycto to reuse the logic for
#' generating configuration blocks in case new
#' blocks have magically appeared in new SurveyCTO
#' exports
#'
#' @param existing.yaml list; existing dataset configuration
#' loaded with read_yaml. Note that the entire configuration
#' need not be correctly specified, but the variables containing
#' the mandatory 'subject_id' and 'subject_age' tags should
#' be correctly specified and ordered
#' @param form.definition.filename character; name of
#' SurveyCTO xlsx form definition
#' @param new.data.filename character; name of wide csv
#' export from SurveyCTO
#' @param intermediate.dataset.yaml character; name of
#' output dataset yaml from parse.surveycto
#' @param intermediate.shared.models character; name
#' of output shared models yaml from parse.surveycto
generate.predicted.yaml <- function(existing.yaml,
                                    form.definition.filename,
                                    new.data.filename,
                                    intermediate.dataset.yaml,
                                    intermediate.shared.models) {
  subject.id.name <- ""
  subject.age.name <- ""
  for (varname in names(existing.yaml$variables)) {
    if (!is.null(existing.yaml$variables[[varname]]$subject_id)) {
      if (existing.yaml$variables[[varname]]$subject_id) {
        subject.id.name <- existing.yaml$variables[[varname]]$name
      }
    }
    if (!is.null(existing.yaml$variables[[varname]]$subject_age)) {
      if (existing.yaml$variables[[varname]]$subject_age) {
        subject.age.name <- existing.yaml$variables[[varname]]$name
      }
    }
  }
  parse.surveycto(form.definition.filename,
    new.data.filename,
    "WHOCARES",
    intermediate.dataset.yaml,
    intermediate.shared.models,
    subject.id.name = subject.id.name,
    age.name = subject.age.name
  )
  yaml::read_yaml(intermediate.dataset.yaml)
}

#' Utility function to update an existing configuration
#' file from parse.surveycto, given new SurveyCTO export data
#'
#' @details Certain types of output columns in exported SurveyCTO
#' datasets are unpredictable, in the sense that, with a constant
#' form definition, the columns may or may not be present, and can
#' only be predicted with access to the export itself. This creates
#' challenges when trying to generate a variable specification for
#' this package from which one can predictably use variable
#' references, for example with the creation of derived variables,
#' or in order to generate consensus variables between datasets.
#'
#' This function is designed to attempt to address the most
#' straightforward and yet fiddly versions of these kinds of
#' unpredictable variables. Repeat variables in SurveyCTO
#' form definitions expand based on the maximum number of repeat
#' responses observed in a dataset. Thus, in particular when
#' a data collection is very small, the set of output variables
#' can change drastically between data exports. Repeat variables
#' are not even guaranteed to be reported in the output at all
#' at first, until at least one respondent fills out at least
#' a single repeat response.
#'
#' Repeat blocks are padded out based on the number of repeat
#' observations in the current data export. The existing yaml's
#' repeat content is expanded with consistent naming: if the
#' repeat block variable prefix is "HW00001_", and repeats
#' "HW00001_1" and "HW00001_2" were already present, configuration
#' blocks are injected starting with "HW00001_3".
#'
#' In the case that the repeat block was not present *at all*
#' in the original configuration, the repeat block is configured
#' with an auxiliary call to parse.surveycto, and the resulting
#' blocks are relabled and injected into the existing dataset
#' yaml. Due to the naming conventions in these configuration files,
#' the block is assigned new base numbers based on the maximum
#' variable number detected in the existing config's 'variables'
#' block. This breaks the standard convention of the variable
#' base numbers being strictly ascending through the file.
#' However, due to the possibly complex downstream uses of the
#' variable names, this is the most reliable manner of extending
#' the configuration file. If other behavior is desired, this
#' function is probably not suitable for your needs. Note again
#' that this particular issue only applies when a repeat block
#' was entirely absent from the original configuration file;
#' as long as one repeat was originally present, the number ordering
#' is preserved.
#'
#' The raw wide csv output from SurveyCTO should be provided as
#' 'new.cto.export.filename'. In certain applications, the actual
#' file used with the existing configuration file may not be
#' the raw SurveyCTO export; if, for example, other columns
#' are appended externally before processing, that resulting
#' csv or tsv should be provided as 'new.processed.export.filename'.
#' If this application does not apply or makes no sense to you,
#' that parameter can be excluded and the raw SurveyCTO export
#' will correctly be used in its place.
#'
#' The parameters 'intermediate.dataset.yaml' and
#' 'intermediate.shared.models' specify where the intermediate
#' output of parse.surveycto should be emitted. These default
#' to tempfiles; however, in some instances, these intermediates
#' might be useful to keep around for some time; and this function
#' is specifically imagined to be inserted into a pipeline, which
#' will feature file tracking. If desired, the filenames for
#' these intermediates can be specified here, and their ultimate
#' fate can be controlled by e.g. Snakemake or some other controller.
#'
#' It is important to emphasize that this function is specifically
#' designed to handle repeat variables. It will not handle non-repeat
#' variables that were not present in the original configuration
#' but that have magically appeared in the new SurveyCTO export.
#' If such a variable is encountered, the function will error
#' with informative complaint, and the configuration will have
#' to be updated manually. Once the configuration is adjusted to
#' include these non-repeat variables, however, this function might
#' be called again to handle the repeat variables exclusively.
#'
#' @param existing.yaml.filename character; name of dataset yaml
#' (potentially with custom configuration data) from a run of
#' parse.surveycto
#' @param new.yaml.filename character; name of output dataset yaml
#' @param new.cto.export.filename character; name of most recent
#' wide-format csv output from SurveyCTO; tested with API output,
#' though it should work with web-based export as well
#' @param form.definition.filename character; name of SurveyCTO
#' form xlsx file that governs the format of the current csv export
#' @param new.processed.export.filename character; name of csv
#' output from SurveyCTO with any postprocessed modifications
#' (see Details)
#' @param intermediate.dataset.yaml character; name of dataset yaml
#' the function generates from parse.surveycto for comparison
#' purposes; if not specified, a generic tempfile
#' @param intermediate.shared.models character; name of
#' shared models yaml the file generates from parse.surveycto for
#' comparison purposes; if not specified, a generic tempfile
#' @export
expand.surveycto.config <- function(existing.yaml.filename,
                                    new.yaml.filename,
                                    new.cto.export.filename,
                                    form.definition.filename,
                                    new.processed.export.filename = new.cto.export.filename,
                                    intermediate.dataset.yaml = tempfile("expand.surveycto.dataset"),
                                    intermediate.shared.models = tempfile("expand.surveycto.shared_models")) {
  stopifnot(
    !is.null(existing.yaml.filename),
    !is.null(new.cto.export.filename),
    !is.null(new.processed.export.filename),
    !is.null(form.definition.filename),
    !is.null(intermediate.dataset.yaml),
    !is.null(intermediate.shared.models),
    !is.null(new.yaml.filename)
  )
  stopifnot(
    is.character(existing.yaml.filename),
    is.character(new.cto.export.filename),
    is.character(new.processed.export.filename),
    is.character(form.definition.filename),
    is.character(intermediate.dataset.yaml),
    is.character(intermediate.shared.models),
    is.character(new.yaml.filename)
  )
  stopifnot(
    length(existing.yaml.filename) == 1,
    length(new.cto.export.filename) == 1,
    length(new.processed.export.filename) == 1,
    length(form.definition.filename) == 1,
    length(intermediate.dataset.yaml) == 1,
    length(intermediate.shared.models) == 1,
    length(new.yaml.filename) == 1
  )
  stopifnot(
    file.exists(existing.yaml.filename),
    file.exists(new.cto.export.filename),
    file.exists(new.processed.export.filename),
    file.exists(form.definition.filename)
  )

  existing.yaml <- yaml::read_yaml(existing.yaml.filename)
  new.data <- read.table(new.processed.export.filename,
    header = TRUE,
    stringsAsFactors = FALSE, sep = "\t",
    comment.char = "", quote = "\"",
    check.names = FALSE
  )

  var.index <- 1
  res.variables <- list()

  regenerated.yaml <- generate.predicted.yaml(
    existing.yaml,
    form.definition.filename,
    new.cto.export.filename,
    intermediate.dataset.yaml,
    intermediate.shared.models
  )


  max.existing.number <- max(as.integer(stringr::str_replace(
    names(existing.yaml$variables),
    "^[^\\d]+(\\d+).*$",
    "\\1"
  )))

  colnames(new.data) <- stringr::str_replace(
    colnames(new.data),
    "tribe",
    "ancestry"
  )
  var.index <- 1
  while (var.index <= length(existing.yaml$variables)) {
    varname <- names(existing.yaml$variables)[var.index]
    if (existing.yaml$variables[[varname]]$name != colnames(new.data)[length(res.variables) + 1]) {
      predicted.colname <- existing.yaml$variables[[varname]]$name
      missing.colname <- colnames(new.data)[length(res.variables) + 1]
      if (stringr::str_detect(missing.colname, "_[0-9]+$")) {
        var.prefix <- stringr::str_replace(missing.colname, "^(.*_)\\d+$", "\\1")
        trailing.num <- as.integer(stringr::str_replace(
          missing.colname,
          "^.*_(\\d+)$",
          "\\1"
        ))
        if (trailing.num == 1) {
          res <- handle.missing.block(
            varname,
            regenerated.yaml,
            max.existing.number,
            res.variables,
            missing.colname
          )
          res.variables <- res$res.variables
          max.existing.number <- res$max.existing.number
        } else {
          res.variables <- handle.existing.block(
            var.prefix,
            trailing.num,
            res.variables,
            missing.colname
          )
        }
      } else {
        stop("encountered irreconcilable column in data from CTO: ",
          missing.colname, " (config expected ", predicted.colname, ")",
          sep = ""
        )
      }
    } else {
      res.variables[[varname]] <- existing.yaml$variables[[varname]]
      var.index <- var.index + 1
    }
  }
  while (length(res.variables) < length(regenerated.yaml$variables)) {
    res <- handle.missing.block(
      names(existing.yaml$variables[1]),
      regenerated.yaml,
      max.existing.number,
      res.variables,
      colnames(new.data)[length(res.variables) + 1]
    )
    res.variables <- res$res.variables
    max.existing.number <- res$max.existing.number
  }
  ## final sanity check: the names of the constructed variable set
  ## should match the names of the new data csv. there is a situation
  ## in which the old yaml ends with a match but is short a number
  ## of csv variables that happens to match the number of repeats
  ## that were added that the above logic misses
  predicted.names <- unname(unlist(lapply(res.variables, function(i) {
    i$name
  })))
  if (!identical(predicted.names, colnames(new.data))) {
    stop(
      "unexpected variables were detected during repeat block expansion",
      "that cannot be handled:", colnames(new.data)[!(colnames(new.data) %in% predicted.names)]
    )
  }
  existing.yaml$variables <- res.variables
  yaml::write_yaml(existing.yaml, new.yaml.filename)
}
