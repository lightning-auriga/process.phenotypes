#' Render SurveyCTO configuration "choices" tab
#' as a shared_models style yaml list
#'
#' @param df data.frame, input choices tab from SurveyCTO spreadsheet
#' with column headers "list_name", "value", and "label"
#' at least present
#' @return list of choice information as shared_models
#' yaml list, with all variables configured as categoricals
populate.choices <- function(df) {
  stopifnot(
    ncol(df) >= 3,
    c("list_name", "value", "label") %in% colnames(df)
  )
  unique.list.names <- unique(df[, "list_name"])
  res <- list()
  for (list.name in unique.list.names) {
    list.name.values <- df[df[, "list_name"] == list.name, "value"]
    list.name.values <- as.vector(as.integer(as.numeric(list.name.values) + 0.5),
      mode = "character"
    )
    list.name.labels <- df[df[, "list_name"] == list.name, "label"]
    var.levels <- list()
    for (i in seq_len(length(list.name.values))) {
      var.levels[[paste("lvl", length(var.levels) + 1, sep = "")]] <- list(
        "name" = list.name.labels[i],
        "alternate_patterns" = rep(list.name.values[i], 2)
      )
    }
    var.model <- list(
      "type" = "categorical",
      "levels" = var.levels
    )
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
    "consent_inclusion_file" = "",
    "consent_exclusion_file" = ""
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

#' Construct variable annotation according to type information
#' from a SurveyCTO configuration row
#'
#' @param type.value character vector; entry from "type" column
#' @param name.value character vector; entry from "name" column
#' @param label.value character vector; entry from "label" column
#' @param choice.list list; shared model data for questionnaire
#' @param varname character vector; constructed name of variable (e.g. DT00001)
#' @return list; contents of variable summary for this variable, under
#' variable.summary$variables[[varname]]
build.variable.data <- function(type.value, name.value, label.value, choice.list, varname) {
  ## SurveyCTO apparently recognizes a series of builtin types
  ## that can be directly referenced and aliased
  cto.specials <- c(
    "start", "end", "deviceid", "subscriberid",
    "simserial", "phonenumber", "username",
    "caseid", "image"
  )
  res <- list(variables = list())
  if (length(type.value) < 1 | is.na(type.value) | type.value == "note" |
    type.value == "begin group" | type.value == "end group") {
    res <- NULL
  } else {
    if (type.value %in% cto.specials) {
      ## these all are ignorable strings for the moment
      res$variables[[varname]] <- list(
        "name" = name.value,
        "type" = "string",
        "suppress_reporting" = TRUE,
        "canonical_name" = ifelse(label.value == "", NA, label.value)
      )
    } else if (type.value == "calculate") {
      ## creates a numeric apparently?
      res$variables[[varname]] <- list(
        "name" = name.value,
        "type" = "numeric",
        "canonical_name" = label.value
      )
    } else if (type.value %in% c("text", "datetime")) {
      res$variables[[varname]] <- list(
        "name" = name.value,
        "type" = "string",
        "suppress_reporting" = TRUE,
        "canonical_name" = label.value
      )
    } else if (type.value == "date") {
      res$variables[[varname]] <- list(
        "name" = name.value,
        "type" = "date",
        "canonical_name" = label.value
      )
    } else if (type.value %in% c("integer", "decimal")) {
      res$variables[[varname]] <- list(
        "name" = name.value,
        "type" = "numeric",
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
      for (level.num in seq_len(length(names(choice.list$models[[shared.model]]$levels)))) {
        sub.varname <- paste(varname,
          choice.list$models[[shared.model]]$levels[[level.num]][["alternate_patterns"]][1],
          sep = "_"
        )
        res$variables[[sub.varname]] <- list(
          "name" = paste(name.value,
            choice.list$models[[shared.model]]$levels[[level.num]][["alternate_patterns"]][1],
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
    } else {
      warning("unrecognized CTO type flag detected: \"", type.value, "\"")
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
        stringr::str_pad(length(out.yaml$variables) + 1, 5, pad = "0"),
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
  while (TRUE) {
    i <- i + 1
    type.value <- survey[i, "type"]
    if (type.value == "end repeat") break
    name.value <- survey[i, "name"]
    label.value <- survey[i, "label"]
    cur.varname <- paste(dataset.tag,
      stringr::str_pad(length(out.yaml$variables) + length(repeat.variables$variables) + 1,
        5,
        pad = "0"
      ),
      sep = ""
    )
    if (is.null(query.varname)) {
      query.varname <- name.value
    }
    variable.data <- build.variable.data(type.value, name.value, label.value, choice.list, cur.varname)
    for (variable in names(variable.data$variables)) {
      repeat.variables$variables[[variable]] <- variable.data$variables[[variable]]
    }
  }
  ## initial logic here was not complex enough to handle multiple response variables
  ## embedded in a repeat block. this now pulls all instances of initial variable
  ## of a repeat block, does some string reduction, and then parses the repeats from there
  repeat.obs <- responses[stringr::str_detect(responses, paste("^", query.varname, "_", sep = ""))]
  n.repeats <- as.integer(stringr::str_replace(repeat.obs[length(repeat.obs)], "^.*_([0-9]+)$", "\\1"))
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
  list(
    out.yaml = out.yaml,
    i = i
  )
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
#' @export
parse.surveycto <- function(in.form.filename, in.response.filename, dataset.tag, out.yaml.filename, out.shared.models) {
  survey <- openxlsx::read.xlsx(in.form.filename, sheet = "survey")
  stopifnot(c("type", "name", "label") %in% colnames(survey))
  choices <- openxlsx::read.xlsx(in.form.filename, sheet = "choices")
  choice.list <- populate.choices(choices)
  responses <- colnames(read.table(in.response.filename,
    sep = ",", comment.char = "",
    quote = "\"", header = TRUE, nrows = 1
  ))
  out.yaml <- create.config(dataset.tag)
  i <- 1
  while (i <= nrow(survey)) {
    type.value <- survey[i, "type"]
    name.value <- survey[i, "name"]
    label.value <- survey[i, "label"]
    cur.varname <- paste(dataset.tag, stringr::str_pad(length(out.yaml$variables) + 1, 5, pad = "0"), sep = "")

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
    if (length(excess.headers.in.prediction) == 0 &
      length(headers.not.present) == 0) {
      print("output variables are correct but in the wrong order")
      aligned <- cbind(responses, output.predicted.headers)
      print(aligned[aligned[, 1] != aligned[, 2], ])
    }
    stop("output variable prediction has failed")
  }
  yaml::write_yaml(out.yaml, out.yaml.filename, fileEncoding = "UTF-8")
  yaml::write_yaml(choice.list, out.shared.models, fileEncoding = "UTF-8")
}
