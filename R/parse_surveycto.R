#' @title
#' Apply standard replacements for certain deprecated terms
#'
#' @description
#' Due to the integration of legacy form files, certain terms
#' that are inappropriate for use in actual datasets are
#' still present. This function is designed to intercept
#' them and replace them with appropriate terms.
#'
#' @details
#' Note that the use of this function creates certain
#' types of possible discrepancies if automatically
#' configured files are partially merged with manually
#' generated files. As this part of the secondary helper
#' function chain around parse.surveycto, we live with
#' the possibility of these discrepancies, as the output
#' of parse.surveycto is supposed to be manually reviewed
#' before use regardless.
#'
#' @param vec Character vector of values that require
#' scanning for possible replacement terms.
#' @return Character vector version of input with
#' replacements applied.
#' @usage NULL
apply.replacements <- function(vec) {
  res <- gsub("tribes", "ancestries", vec, ignore.case = TRUE)
  res <- gsub("tribe", "ancestry", res, ignore.case = TRUE)
  res
}

#' @title
#' Determine from a set of variables what the last
#' variable number was and return as integer
#'
#' @description
#' As part of the parse.surveycto processing chain,
#' a new variable identifier must be selected to
#' add to an existing set of configured variables.
#' This function takes the current set of variable
#' names and returns the maximum integer component
#' of the tag, such that a new variable name can
#' be constructed.
#'
#' @details
#' The entire parse.surveycto processing chain assumes
#' that the configuration files use our standard
#' naming convention of "^\[A-Za-z\]+\[0-9\]{5}" (followed
#' by possible annotations for repeat variables). There
#' is no reason in the processing of create.phenotype.report
#' that such a convention be required, but since parse.surveycto
#' is constructing variable names automatically, a convention
#' is required. If a different convention is required, that's
#' totally ok, but the parse.surveycto helper function set
#' may not be suitable for your application.
#'
#' @param variables List of previously added variables,
#' with names of the format "TAG#####.*"
#' @return Numeric part of final named entry as integer.
#' @examples
#' var.data <- list(
#'   HW00001 = list(),
#'   HW00002 = list(),
#'   HW00003 = list(),
#'   HW00004_1 = list()
#' )
#' res <- process.phenotypes:::get.last.variable.number(var.data)
get.last.variable.number <- function(variables) {
  last.name <- names(variables)[length(names(variables))]
  last.num <- stringr::str_replace(last.name, "^.*(\\d{5}).*$", "\\1")
  as.integer(last.num)
}

#' @title
#' Render SurveyCTO configuration "choices" tab
#' as a shared_models style yaml list
#'
#' @description
#' The SurveyCTO "choices" tab represents shared categorical
#' variable data for defined variables in the current form.
#' This corresponds to the process.phenotypes shared models,
#' concept. For compatibility with create.phenotype.report,
#' the "choices" tab is extracted from the form and converted
#' into unordered categorical models.
#'
#' @details
#' SurveyCTO does not seem to enforce much in the way
#' of consistency in the three required columns in the
#' choices tab: "list_name", "value", and "label". We
#' have observed, among other things, instances of value
#' encodings for the same model mixing integers, floats,
#' and strings; multiple values corresponding to an
#' identical user-facing label; and all sorts of strange
#' issues with Excel numeric formatting of cells causing
#' various types of conversion issues. We probably
#' have not encountered or handled all possible idiosyncratic
#' behaviors in the form choice data, and will try to add support
#' as we run into those issues.
#'
#' SurveyCTO encodes multiple response variables as
#' one-hot style binary indicator variables. Due to
#' the kinds of level redundancy that the choices tab permits,
#' there are theoretically certain combinations of model
#' and multiple choice variable that could lead to ambiguous
#' output column names. We have not actually observed this
#' behavior in practice, and don't know what the output
#' csv result information would be in that situation.
#'
#' @param df Data frame of input choices tab from SurveyCTO spreadsheet
#' minimally with column headers "list_name", "value", and "label"
#' present.
#' @param survey.type Character vector of type column entries
#' from "survey" tab of CTO form configuration.
#' @param na.values Character vector of factor levels that should
#' be treated as NA.
#' @return List of choice information as shared_models
#' yaml list, with all variables configured as categoricals.
#' @examples
#' ## choices data require list_name, value, and label
#' choice.data <- data.frame(
#'   list_name = c("model1", "model1"),
#'   value = c("1", "2"),
#'   label = c("answer 1", "answer 2")
#' )
#' ## actual form variable specification is exclusively
#' ## required due to possible chaos in multiple responses
#' survey.type <- c("select_one model1")
#' ## allow user to inject NA encodings
#' na.values <- c("none", "absent", "did not respond")
#'
#' shared.models <- process.phenotypes:::populate.choices(
#'   choice.data,
#'   survey.type,
#'   na.values
#' )
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

#' @title
#' Initialize dataset yaml configuration
#' with basic default entries
#'
#' @description
#' process.phenotypes dataset configuration
#' files have a minimum required set of entries
#' for compliance with schema validation, regardless
#' of the actual variables in the dataset. This
#' populates a skeleton config with those entries
#' and sensible default values.
#'
#' @details
#' This function is an internal component of
#' the parse.surveycto processing chain. The
#' default values populated by this function
#' are sensible, but each individual project
#' will have its own requirements, so these
#' entries in particular will likely require
#' manual modification after parse.surveycto
#' has been successfully run.
#'
#' The variable "SubmissionDate" has been observed
#' in all SurveyCTO exports to date, and appears
#' to be required infrastructure regardless
#' of the actual study configuration. As such,
#' it is unconditionally injected into the
#' default configuration. If you end up
#' with an exported wide csv without a
#' "SubmissionDate" variable in the first
#' column, please post an issue about it.
#'
#' @param dataset.tag Character vector tag
#' for dataset.
#' @return List representing dataset yaml
#' with a single placeholder variable.
#' @examples
#' tag <- "HW"
#' dataset.yaml <- process.phenotypes:::create.config(tag)
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

#' @title
#' Populate variables list with multiple response one-hot variables
#'
#' @description
#' SurveyCTO multiple response variables are represented in wide csv
#' export files as a single column containing a whitespace-delimited
#' list of selected responses, followed by indicator variables,
#' one for each possible response level, in the order encountered
#' in the form choices tab, indicating whether the subject selected
#' that response level.
#'
#' @param choice.list Data frame of SurveyCTO form definition choices tab.
#' @param shared.model Character vector name of categorical model from xlsx
#' choices tab; corresponds to "list_name" column.
#' @param varname Character vector of harmonized variable name for
#' multiple response group.
#' @param name.value Character vector of name entry for multiple response
#' variable from SurveyCTO form definition survey tab.
#' @param label.value Character vector of label entry for multiple response
#' variable from SurveyCTO form definition survey tab.
#' @param res List containing partially constructed output yaml configuration,
#' with variables up to this multiple response variable already added.
#' @return List of input configuration data, with variables extended with new
#' entries for the multiple response one-hots.
#' @examples
#' choice.data <- data.frame(models = list(model1 = list(
#'   type = "categorical",
#'   levels = list(
#'     "1" = list(
#'       name = "answer 1",
#'       alternate_patterns = c("1")
#'     ),
#'     "2" = list(
#'       name = "answer 2",
#'       alternate_patterns = c("2")
#'     )
#'   )
#' )))
#' shared.model <- "model1"
#' varname <- "HW00002"
#' name.value <- "var50"
#' label.value <- "Description of var50"
#' res <- list(variables = list(
#'   HW00001 = list(),
#'   HW00002 = list()
#' ))
#' output.config <- process.phenotypes:::handle.multiple.levels(
#'   choice.data,
#'   shared.model,
#'   varname,
#'   name.value,
#'   label.value,
#'   res
#' )
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

#' @title
#' Construct variable annotation according to type information
#' from a SurveyCTO configuration row
#'
#' @description
#' Given a variable configuration entry from a SurveyCTO form
#' definition, this function attempts to construct a corresponding
#' basic entry in a process.phenotypes-style variable configuration
#' entry. Basic functionality for each type is provided (see Details);
#' however, it is expected that manual inspection and modifiction will
#' be necessary in many cases.
#'
#' @details
#' The constructed variable entry is designed to contain what we
#' find to be sensible default values for each output type. The
#' behaviors by type are as follows:
#'
#' - SurveyCTO types "start", "end", "deviceid", "subscriberid",
#'   "simserial", "phonenumber", "username", "caseid", "image",
#'   "text", "datetime": all are encoded as process.phenotypes
#'   "string" types with NA canonical_name and suppressed reporting.
#'   There is an argument for encoding the "datetime" type as
#'   "date"; however, in the vast majority of instances we've
#'   observed, datetime variables are largely there for recordkeeping
#'   and are not actually expected to be parsed for year. In cases
#'   where such parsing is required, a derived variable may be
#'   more appropriate.
#' - SurveyCTO type "calculate": these variables have contextually
#'   different types, and for standardization purposes are encoded
#'   by default as process.phenotypes "string" type. During manual
#'   review, you will likely prefer to override this setting for
#'   calculated variables that are, for example, truly numeric.
#' - SurveyCTO type "date": process.phenotypes type "date", which
#'   will cause them to be parsed into numeric representations of
#'   their year component. If this behavior is not desired, be sure
#'   to override this setting during manual review.
#' - SurveyCTO types "integer" and "decimal": process.phenotypes
#'   type "numeric".
#' - SurveyCTO types: "select_one" and "select_multiple":
#'   process.phenotypes type "categorical" with "shared_model"
#'   the corresponding group in the SurveyCTO form choices tab.
#'   It may be appropriate to override some of these values
#'   to instead be "ordinal" type during manual review.
#'
#' We have not encountered any other types _per se_ during our
#' review of SurveyCTO forms to date. It is likely that there are
#' some we do not currently support, or that will have been added
#' after this function was written. Certain types of infrastructure
#' entries in the form definition (e.g. notes, begin/end statements
#' for repeats and groups, and blank lines) are seamlessly processed.
#' Anything else encountered in the form file should cause
#' build.variable.data to emit an `unrecognized CTO type flag detected`
#' warning. If you see such warnings, please inspect the type value
#' emitted in the warning. In most cases, the warnings are actually
#' harmless, the offending row is skipped, and the processing chain
#' continues without issue. However, if it looks like a meaningful
#' type entry has been skipped, it will probably break the parse.surveycto
#' logic chain. In that case, please post an issue so we can extend
#' support to the implicated type.
#'
#' @param type.value Character vector of entry from SurveyCTO form definition "type" column.
#' @param name.value Character vector of entry from SurveyCTO form definition "name" column.
#' @param label.value Character vector of entry from SurveyCTO form definition "label" column.
#' @param choice.list List of shared model data for questionnaire, created from form definition
#' "choices" tab.
#' @param varname Character vector of constructed name of variable (e.g. HW00001).
#' @return List with contents of variable configuration block for this variable, under
#' variable.summary$variables\[\[varname\]\].
#' @examples
#' type.value <- "text"
#' name.value <- "var1"
#' label.value <- "write something cool here"
#' choice.data <- data.frame(models = list(model1 = list(
#'   type = "categorical",
#'   levels = list(
#'     "1" = list(
#'       name = "answer 1",
#'       alternate_patterns = c("1")
#'     ),
#'     "2" = list(
#'       name = "answer 2",
#'       alternate_patterns = c("2")
#'     )
#'   )
#' )))
#' varname <- "HW00001"
#' var.data <- process.phenotypes:::build.variable.data(
#'   type.value,
#'   name.value,
#'   label.value,
#'   choice.data,
#'   varname
#' )
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

#' @title
#' Add fixed trailing annotations to questionnaire configuration
#'
#' @description
#' Analogous to the "SubmissionDate" starting variable, survey
#' responses have fixed output metadata columns
#' appended. This function scans the observed csv data and
#' injects variable configuration data for whichever of the metadata
#' columns happens to be present in the SurveyCTO export.
#'
#' @details
#' The inclusion of these columns seems to be based on interactions
#' with the SurveyCTO system that are not reflected in the form
#' configuration. For example, SurveyCTO supports a review process
#' that can inject review quality annotations into the export,
#' and this system exists entirely in parallel to the actual form
#' definition.
#'
#' The actual metadata columns included seem to be pulled from
#' a somewhat amorpheous pool of options, and we almost certainly
#' have not encountered all possible values. More may need to be
#' added to the pool at some time; if you encounter any, which
#' will be noted as extra data columns at the very end of the export
#' that parse.surveycto seems unable to resolve and that were not
#' present in the input form spreadsheet, please consider posting
#' an issue describing the name and contents of the offending
#' column(s).
#'
#' @param out.yaml List containing partially constructed variable summary
#' information for questionnaire.
#' @param dataset.tag Character vector of tag for this dataset
#' @param responses Character vector of column names from actual
#' exported wide format csv survey data.
#' @return List of input yaml configuration with additional
#' variable information appended.
#' @examples
#' out.yaml <- list(variables = list(HW00001 = list(
#'   name = "SubmissionDate",
#'   type = "string"
#' )))
#' dataset.tag <- "HW"
#' responses <- c("SubmissionDate", "instanceID", "instanceName")
#' extended.yaml <- process.phenotypes:::add.trailing.metadata(
#'   out.yaml,
#'   dataset.tag,
#'   responses
#' )
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

#' @title
#' Create variable annotations for repeat variable blocks
#'
#' @description
#' SurveyCTO form definitions support what we term "repeat blocks":
#' delimited sets of variables that are repeatedly prompted to users
#' until the user passes and moves on to the next set of variables.
#' This function determines how many responses were maximally observed
#' in the current SurveyCTO wide csv export, and creates variable configuration
#' entries as necessary.
#'
#' @details
#' These variables are fundamentally different than what was originally
#' envisioned for process.phenotypes, and so there is substantial logic
#' involved in emitting compatible configuration data for them.
#'
#' The ordering of repeat blocks is as follows: if the form definition
#' contains a `begin repeat` statement, a definition for variables `var`
#' and `var2`, and an `end repeat` statement, the output csv will contain,
#' for however many repeat instances were maximally present in the response
#' set, `var1_1`, `var2_1`, `var1_2`, `var2_2`, etc. Configuration blocks
#' are thus added to the dataset configuration file to match this ordering.
#' The standard name convention `TAG#####` is extended as `TAG#####_1`, etc.
#' The hope with this naming is that, with fixed width numeric tags and delimiters,
#' it should be possible to easily query the entire set of repeat responses to
#' a given question as something like `data[, grepl("TAG#####_", colnames(data))]`.
#'
#' We don't have a good explanation for this at this point, but seemingly
#' sometimes repeat variables are led by a single variable named "{repeatname}_count",
#' based on the name of the repeat block in the SurveyCTO configuration form data.
#' We don't know exactly what in the form data predicts the presence or absence
#' of this column. As such, this function scans the response data for such a column
#' in the predicted location and adds a configuration entry if it is detected.
#'
#' Repeat blocks may be entirely absent from response data, when the repeat
#' variables are optional and no one has yet opted to respond even one time.
#' This ultimately means that the run of `parse.surveycto` will correctly
#' exclude that repeat block from configuration, but future exports for the
#' questionnaire from SurveyCTO will gain a repeat block that was not initially
#' present. This is, as they say, a real bummer. The variable(s) can be added
#' to the existing configuration file, though using the variable name nomenclature
#' assumed by the parse.surveycto processing chain, it ruins the aesthetics
#' by injecting higher numbered variable tags into the middle of the variable
#' configuration block.
#'
#' Manually updating repeat blocks to reflect new exports from SurveyCTO
#' is tedious and error-prone. If you have to do it, please look into
#' the utility function expand.surveycto.config and see if it can help you.
#'
#' @param out.yaml List containing partially constructed variable configuration
#' data.
#' @param cur.varname Character vector of constructed name of repeat start variable.
#' @param name.value Character vector of name column entry for repeat start variable.
#' @param label.value Character vector of label column entry for repeat start variable.
#' @param survey Data frame containing survey tab from SurveyCTO configuration form file.
#' @param dataset.tag Character vector of tag for current dataset.
#' @param responses Character vector of column names of completed questionnaire wide format csv.
#' @param choice.list List of configured shared model data for form definition, based
#' on the form file choices tab.
#' @param i Integer index of repeat start variable in survey configuration table.
#' @return List; first entry `out.yaml` a modified version of input yaml configuration
#' with repeat variable block data added; second entry `i` incremented global counter
#' reflecting the number of variables added in the repeat.
#' @seealso expand.surveycto.config
#' @examples
#' config.data <- list(variables = list(HW00001 = list(
#'   name = "SubmissionDate",
#'   type = "string"
#' )))
#' cur.varname <- "HW00002"
#' name.value <- "var2"
#' label.value <- "description of var2"
#' survey <- data.frame(
#'   type = c("begin repeat", "text", "text", "end repeat"),
#'   name = c("repeat1", "var2", "var3", ""),
#'   label = c("", "description of var2", "description of var3", "")
#' )
#' dataset.tag <- "HW"
#' responses <- c("SubmissionDate", "var2_1", "var3_1")
#' choice.data <- data.frame(models = list(model1 = list(
#'   type = "categorical",
#'   levels = list(
#'     "1" = list(
#'       name = "answer 1",
#'       alternate_patterns = c("1")
#'     ),
#'     "2" = list(
#'       name = "answer 2",
#'       alternate_patterns = c("2")
#'     )
#'   )
#' )))
#' i <- 2
#' results <- process.phenotypes:::handle.repeat.variables(
#'   config.data, cur.varname, name.value,
#'   label.value, survey, dataset.tag,
#'   responses, choice.data, i
#' )
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

#' @title
#' Add flags for subject ID and age variables
#'
#' @description
#' process.phenotypes dataset configuration files are required to contain
#' a variable flagged as containing subject IDs, and a variable containing
#' subject age at consent. This function attempts to automatically tag
#' the required variables based on user-specified expected name patterns.
#'
#' @details
#' Ideally, input form data would contain regularly named variables, in such
#' a way that you could pretty easily predict the variable names involved in the
#' subject ID and age. However, this has rarely panned out well in our experience.
#' Additional specification of aliases has been required with every additional
#' dataset we've processed.
#'
#' This logic could be made substantially more complex, and if there's need that
#' will be patched later.
#'
#' @param out.yaml List containing constructed variable configuration data.
#' @param subject.id.name Character vector of expected name of subject ID variable.
#' @param age.name Character vector of expected name of age variable.
#' @return List of input configuration data with flags added to appropriate variables.
#' @examples
#' config.data <- list(variables = list(
#'   HW00001 = list(name = "subject_id"),
#'   HW00002 = list(name = "subject_age")
#' ))
#' result <- process.phenotypes:::flag.required.variables(
#'   config.data,
#'   "subject_id",
#'   "subject_age"
#' )
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

#' @title
#' Convert SurveyCTO configuration form into yaml configuration for
#' use with process.phenotypes
#'
#' @description
#' SurveyCTO configuration forms are approximately equivalent to
#' the configuration data used by process.phenotypes::create.phenotype.report.
#' However, the process.phenotypes configuration accepts a wide array
#' of cleaning and derivation options that need to be separately configured.
#' The process of converting SurveyCTO form data to process.phenotypes
#' configuration files is tedious and error-prone, so this utility function
#' attempts to create a baseline configuration set from a form file,
#' with reasonable default settings based on set conversions from
#' SurveyCTO variable types.
#'
#' @details
#' process.phenotypes was initially developed to support truly unstructured
#' data sources, in which there were no guarantees whatsoever about data
#' consistency in any dimension. Managed questionnaire data from a system
#' like SurveyCTO are much preferable, but many of the features of
#' process.phenotypes are still desirable: reproducible and transparent
#' data processing, derived variable calculation, etc. This function is
#' designed to facilitate the process of generating configuration files
#' for process.phenotypes from SurveyCTO form specifications.
#'
#' process.phenotypes does not directly accept SurveyCTO forms.
#' This library is designed for quality control and derived variable
#' generation, and furthermore emphasizes transparency and reproducibility,
#' and in order to achieve these things, a record of data configuration
#' that reflects the current status of the data is required.
#' Additionally, SurveyCTO supports kinds of variables that expand in size
#' depending on the number of responses observed in the data. That's
#' not necessarily a problem _per se_, but after a certain number
#' of responses to a given question, we find that the behavior of
#' actual observed responses tends to change, such that the desired
#' treatment of such variables differs spending on which repeat observation
#' is being considered. As such, we keep configuration separate for
#' each variable, and furthermore provide a utility function
#' `expand.surveycto.config` that makes the process of expanding an
#' existing configuration file to adapt to new repeat blocks much
#' more straightforward.
#'
#' For context, this package is an important aspect of our reproducible
#' data flow, but it does not operate in isolation. We have developed
#' this package in parallel with a Snakemake workflow for handling
#' our data streams. In that workflow, multiple calls to various functions
#' in this package are automated in such a way that the functions communicate
#' seamlessly with one another, and so that upstream changes in the various
#' data sources used by process.phenotypes reliably trigger the correct
#' downstream updates. We highly recommend this kind of process to other
#' interested users, as a way of further streamlining the very
#' finicky process of iterative data cleaning.
#'
#' The development of this function has been fairly organic, as we've
#' expanded its supported featureset based on what we've observed
#' in live form definitions. In light of this fact, we expect that there
#' are additional form features that this conversion function does
#' not yet support. The function is capable of detecting most errors
#' in its own process by comparing its predicted variable set against
#' the headers of the provided wide format csv data. It will report out
#' as an error if it finds headers that are any of:
#'
#' - predicted by this function but absent in the csv;
#' - present in the csv but not predicted by this function; or
#' - both predicted and observed but out of order.
#'
#' In any of those situations, it abstains from emitting an output
#' configuration fileset. The two most likely causes of these errors are:
#'
#' - as mentioned above, this function may not support a configured
#' feature; or
#' - the csv file provided to the function does not match the form version
#' provided to the function.
#'
#' If you're quite certain that the form and csv file match each other,
#' then please consider posting an issue describing the observed error
#' and ideally providing an example of the offending form definition.
#'
#' @param in.form.filename Character vector of name of SurveyCTO Excel xlsx
#' form definition corresponding to the current csv data export.
#' @param in.response.filename Character vector of name of wide format
#' csv data export from SurveyCTO.
#' @param dataset.tag Character vector of tag (e.g. 'HW') for this dataset,
#' which is used as prefix for automated variable names.
#' @param out.yaml.filename Character vector of destination filename
#' for dataset-specific variable configuration file.
#' @param out.shared.models Character vector of destination filename
#' for shared categorical model configuration file for this questionnaire.
#' @param subject.id.name Character vector of name of variable containing
#' subject ID for each row of the input wide format csv data.
#' @param age.name Character vector of name of variable containing subject age
#' for each row of the input wide format csv data.
#' @param na.values Character vector of factor levels that should be
#' treated as NA when creating configuration data. This can be empty if desired.
#' @return NULL
#' @export
#' @seealso expand.surveycto.config
#' @examples
#' csv.filename <- system.file("examples", "parse_surveycto_example.csv",
#'   package = "process.phenotypes"
#' )
#' xlsx.filename <- system.file("examples", "parse_surveycto_example.xlsx",
#'   package = "process.phenotypes"
#' )
#' dataset.tag <- "HW"
#' out.dataset.filename <- tempfile("psc_out_dataset", fileext = ".yaml")
#' out.shared.filename <- tempfile("psc_out_shared", fileext = ".yaml")
#' parse.surveycto(
#'   xlsx.filename,
#'   csv.filename,
#'   dataset.tag,
#'   out.dataset.filename,
#'   out.shared.filename,
#'   "subjectid_1",
#'   "subjectage"
#' )
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



#' @title
#' Generate configuration blocks for newly appeared SurveyCTO repeat variables
#'
#' @description
#' SurveyCTO repeat variables have a variety of idiosyncratic behaviors
#' that complicate their compatibility with process.phenotypes. Repeat
#' variables that have no observed responses will in some cases be entirely
#' suppressed from the exported wide format csv data. That's fine, until
#' a later data export features responses to those questions, and the remaining
#' variables are shifted. This function attempts to inject configuration
#' blocks for newly observed repeat variables into an existing variable
#' configuration.
#'
#' @details
#' This function is an internal function called within expand.surveycto.config.
#' This function should not be directly called by the user.
#'
#' @param varname Character vector of formatted variable name for predicted
#' variable insertion point in existing configuration.
#' @param regenerated.yaml List of loaded dataset yaml corresponding
#' to current actual wide csv export from SurveyCTO. Expected to
#' be the loaded output of parse.surveycto.
#' @param max.existing.number Numeric representing maximum variable number
#' in an existing variable configuration, expecting variable names of the
#' format "HW#####".
#' @param res.variables List of accumulated output variable
#' configuration with repeats added or extended as needed.
#' @param missing.colname Character vector encountered column name
#' that is missing from current configuration. Expected to be
#' correctly predicted and present in parse.surveycto configuration
#' based on the current form definition and csv export.
#' @return List; first entry `res.variables` an updated version of input parameter
#' with the new resolved variable configuration entry added; second entry
#' `max.existing.number` updated version of input parameter incremented
#' to reflect variable addition.
#' @usage NULL
#' @seealso expand.surveycto.config
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


#' @title
#' Generate configuration blocks for extensions of existing SurveyCTO
#' repeat variables
#'
#' @description
#' SurveyCTO repeat variables have a variety of idiosyncratic behaviors
#' that complicate their compatibility with process.phenotypes. Repeat
#' variables that have observed responses in a particular wide format csv
#' export may gain additional repeat observations if recently collected
#' responses contain more than the maximum number of existing repeats.
#' This function attempts to extend an existing repeat block to match
#' the observed maximum number of repeat instances in a new wide format
#' csv export.
#'
#' @details
#' This function is an internal function called within expand.surveycto.config.
#' This function should not be directly called by the user.
#'
#' @param var.prefix Character vector shared common prefix of the dataset
#' yaml configuration for this repeat variable. So for example,
#' if the existing repeat block was "HW00001_1", "HW00001_2", "HW00001_3",
#' this would be "HW00001_".
#' @param trailing.num Numeric indicating instance of the repeat observed
#' in the new dataset but absent from original configuration.
#' @param res.variables List of accumulated output variable
#' configuration with repeats added or extended as needed.
#' @param missing.colname Character vector encountered column name
#' that is missing from current configuration. Expected to be
#' correctly predicted and present in parse.surveycto configuration
#' based on the current form definition and csv export.
#' @return List representing updated version of input parameter 'res.variables'
#' with the new resolved variable configuration entry added.
#' @usage NULL
#' @seealso expand.surveycto.config
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


#' @title
#' Wrap a call to parse.surveycto and return its loaded dataset configuration
#'
#' @description
#' Repeat expansion of SurveyCTO configuration data needs to generate
#' variable configuration blocks. That logic is already handled by
#' `parse.surveycto`. To avoid duplicating code, this function
#' wraps a call to `parse.surveycto` and returns that predicted
#' dataset yaml configuration as a list, such that the general
#' settings for variables can be used and the IDs can just be updated.
#'
#' @details
#' This function is an internal function called within expand.surveycto.config.
#' This function should not be directly called by the user.
#'
#' @param existing.yaml List of existing dataset configuration
#' loaded with read_yaml. Note that the entire configuration
#' need not be correctly specified, but the variables containing
#' the mandatory 'subject_id' and 'subject_age' tags should
#' be correctly specified and ordered.
#' @param form.definition.filename Character vector of name of
#' SurveyCTO xlsx form definition.
#' @param new.data.filename Character vetor of name of wide csv
#' export from SurveyCTO.
#' @param intermediate.dataset.yaml Character vector of name of
#' output dataset yaml from parse.surveycto.
#' @param intermediate.shared.models Character vector of name
#' of output shared models yaml from parse.surveycto.
#' @return Dataset configuration output of `parse.surveycto`,
#' loaded and returned as a list.
#' @usage NULL
#' @seealso expand.surveycto.config
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

#' @title
#' Utility function to update an existing configuration
#' file from parse.surveycto, given new SurveyCTO export data
#'
#' @description
#' Certain types of output columns in exported SurveyCTO
#' datasets are unpredictable, in the sense that, with a constant
#' form definition, the columns may or may not be present, and can
#' only be predicted with access to the export itself. This creates
#' challenges when trying to generate a variable specification for
#' this package from which one can predictably use variable
#' references, for example with the creation of derived variables,
#' or in order to generate consensus variables between datasets.
#'
#' @details
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
#' @param existing.yaml.filename Character vector of name of
#' dataset yaml (potentially with custom configuration data)
#' from a run of parse.surveycto.
#' @param new.yaml.filename Character vector of name of output
#' dataset yaml.
#' @param new.cto.export.filename Character vector of name of
#' most recent wide-format csv output from SurveyCTO. This
#' iput has been tested with API output, though it should
#' work with web-based export as well.
#' @param form.definition.filename Character vector of name of
#' SurveyCTO form xlsx file that governs the format of the
#' current csv export.
#' @param new.processed.export.filename Character vector of
#' name of csv output from SurveyCTO with any postprocessed
#' modifications (see Details).
#' @param intermediate.dataset.yaml Character vector of name
#' of dataset yaml the function generates from parse.surveycto
#' for comparison purposes; if not specified, a generic tempfile.
#' @param intermediate.shared.models Character vector of name of
#' shared models yaml the file generates from parse.surveycto for
#' comparison purposes; if not specified, a generic tempfile.
#' @param quote Character vector used to quote string tokens in expanded SurveyCTO export
#' and passed to read.table. Defaults to NULL. This parameter is exposed for
#' greater compability with unpredictable input formats.
#' @param sep Character vector used to delimit input fields in expanded SurveyCTO export
#' and passed to read.table. Defaults to tab (\\t). This parameter is exposed for
#' greater compatibility with unpredictable input formats.
#' @return NULL
#' @export
#' @examples
#' existing.form <- system.file("examples", "parse_surveycto_example.xlsx",
#'   package = "process.phenotypes"
#' )
#' existing.csv <- system.file("examples", "parse_surveycto_example.csv",
#'   package = "process.phenotypes"
#' )
#' ## first, use parse.surveycto to emit a configuration file set for the initial csv export
#' initial.dataset.yaml <- tempfile("esc_dataset", fileext = ".yaml")
#' initial.shared.models <- tempfile("esc_shared_models", fileext = ".yaml")
#' parse.surveycto(existing.form, existing.csv,
#'   "HW",
#'   initial.dataset.yaml, initial.shared.models,
#'   subject.id.name = "subjectid_1",
#'   age.name = "subjectage",
#' )
#'
#' ## after some time, you end up with a csv with a new repeat entry
#' expanded.csv <- tempfile("esc_expanded_csv", fileext = ".csv")
#' data <- read.table(existing.csv,
#'   header = TRUE, sep = ",",
#'   quote = "\"", comment.char = "", stringsAsFactors = FALSE
#' )
#' data <- cbind(
#'   data[, 1:11],
#'   sample(letters, nrow(data), replace = TRUE),
#'   data[, seq(12, ncol(data))]
#' )
#' colnames(data)[12] <- "subjectid_2"
#' write.table(data, expanded.csv,
#'   row.names = FALSE,
#'   col.names = TRUE, quote = TRUE, sep = ","
#' )
#'
#' ## expand the dataset configuration file to handle this new repeat entry
#' expanded.yaml <- tempfile("esc_expanded_yaml", fileext = ".yaml")
#' expand.surveycto.config(
#'   initial.dataset.yaml,
#'   expanded.yaml,
#'   expanded.csv,
#'   existing.form,
#'   quote = "\"",
#'   sep = ","
#' )
expand.surveycto.config <- function(existing.yaml.filename,
                                    new.yaml.filename,
                                    new.cto.export.filename,
                                    form.definition.filename,
                                    new.processed.export.filename = new.cto.export.filename,
                                    intermediate.dataset.yaml = tempfile("expand.surveycto.dataset"),
                                    intermediate.shared.models = tempfile("expand.surveycto.shared_models"),
                                    quote = "\"",
                                    sep = "\t") {
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
    stringsAsFactors = FALSE, sep = sep,
    comment.char = "", quote = quote,
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
  existing.yaml$variables <- res.variables
  yaml::write_yaml(existing.yaml, new.yaml.filename)
}
