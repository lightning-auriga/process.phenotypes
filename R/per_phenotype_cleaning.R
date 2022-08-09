#' @title
#' Apply type constraints to each variable from yaml config
#'
#' @description
#' Each yaml configuration block should have a "type" entry
#' denoting what data storage type is expected for this variable.
#' This function dispatches the appropriate handlers for these
#' methods, specifically triggering type-specific cleaning in some
#' cases before casting the values.
#'
#' If for any reason you don't like the type conversion behaviors
#' in this package, set the variable type to "string" and then
#' implement the processing you desire as a derived variable.
#'
#' @details
#' This function is a pure internal, and should never be
#' called outside of the package.
#'
#' @param phenotype.data Data frame of loaded phenotype data with
#' standardized headers. All columns should be character vectors in
#' input, as this is the method that casts them out of character vectors.
#' @param variable.summary List of per-column summary information
#' and parameters from dataset configuration.
#' @return List, with first entry 'phenotype.data' a data frame of modified
#' phenotype input data with conversions applied as appropriate;
#' and second entry 'variable.summary' a list of modified input configuration
#' with summary information injected from certain handlers.
#' @seealso load.configuration
#' @keywords phenotypes yaml
#' @usage NULL
apply.type.conversions <- function(phenotype.data, variable.summary) {
  stopifnot(ncol(phenotype.data) == length(variable.summary$variables))
  for (i in seq_len(length(variable.summary$variables))) {
    if (is.null(variable.summary$variables[[i]]$params$type)) {
      warning(paste("variable \"", names(variable.summary$variables)[i], "\" has null type specification", sep = ""))
    }
    target.type <- NULL
    if (!is.null(variable.summary$variables[[i]]$params$type)) {
      target.type <- tolower(variable.summary$variables[[i]]$params$type)
    }
    possible.types <- c(
      "string",
      "categorical",
      "categorical_to_numeric",
      "ordinal",
      "binary",
      "numeric",
      "blood_pressure",
      "blood pressure",
      "bp",
      "date"
    )
    if (is.null(target.type)) {
      ## null
      next
    }
    if (!(target.type %in% possible.types)) {
      stop(
        "In apply.type.conversions, unrecognized type for variable \"",
        colnames(phenotype.data)[i], "\": \"",
        target.type, "\""
      )
    } else if (grepl("string", target.type, ignore.case = TRUE)) {
      ## string
      next
    } else {
      na.before.conversion <- is.na(phenotype.data[, i])
      result.list <- convert.type(
        phenotype.data[, i], variable.summary$variables[[i]], target.type
      )
      phenotype.data[, i] <- result.list$phenotype.data
      variable.summary$variables[[i]] <- result.list$variable.summary
      na.after.conversion <- is.na(phenotype.data[, i])
      variable.summary$variables[[i]]$subjects.wrong.type <-
        phenotype.data[
          na.after.conversion & !na.before.conversion,
          find.subject.id.index(variable.summary)
        ]
    }
  }
  list(
    phenotype.data = phenotype.data,
    variable.summary = variable.summary
  )
}

#' @title
#' Coordinate different function calls for type conversion
#'
#' @description
#' Each yaml configuration block should have a "type" entry
#' denoting what data storage type is expected for this variable.
#' This function dispatches the appropriate handlers for these
#' methods, specifically triggering type-specific cleaning in some
#' cases before casting the values.
#'
#' If for any reason you don't like the type conversion behaviors
#' in this package, set the variable type to "string" and then
#' implement the processing you desire as a derived variable.
#'
#' @details
#' This function is a pure internal, and should never be
#' called outside of the package.
#'
#' @param vec character vector, input phenotype content
#' @param var.summary list, variable summary entry for this particular variable
#' @param target.type character vector, denotes the expected type for each variable;
#' read from the yaml config
#' @return list, 'phenotype.data' contains converted phenotype information,
#' 'variable.summary' contains input variable summary.
#' @seealso apply.type.conversions
#' @keywords phenotypes yaml
#' @usage NULL
convert.type <- function(vec, var.summary, target.type) {
  result.list <- NULL
  if (grepl("^categorical$|^ordinal$|^binary$|^categorical_to_numeric$", target.type, ignore.case = TRUE)) {
    ## categorical or ordinal or binary
    result.list <- reformat.factor(vec, var.summary)
    if (grepl("ordinal", target.type, ignore.case = TRUE)) {
      result.list$phenotype.data <- ordered(result.list$phenotype.data,
        levels = levels(result.list$phenotype.data)
      )
    }
    if (grepl("categorical_to_numeric", target.type, ignore.case = TRUE)) {
      ## new: allow categoricals that have pure numeric labels
      ## to be converted to numerics with their labels as values.
      ## note that this will seemingly suppress categorical conversion
      ## errors and skip some numeric cleaning; so in theory, this
      ## should only be performed on variables that have already
      ## been sufficiently sanitized such that this is not a concern.
      result.list$phenotype.data <- as.vector(result.list$phenotype.data, mode = "character")
      result.list$variable.summary$params$type <- "numeric"
      result.list <- reformat.numerics(
        result.list$phenotype.data,
        result.list$variable.summary
      )
    }
  } else if (grepl("numeric", target.type, ignore.case = TRUE)) {
    ## numeric
    result.list <- reformat.numerics(vec, var.summary)
  } else if (grepl("^blood[_ ]?pressure$|^bp$", target.type, ignore.case = TRUE)) {
    ## blood pressure
    result.list <- reformat.blood.pressure(vec, var.summary)
  } else if (grepl("^date$", target.type, ignore.case = TRUE)) {
    ## date
    result.list <- parse.date(vec, var.summary)
  } else {
    stop("invalid switch condition reached in convert.type")
  }
  list(
    phenotype.data = result.list$phenotype.data,
    variable.summary = result.list$variable.summary
  )
}

#' @title
#' Apply range bounds to each variable as defined in yaml config
#'
#' @description
#' Numeric variables can optionally have min/max bounds defined in
#' the config (under "bounds") - apply them here by replacing values
#' outside the bounds with NA.
#'
#' @details
#' Currently, numeric bounds are only supported on numeric
#' and date input configuration types. Other types may be
#' added to the list depending on future functionality.
#'
#' The bounds understood at this time are:
#' - min: set values below this threshold to NA
#' - max: set values above this threshold to NA
#' - sd: set values this many standard deviations _either above or below_
#' the mean to NA
#'
#' In case other bound conditions are desired, either file an issue,
#' or use a derived variable to apply the bounds based on arbitrary criteria.
#'
#' In addition to setting the offending values to NA, this function
#' injects summary counts describing the number of values removed for
#' each bound in turn into the configuration list. This information
#' is intended for use in the cleaning report.
#'
#' @param phenotype.data Data frame of loaded phenotype data with
#' standardized headers. All columns should be character vectors.
#' @param variable.summary List of per-column summary information
#' and parameters from dataset configuration.
#' @return List, with first entry 'phenotype.data' data frame a modified
#' version of phenotype input data bounds applied as appropriate;
#' and second entry 'variable.summary' list a modified version of input list
#' with information on entries outside the defined bounds.
#' @seealso load.configuration
#' @keywords phenotypes yaml
#' @examples
#' phenotype.data <- data.frame(
#'   HW00001 = c("A", "B", "C", "D"),
#'   HW00002 = 1:4
#' )
#' variable.summary <- list(variables = list(
#'   HW00001 = list(params = list(
#'     name = "subjid",
#'     type = "string",
#'     subject_id = TRUE
#'   )),
#'   HW00002 = list(params = list(
#'     name = "count",
#'     type = "numeric",
#'     bounds = list(min = 2)
#'   ))
#' ))
#' result <- process.phenotypes:::apply.bounds(phenotype.data, variable.summary)
apply.bounds <- function(phenotype.data, variable.summary) {
  stopifnot(ncol(phenotype.data) == length(variable.summary$variables))
  for (i in seq_len(length(variable.summary$variables))) {
    target.type <- variable.summary$variables[[i]]$params$type
    if (!is.null(target.type)) {
      if (grepl("^numeric$|^date$", target.type, ignore.case = TRUE)) {
        var.min <- variable.summary$variables[[i]]$params$bounds$min
        if (!is.null(var.min)) {
          var.min <- as.numeric(var.min)
          if (!is.na(var.min) && is.null(variable.summary$variables[[i]]$num.below.min)) {
            # count and apply min threshold
            num.min <- length(phenotype.data[phenotype.data[, i] < var.min &
              !is.na(phenotype.data[, i]), i])
            variable.summary$variables[[i]]$num.below.min <- num.min
            phenotype.data[phenotype.data[, i] < var.min &
              !is.na(phenotype.data[, i]), i] <- NA
          }
        }
        var.max <- variable.summary$variables[[i]]$params$bounds$max
        if (!is.null(var.max)) {
          var.max <- as.numeric(var.max)
          if (!is.na(var.max) && is.null(variable.summary$variables[[i]]$num.above.max)) {
            # count and apply max threshold
            num.max <- length(phenotype.data[phenotype.data[, i] > var.max &
              !is.na(phenotype.data[, i]), i])
            variable.summary$variables[[i]]$num.above.max <- num.max
            phenotype.data[phenotype.data[, i] > var.max &
              !is.na(phenotype.data[, i]), i] <- NA
          }
        }
        var.sd <- variable.summary$variables[[i]]$params$bounds$sd
        if (!is.null(var.sd) && is.null(variable.summary$variables[[i]]$num.beyond.sd)) {
          var.sd <- as.numeric(var.sd)
          stopifnot(var.sd >= 0)
          ## count and apply bidirectional standard deviation threshold
          sd.min.bound <- mean(phenotype.data[, i], na.rm = TRUE) - var.sd * sd(phenotype.data[, i], na.rm = TRUE)
          sd.max.bound <- mean(phenotype.data[, i], na.rm = TRUE) + var.sd * sd(phenotype.data[, i], na.rm = TRUE)
          num.sd <- length(which(phenotype.data[, i] < sd.min.bound | phenotype.data[, i] > sd.max.bound))
          variable.summary$variables[[i]]$num.beyond.sd <- num.sd
          phenotype.data[(phenotype.data[, i] < sd.min.bound |
            phenotype.data[, i] > sd.max.bound) &
            !is.na(phenotype.data[, i]), i] <- NA
        }
      }
    }
  }
  list(
    phenotype.data = phenotype.data,
    variable.summary = variable.summary
  )
}

#' @title
#' Find additional NA aliases in user config and apply them to
#' particular variables
#'
#' @description
#' User configuration can optionally contain an `na-values` sequence
#' containing entries that should be mapped to NA. This functionality
#' is only really reliable for factors and strings; numerics may
#' not match exactly as desired in some cases.
#'
#' @details
#' Well after `na-values` support was added to the package, there
#' was additionally need for an overall logical flag `suppress_output`,
#' which would prevent a variable's contents from being reported
#' in the output dataset(s). This function was coopted for this functionality;
#' as such, any variable flagged with `suppress_output: yes` in configuration
#' will have all of its values treated as NA aliases, and thus while the
#' variable will have a column in the output data, all observations will be NA.
#'
#' @param phenotype.data Data frame of loaded phenotype data with
#' standardized headers.
#' @param variable.summary List of per-column summary information
#' and parameters from dataset configuration.
#' @return Data frame containing input phenotype information, modified
#' such that all NA aliased values are in fact encoded as R NA.
#' @examples
#' phenotype.data <- data.frame(
#'   HW00001 = c("A", "B", "C", "D"),
#'   HW00002 = c("apple", "apple", "banana", "chicken"),
#'   HW00003 = 1:4
#' )
#' variable.summary <- list(variables = list(
#'   HW00001 = list(params = list(
#'     name = "subjid",
#'     type = "string",
#'     subject_id = TRUE
#'   )),
#'   HW00002 = list(params = list(
#'     name = "fruit",
#'     type = "string",
#'     "na-values" = c("chicken")
#'   )),
#'   HW00003 = list(params = list(
#'     name = "count",
#'     type = "numeric",
#'     suppress_output = TRUE
#'   ))
#' ))
#' result <- process.phenotypes:::convert.variable.specific.na(phenotype.data, variable.summary)
convert.variable.specific.na <- function(phenotype.data, variable.summary) {
  for (i in seq_len(length(variable.summary$variables))) {
    na.values <- variable.summary$variables[[i]]$params[["na-values"]]
    if (!is.null(na.values)) {
      phenotype.data[phenotype.data[, i] %in% as.character(na.values), i] <- NA
    }
    if (!is.null(variable.summary$variables[[i]]$params[["suppress_output"]])) {
      if (variable.summary$variables[[i]]$params[["suppress_output"]]) {
        phenotype.data[, i] <- NA
      }
    }
  }
  phenotype.data
}

#' @title
#' Exclude subjects for whom no ID has been recorded
#'
#' @description
#' This function excludes subjects where their ID is NA, as
#' such subjects cannot be meaningfully linked to relevant external
#' data sources, most notably consent calls.
#'
#' @details
#' This function was added during the original set of dataset processing
#' that led to the development of this package. More recent input datasets
#' have almost completely lacked this particular discrepancy. If any
#' subjects whatsoever are flagged as removed by this function, something
#' is likely seriously malformed in the input data.
#'
#' @param phenotype.data Data frame of loaded phenotype data with
#' standardized headers. All columns should be character vectors.
#' @param variable.summary List of per-column summary information
#' and parameters from dataset configuration.
#' @return List, with first entry 'phenotype.data' data frame of input
#' phenotypes with subjects without a subject ID excluded; and second
#' entry 'variable.summary' input configuration with information about
#' excluded subjects.
#' @examples
#' phenotype.data <- data.frame(
#'   HW00001 = c("A", "B", NA, "D"),
#'   HW00002 = 1:4
#' )
#' variable.summary <- list(variables = list(
#'   HW00001 = list(params = list(
#'     name = "subjid",
#'     type = "string",
#'     subject_id = TRUE
#'   )),
#'   HW00002 = list(params = list(
#'     name = "count",
#'     type = "numeric"
#'   ))
#' ))
#' result <- process.phenotypes:::exclude.by.missing.subject.id(phenotype.data, variable.summary)
exclude.by.missing.subject.id <- function(phenotype.data, variable.summary) {
  subject.id.index <- find.subject.id.index(variable.summary)
  na.subjects <- is.na(phenotype.data[, subject.id.index])
  variable.summary$na.subject.id.count <- length(which(na.subjects))
  phenotype.data <- phenotype.data[!na.subjects, ]
  list(
    phenotype.data = phenotype.data,
    variable.summary = variable.summary
  )
}

#' @title
#' Exclude subjects below a certain user-defined age threshold
#'
#' @description
#' This function will exclude subjects whose age, as defined with the
#' `subject_age: true` flag in the config, falls below the threshold.
#' This is most useful for excluding minors, for example, and may
#' additionally flag datasets that contain unexpected sets of subjects,
#' potentially indicating issues with corruption or provenance.
#'
#' @param phenotype.data Data frame of loaded phenotype data with
#' standardized headers. All columns should be character vectors.
#' @param variable.summary List of per-column summary information
#' and parameters from dataset configuration.
#' @return List, with first entry 'phenotype.data' data frame containing input phenotype
#' information with subjects below the age threshold excluded; and second
#' entry 'variable.summary' containing input configuration and information about excluded subjects.
#' @examples
#' phenotype.data <- data.frame(
#'   HW00001 = c("A", "B", "C", "D"),
#'   HW00002 = 18:21
#' )
#' variable.summary <- list(
#'   globals = list(min_age_for_inclusion = 20),
#'   variables = list(
#'     HW00001 = list(params = list(
#'       name = "subjid",
#'       type = "string",
#'       subject_id = TRUE
#'     )),
#'     HW00002 = list(params = list(
#'       name = "subjage",
#'       type = "numeric",
#'       subject_age = TRUE
#'     ))
#'   )
#' )
#' result <- process.phenotypes:::exclude.by.age(phenotype.data, variable.summary)
exclude.by.age <- function(phenotype.data, variable.summary) {
  min.age <- as.numeric(variable.summary$globals$min_age_for_inclusion)
  stopifnot(!is.null(variable.summary$globals$min_age_for_inclusion), !is.na(min.age))
  flag <- 0
  for (i in seq_len(length(variable.summary$variables))) {
    if (!is.null(variable.summary$variables[[i]]$params$subject_age)) {
      if (variable.summary$variables[[i]]$params$subject_age) {
        flag <- i
        break
      }
    }
  }
  if (flag) {
    ## possible future feature: recognize date of birth column and attempt to rescue NA ages
    subjects.dropped <- length(which(phenotype.data[, flag] < min.age | is.na(phenotype.data[, flag])))
    variable.summary$subjects.excluded.for.age <- subjects.dropped
    phenotype.data <- phenotype.data[phenotype.data[, flag] >= min.age & !is.na(phenotype.data[, flag]), ]
  } else {
    stop(
      "Please indicate which variable should be used as the subject age ",
      "by adding `subject_age: true` to the appropriate section of the configuration."
    )
  }
  list(
    phenotype.data = phenotype.data,
    variable.summary = variable.summary
  )
}

#' Convert values that should be dates to year
#'
#' @description
#' Given string vector with malformed date entries, attempt
#' to force conversion to YYYY.
#'
#' @details
#' Extracts two or four digit year values from dates of several formats,
#' assigns likely century (20 or 19), and replaces the original date
#' with just the four-digit year value.  Also removes extremely low
#' likely erroneous year values.
#'
#' With new data, new support for the following data formats:
#' - YYYY
#' - YYYY-##-##
#' - any amount of text; a space, -, or /; and year as YY or YYYY
#' - any of the 12 Gregorian calendar months without typos, an optional comma, and then YYYY
#'
#' @param vec Character vector of input phenotype content.
#' @param var.summary List of variable summary entry for this particular variable.
#' @return Modified version of input vector with values cleaned as described above.
#' @examples
#' data <- c("1987", "1999-12-11")
#' var.summary <- list()
#' result <- process.phenotypes:::parse.date(data, var.summary)
parse.date <- function(vec, var.summary) {
  date.leading.year <- "^(\\d{4})-\\d{2}-\\d{2}$"
  date.leading.year.match <- stringr::str_detect(vec, date.leading.year) & !is.na(vec)
  date.trailing.year <- "^.*[/ -](\\d{2}|\\d{4})$"
  date.trailing.year.match <- stringr::str_detect(vec, date.trailing.year) & !is.na(vec)
  date.year.only <- "^(\\d{4})$"
  date.year.only.match <- stringr::str_detect(vec, date.year.only) & !is.na(vec)
  date.text.month.year <- paste("^(january|february|march|april|may|june|july",
    "|august|september|october|november|december),?(\\d{4})$",
    sep = ""
  )
  date.text.month.year.match <- stringr::str_detect(vec, date.text.month.year) & !is.na(vec)
  res <- rep(NA, length(vec))
  res[date.leading.year.match] <- stringr::str_replace(
    vec[date.leading.year.match],
    date.leading.year, "\\1"
  )
  res[date.trailing.year.match &
    !date.leading.year.match] <- stringr::str_replace(
    vec[date.trailing.year.match &
      !date.leading.year.match],
    date.trailing.year, "\\1"
  )
  res[date.year.only.match] <- stringr::str_replace(
    vec[date.year.only.match],
    date.year.only, "\\1"
  )
  res[date.text.month.year.match] <- stringr::str_replace(
    vec[date.text.month.year.match],
    date.text.month.year, "\\2"
  )
  res <- as.numeric(res)
  res[res <= 21 & !is.na(res)] <- res[res <= 21 & !is.na(res)] + 2000
  res[res < 100 & !is.na(res)] <- res[res < 100 & !is.na(res)] + 1900
  res[res < 1800 & !is.na(res)] <- NA
  var.summary$invalid.date.entries <- vec[(!(date.leading.year.match |
    date.trailing.year.match |
    date.year.only.match |
    date.text.month.year.match) | is.na(res)) & !is.na(vec)]
  list(phenotype.data = res, variable.summary = var.summary)
}
