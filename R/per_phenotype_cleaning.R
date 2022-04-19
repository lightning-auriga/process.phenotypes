#' Apply type constraints to each variable from yaml config
#'
#' @details
#' Each yaml configuration block should have a "type" entry
#' (if not, will default to unmodified string) denoting what
#' data storage type is expected for this variable.
#'
#' @description
#' A short list of special handlers are implemented for particular
#' data conversion problems (e.g. numerics with trailing unit suffixes,
#' or blood pressure data)
#'
#' @param phenotype.data data frame, loaded phenotype data with
#' standardized headers; all columns should be character vectors
#' @param variable.summary list, per-column summary information
#' and parameters from yaml input
#' @return list, entry 'phenotype.data' is a data frame (modified
#' version of phenotype input data with conversions applied as appropriate);
#' entry 'variable.summary' is a list (modified version of input list
#' with summary information injected from certain handlers)
#' @seealso load.configuration
#' @keywords phenotypes yaml
apply.type.conversions <- function(phenotype.data, variable.summary) {
  stopifnot(ncol(phenotype.data) == length(variable.summary$variables))
  for (i in seq_len(length(variable.summary$variables))) {
    if (is.null(variable.summary$variables[[i]]$params$type)) {
      warning(paste("variable \"", names(variable.summary$variables)[i], "\" has null type specification", sep = ""))
    }
    target.type <- tolower(variable.summary$variables[[i]]$params$type)
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

#' Coordinate different function calls for type conversion
#'
#' @details
#' Each yaml configuration block should have a "type" entry
#' (if not, will default to unmodified string) denoting what
#' data storage type is expected for this variable.
#'
#' @description
#' A short list of special handlers are implemented for particular
#' data conversion problems (e.g. numerics with trailing unit suffixes,
#' or blood pressure data).  This function dispatches the appropriate
#' subroutine based on the detected data type.
#'
#' @param vec character vector, input phenotype content
#' @param var.summary list, variable summary entry for this particular variable
#' @param target.type character vector, denotes the expected type for each variable;
#' read from the yaml config
#' @return list, 'phenotype.data' contains converted phenotype information,
#' 'variable.summary' contains input variable summary.
#' @seealso apply.type.conversion
#' @keywords phenotypes yaml
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

#' Apply range bounds to each variable as defined in yaml config
#'
#' @details
#' Numeric variables can optionally have min/max bounds defined in
#' the config (under "bounds") - apply them here by replacing values
#' outside the bounds with NA.
#'
#' @description
#' Function to pull bounds from yaml config and apply to the correct
#' variables.
#'
#' @param phenotype.data data frame, loaded phenotype data with
#' standardized headers; all columns should be character vectors
#' @param variable.summary list, per-column summary information
#' and parameters from yaml input
#' @return list, entry 'phenotype.data' is a data frame (modified
#' version of phenotype input data with bounds applied as appropriate);
#' entry 'variable.summary' is a list (modified version of input list
#' with information on entries outside the defined bounds)
#' @seealso load.configuration
#' @keywords phenotypes yaml
apply.bounds <- function(phenotype.data, variable.summary) {
  stopifnot(ncol(phenotype.data) == length(variable.summary$variables))
  for (i in seq_len(length(variable.summary$variables))) {
    target.type <- variable.summary$variables[[i]]$params$type
    if (!is.null(target.type)) {
      if (grepl("^numeric$|^date$", target.type, ignore.case = TRUE)) {
        var.min <- variable.summary$variables[[i]]$params$bounds$min
        if (!is.null(var.min)) {
          var.min <- as.numeric(var.min)
          if (!is.na(var.min) & is.null(variable.summary$variables[[i]]$num.below.min)) {
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
          if (!is.na(var.max) & is.null(variable.summary$variables[[i]]$num.above.max)) {
            # count and apply max threshold
            num.max <- length(phenotype.data[phenotype.data[, i] > var.max &
              !is.na(phenotype.data[, i]), i])
            variable.summary$variables[[i]]$num.above.max <- num.max
            phenotype.data[phenotype.data[, i] > var.max &
              !is.na(phenotype.data[, i]), i] <- NA
          }
        }
        var.sd <- variable.summary$variables[[i]]$params$bounds$sd
        if (!is.null(var.sd) & is.null(variable.summary$variables[[i]]$num.beyond.sd)) {
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

#' Find additional NA aliases in user config and apply them to
#' particular variables
#'
#' @details
#' User config can optionally contain an 'na-values' sequence
#' containing entries that should be mapped to NA. This functionality
#' is only really reliable for factors and strings; numerics may
#' not match exactly as desired in some cases.
#'
#' @description
#' TBD
#'
#' @param phenotype.data data frame, loaded phenotype data with
#' standardized headers
#' @param variable.summary list, per-column summary information
#' and parameters from yaml input
#' @return data frame containing phenotype information with
#' subjects containing specified values set to NA
convert.variable.specific.na <- function(phenotype.data, variable.summary) {
  for (i in seq_len(length(variable.summary$variables))) {
    na.values <- variable.summary$variables[[i]]$params[["na-values"]]
    if (!is.null(na.values)) {
      if (is.vector(na.values)) {
        phenotype.data[phenotype.data[, i] %in% as.character(na.values), i] <- NA
      } else {
        stop(
          "for variable \"",
          variable.summary[[i]]$original.name,
          "\", na-values configuration option has ",
          "unrecognized type ", typeof(na.values)
        )
      }
    }
    if (!is.null(variable.summary$variables[[i]]$params[["suppress_output"]])) {
      if (variable.summary$variables[[i]]$params[["suppress_output"]]) {
        phenotype.data[, i] <- NA
      }
    }
  }
  phenotype.data
}

#' Exclude subjects below a certain user-defined age threshold
#'
#' @details
#' This function will exclude subjects whose age, as defined with the
#' `subject_age: true` flag in the config, falls below the threshold.
#' This is useful for excluding minors, for example.
#'
#' @description
#' TBD
#'
#' @param phenotype.data data frame, loaded phenotype data with
#' standardized headers; all columns should be character vectors
#' @param variable.summary list, per-column summary information
#' and parameters from yaml input
#' @return list, 'phenotype.data' contains phenotype information with
#' subjects below the age threshold excluded, 'variable.summary' contains
#' input variable summary with information about excluded subjects.
exclude.by.age <- function(phenotype.data, variable.summary) {
  min.age <- as.numeric(variable.summary$globals$min_age_for_inclusion)
  stopifnot(!is.null(variable.summary$globals$min_age_for_inclusion), !is.na(min.age))
  flag <- 0
  for (i in seq_len(length(variable.summary$variables))) {
    # TODO look for variable marked as subject age
    if (!is.null(variable.summary$variables[[i]]$params$subject_age)) {
      if (variable.summary$variables[[i]]$params$subject_age) {
        flag <- i
        break
      }
    }
  }
  if (flag) {
    ## TODO: recognize date of birth column and attempt to rescue NA ages
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
#' @details
#' Given string vector with malformed date entries, attempt
#' to force conversion to YYYY
#'
#' @description
#' Extracts two or four digit year values from dates of several formats,
#' assigns likely century (20 or 19), and replaces the original date
#' with just the four-digit year value.  Also removes extremely low
#' likely erroneous year values.
#'
#' With new data, new support for the following data formats:
#' - YYYY
#' - YYYY-##-##
#'
#' @param vec character vector, input phenotype content
#' @param var.summary list, variable summary entry for this particular variable
#' @return modified version of input with values cleaned as described above
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
