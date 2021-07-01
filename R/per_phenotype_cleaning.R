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
#' @export apply.type.conversions
apply.type.conversions <- function(phenotype.data, variable.summary) {
  stopifnot(ncol(phenotype.data) == length(variable.summary$variables))
  for (i in seq_len(length(variable.summary$variables))) {
    target.type <- variable.summary$variables[[i]]$params$type
    if (is.null(target.type)) {
      ## null
      next
    }
    if (grepl("string", target.type, ignore.case = TRUE)) {
      ## string
      next
    } else if (grepl("^categorical$|^ordinal$|^binary$", target.type, ignore.case = TRUE)) {
      ## categorical or ordinal or binary
      result.list <- phenotypeprocessing::reformat.factor(phenotype.data[, i], variable.summary$variables[[i]])
      if (grepl("ordinal", target.type, ignore.case = TRUE)) {
        result.list$phenotype.data <- ordered(result.list$phenotype.data,
          levels = levels(result.list$phenotype.data)
        )
      }
      phenotype.data[, i] <- result.list$phenotype.data
      variable.summary$variables[[i]] <- result.list$variable.summary
    } else if (grepl("numeric", target.type, ignore.case = TRUE)) {
      ## numeric
      result.list <- phenotypeprocessing::reformat.numerics(phenotype.data[, i], variable.summary$variables[[i]])
      phenotype.data[, i] <- result.list$phenotype.data
      variable.summary$variables[[i]] <- result.list$variable.summary
    } else if (grepl("^blood[_ ]?pressure$|^bp$", target.type, ignore.case = TRUE)) {
      ## blood pressure
      result.list <- phenotypeprocessing::reformat.blood.pressure(phenotype.data[, i], variable.summary$variables[[i]])
      phenotype.data[, i] <- result.list$phenotype.data
      variable.summary$variables[[i]] <- result.list$variable.summary
    } else {
      stop(
        "In apply.type.conversions, unrecognized type for variable \"",
        colnames(phenotype.data)[i], "\": \"",
        target.type, "\""
      )
    }
  }

  list(
    phenotype.data = phenotype.data,
    variable.summary = variable.summary
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
#' @export apply.bounds
apply.bounds <- function(phenotype.data, variable.summary) {
  stopifnot(ncol(phenotype.data) == length(variable.summary$variables))
  for (i in seq_len(length(variable.summary$variables))) {
    target.type <- variable.summary$variables[[i]]$params$type
    if (!is.null(target.type)) {
      if (grepl("numeric", target.type, ignore.case = TRUE)) {
        var.min <- variable.summary$variables[[i]]$params$bounds$min
        if (!is.null(var.min)) {
          var.min <- as.numeric(var.min)
          if (!is.na(var.min)) {
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
          if (!is.na(var.max)) {
            # count and apply max threshold
            num.max <- length(phenotype.data[phenotype.data[, i] > var.max &
              !is.na(phenotype.data[, i]), i])
            variable.summary$variables[[i]]$num.above.max <- num.max
            phenotype.data[phenotype.data[, i] > var.max &
              !is.na(phenotype.data[, i]), i] <- NA
          }
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
#'
#' @param phenotype.data data frame, loaded phenotype data with
#' standardized headers; all columns should be character vectors
#' @param variable.summary list, per-column summary information
#' and parameters from yaml input
#' @return list, 'phenotype.data' contains phenotype information with
#' subjects below the age threshold excluded, 'variable.summary' contains
#' input variable summary with information about excluded subjects.
#' @export exclude.by.age
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
    subjects.dropped <- length(which(phenotype.data[, flag] < min.age))
    variable.summary$subjects.excluded.for.age <- subjects.dropped
    phenotype.data <- phenotype.data[phenotype.data[, flag] >= min.age, ]
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
