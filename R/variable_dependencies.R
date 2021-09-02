#' Apply config-specified R commands as variable sanity checks
#'
#' @details
#' Certain variables in an input dataset should have defined relationships
#' between one another. This function will evaluate, in a hopefully safe manner,
#' R syntax expressions from the config yaml files and report their results
#' to the per-variable summary data, for reporting in the output html.
#'
#' @description
#' TBD
#'
#' @param phenotype.data data.frame, input phenotype data
#' @param variable.summary list, configuration data per variable
#' @return list, modified version of input variable.summary argument
#' with some reporting information about dependency check success/failure
check.variable.dependencies <- function(phenotype.data, variable.summary) {
  ## first step: find the subject ID variable. this will enable prettier error
  ## reporting for some styles of dependencies
  subject.id.column.index <- find.subject.id.index(variable.summary)
  for (i in seq_len(ncol(phenotype.data))) {
    dependencies <- variable.summary$variables[[i]]$params$dependencies
    if (!is.null(dependencies)) {
      variable.summary$variables[[i]]$dependency.results <- list()
      for (j in names(dependencies)) {
        dependency.name <- dependencies[[j]]$name
        dependency.condition <- dependencies[[j]]$condition
        stopifnot(
          !is.null(dependency.name),
          !is.null(dependency.condition)
        )
        ## in theory, this creates an unevaluated expr structure from the
        ## input string and then evaluates it in a quarantined environment.
        ## the point of that is that this should prevent this unknown code
        ## from overwriting anything in the current environment.
        dependency.result <- rlang::eval_tidy(rlang::parse_expr(dependency.condition),
          data = phenotype.data
        )
        ## postconditions: result must be logical
        stopifnot(is.logical(dependency.result))
        ## if it's a single logical, it's just an overall test
        if (length(dependency.result) == 1) {
          variable.summary$variables[[i]]$dependency.results[[j]] <- dependency.result
          next
        } else if (length(dependency.result) == nrow(phenotype.data)) {
          ## if it's a logical as long as the data frame has rows,
          ## it's a per-subject test that should be true for everyone
          variable.summary$variables[[i]]$dependency.results[[j]] <-
            as.character(phenotype.data[!dependency.result, subject.id.column.index])
          next
        }
        ## it's something bizarre that this very tiny brain tester
        ## cannot understand
        stop("for variable ", names(variable.summary$variables)[i],
          ", dependency \"", dependency.name, "\", evaluated result",
          " is not in a recognized format (should either be a logical",
          " of length 1 or length nrow(phenotype.data))",
          sep = ""
        )
      }
    }
  }
  variable.summary
}

#' Apply config-specified R commands as variable sanity checks
#'
#' @details
#' Certain variables in an input dataset should have defined relationships
#' between one another. This function will evaluate, in a hopefully safe manner,
#' R syntax expressions from the config yaml files and report their results
#' to the per-variable summary data, for reporting in the output html.
#'
#' @description
#' TBD
#'
#' @param phenotype.data data.frame, input phenotype data
#' @param variable.summary list, configuration data per variable
#' @return data frame, modified version of input phenotype.data argument
#' with requested subject/variable entries replaced with NA on
#' dependency failure
dependency.failure.handling <- function(phenotype.data, variable.summary) {
  ## need to map reported failed subject ID back to phenotype.data row index
  subject.id.column.index <- find.subject.id.index(variable.summary)
  ## tracking variables, to determine number of dependency failures out of possible max
  all.targeted.variables <- c()
  na.applied.mask <- matrix(FALSE, nrow = nrow(phenotype.data), ncol = ncol(phenotype.data))
  colnames(na.applied.mask) <- colnames(phenotype.data)
  for (i in seq_len(ncol(phenotype.data))) {
    dependencies <- variable.summary$variables[[i]]$params$dependencies
    results <- variable.summary$variables[[i]]$dependency.results
    stopifnot(length(dependencies) == length(results))
    for (j in seq_len(length(dependencies))) {
      exclude.on.failure <- dependencies[[j]]$exclude_on_failure
      all.targeted.variables <- c(
        all.targeted.variables,
        exclude.on.failure
      )
      exclude.subjects <- results[[j]]
      if (is.null(exclude.on.failure) |
        is.null(exclude.subjects)) {
        next
      }
      exclude.indices <- phenotype.data[, subject.id.column.index] %in% exclude.subjects
      stopifnot(identical(
        exclude.on.failure %in% colnames(phenotype.data),
        rep(TRUE, length(exclude.on.failure))
      ))
      was.na.before <- is.na(phenotype.data[, exclude.on.failure])
      phenotype.data[exclude.indices, exclude.on.failure] <- NA
      is.na.now <- is.na(phenotype.data[, exclude.on.failure])
      na.applied.mask[, exclude.on.failure] <- na.applied.mask[, exclude.on.failure] | (is.na.now & !was.na.before)
    }
  }
  variable.summary$actual.nas.from.deps <- sum(na.applied.mask[, unique(all.targeted.variables)])
  variable.summary$possible.nas.from.deps <- nrow(na.applied.mask) * length(unique(all.targeted.variables))
  list(
    phenotype.data = phenotype.data,
    variable.summary = variable.summary
  )
}

#' Locate a variable in the dataset flagged as subject ID
#'
#' @details
#' the input yaml specification allows a single variable
#' to be flagged as "subject_id: true" for context-specific
#' behaviors.
#'
#' @description
#' if this column is not located, stop() is called. in the
#' future, this column's presence and uniqueness will be
#' enforced upstream with yaml consistency checking.
#'
#' @param variable.summary list, input configuration data
#' @return integer, index of subject ID column in input data
find.subject.id.index <- function(variable.summary) {
  for (i in seq_len(length(variable.summary$variables))) {
    if (!is.null(variable.summary$variables[[i]]$params$subject_id)) {
      if (variable.summary$variables[[i]]$params$subject_id) {
        return(i)
      }
    }
  }
  stop("no variable in input config yaml was flagged as the subject ID ",
    "variable; this is required exactly once per file and can be added ",
    "with the line \"subject_id: true\" under the corresponding variable",
    sep = ""
  )
}
