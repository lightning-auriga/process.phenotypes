#' @title
#' Apply config-specified R commands to generate derived variables
#'
#' @description
#' Userspace configuration of a dataset can contain a block of
#' derived variables, which are computed dynamically (after
#' primary dataset cleaning) from existing variables. This uses
#' functionality similar to that of dependency checking to allow
#' arbitrary evaluation of code from the config.
#'
#' Evaluation of derived variables may use any variable, either
#' raw or derived, from the user config. Evaluation is conducted
#' in a restricted environment that prevents modification to the
#' primary phenotype dataset.
#'
#' @details
#' After the first run with a derived variable, that variable will
#' be present in the output dataset, and in its output data
#' dictionary. Subsequent loads of the data into this software
#' will treat the derived variable as a primary variable, and
#' it will not be recomputed.
#'
#' At this time, errors encountered within the sequestered environment
#' that indicate merely that a valid dependency has not yet been
#' evaluated are echoed to terminal. As long as the progression
#' of the function continues, these errors are expected and
#' not indicative of a failure in overall evaluation.
#'
#' If such sporadic errors are bothersome, the simplest
#' solution is to reorder the specification of derived variables in the
#' dataset configuration file such that each variable
#' in turn can be computed when the variables are evaluated
#' from the beginning of the derived block to the end.
#' That is to say: make sure your derived variables only depend
#' on variables above them in the file.
#'
#' Otherwise, you can wait until we get around to patching
#' this functionality to suppress intermittent errors. Sorry for
#' the inconvenience.
#'
#' @param phenotype.data Data frame of loaded phenotypes.
#' @param variable.summary List representation of configuration data
#' per variable.
#' @return List, with first entry `phenotype.data` a modified version
#' of input data frame augmented with resolved derived variables,
#' and second entry `variable.summary` a modified version of the
#' input config list with derived variable entries added in order
#' to the `variables` block for eventual reporting.
#' @importFrom methods is
#' @examples
#'
#' phenotype.data <- data.frame(
#'   HW00001 = c("A", "B", "C", "D"),
#'   HW00002 = 1:4
#' )
#' variable.summary <- list(
#'   variables = list(
#'     HW00001 = list(type = "string"),
#'     HW00002 = list(type = "numeric")
#'   ),
#'   derived = list(
#'     DV00001 = list(
#'       name = "only depends on variables",
#'       code = "2 * HW00002"
#'     ),
#'     DV00002 = list(
#'       name = "variables and derived",
#'       code = "2 * HW00002 + DV00001"
#'     )
#'   )
#' )
#' solved.data <- process.phenotypes:::create.derived.variables(
#'   phenotype.data,
#'   variable.summary
#' )
create.derived.variables <- function(phenotype.data, variable.summary) {
  not.done.list <- variable.summary$derived
  previous.list.length <- 0
  while (length(not.done.list) > 0 && length(not.done.list) != previous.list.length) {
    previous.list.length <- length(not.done.list)
    for (i in names(not.done.list)) {
      if (i %in% colnames(phenotype.data)) {
        not.done.list[[i]] <- NULL
        next
      }
      derived.name <- variable.summary$derived[[i]]$name
      derived.code <- variable.summary$derived[[i]]$code
      derived.exprs <- rlang::parse_exprs(derived.code)
      if (identical(derived.exprs, list())) {
        not.done.list[[i]] <- NULL
        next
      }
      derived.result <- evaluate.derived.expressions(phenotype.data, derived.exprs)
      if (!is(derived.result, "try-error")) {
        not.done.list[[i]] <- NULL
        stopifnot(!is.list(derived.result), length(derived.result) == nrow(phenotype.data))
        phenotype.data[, i] <- derived.result
        variable.summary$variables[[i]] <- list(
          original.name = variable.summary$derived[[i]]$name,
          params = variable.summary$derived[[i]]
        )
      }
    }
  }
  if (length(not.done.list) > 0) {
    stop(paste("Unable to successfully evaluate expressions for: ", paste(not.done.list, collapse = ", "), sep = ""))
  }
  list(
    phenotype.data = phenotype.data,
    variable.summary = variable.summary
  )
}

#' @title
#' Evaluate user expressions for computing a derived variable
#'
#' @description
#' An environment and data mask providing access to the phenotype
#' dataset column names are created for a specific derived variable,
#' and all user calculations are conducted in those quarantined
#' environments. This should prevent any accidental overwrites
#' of loaded data in the primary environment.
#'
#' @details
#' This is an internal utility function, and should not be called
#' otherwise. Failure of this evaluation may either be indicative
#' of bugs in the input code, or the absence of a required variable
#' in the phenotype dataset (most likely because a derived variable
#' is calling another derived variable that has not yet been
#' computed). The latter situation should be correctly handled
#' by calling logic that attempts iterative evaluation of
#' derived variable statements.
#'
#' @param phenotype.data Data frame of loaded phenotypes.
#' @param derived.exprs List of parsed expressions from a derived
#' variable code block.
#' @return Either a vector of length the number of rows of the working
#' phenotype dataset representing
#' the derived variable, or an object of class "try-error" indicating
#' the failure of the evaluation chain.
#' @importFrom methods is
#' @usage NULL
evaluate.derived.expressions <- function(phenotype.data, derived.exprs) {
  my.data.mask <- rlang::as_data_mask(phenotype.data)
  my.env <- rlang::caller_env()
  for (expr in derived.exprs) {
    # if evaluation fails, add it to a list for re-trying (e.g. if
    # a derived variable is required but doesn't exist yet)
    derived.result <- try(
      rlang::eval_tidy(expr, env = my.env, data = my.data.mask)
    )
    if (is(derived.result, "try-error")) {
      break
    }
  }
  derived.result
}
