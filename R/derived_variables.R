#' Apply config-specified R commands to generate derived variables
#'
#' @details
#'
#'
#' @description
#' TBD
#'
#' @param phenotype.data data.frame, input phenotype data
#' @param variable.summary list, configuration data per variable
#' @return list, modified version of input variable.summary argument
#' with some reporting information about dependency check success/failure
create.derived.variable <- function(phenotype.data, variable.summary) {
  not.done.list <- variable.summary$derived
  previous.list.length <- 0
  while (length(not.done.list) > 0 & length(not.done.list) != previous.list.length) {
    previous.list.length <- length(not.done.list)
    for (i in names(not.done.list)) {
      if (i %in% colnames(phenotype.data)) {
        not.done.list[[i]] <- NULL
        next
      }
      derived.name <- variable.summary$derived[[i]]$name
      derived.code <- variable.summary$derived[[i]]$code
      derived.exprs <- rlang::parse_exprs(derived.code)
      if (derived.exprs == list()) {
        next
      }
      derived.result <- evaluate.derived.expressions(phenotype.data, derived.exprs)
      if (class(derived.result) != "try-error") {
        not.done.list[[i]] <- NULL
        stopifnot(is.vector(derived.result), length(derived.result) == nrow(phenotype.data))
        phenotype.data[, i] <- derived.result
        variable.summary$variables[[i]] <- variable.summary$derived[[i]]
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
# TODO: add to schema, add function call!

#' TODO
#'
#' @details
#'
#'
#' @description
#' TBD
#'
#' @param phenotype.data data.frame, input phenotype data
#' @param derived.exprs list, parsed expressions
#' @return TBD
evaluate.derived.expressions <- function(phenotype.data, derived.exprs) {
  my.data.mask <- as_data_mask(phenotype.data)
  my.env <- caller_env()
  for (expr in derived.exprs) {
    # if evaluation fails, add it to a list for re-trying (e.g. if
    # a derived variable is required but doesn't exist yet)
    derived.result <- try(
      rlang::eval_tidy(expr, env = my.env, data = my.data.mask)
    )
    if (class(derived.result) == "try-error") {
      break
    }
  }
  derived.result
}
