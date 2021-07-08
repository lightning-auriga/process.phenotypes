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
  for (i in seq_len(ncol(phenotype.data))) {

  }
}
