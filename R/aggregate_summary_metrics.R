#' Find, for each subject, the number of variables for
#' which that subject has data of the wrong type.
#'
#' @description
#' This aggregates non-compliant subject ID information from
#' all variables and reduces them to counts of instances
#' per subject, for downstream tabular reporting.
#'
#' @details
#' These counts may be inflated per subject if there is
#' something specifically wrong with a block of related
#' variables. This count summary is hopefully a proxy
#' for subject-specific data corruption, so they can
#' be removed before downstream analysis.
#'
#' @param variable.summary list, variable configuration and
#' summary data
#' @return named integer vector; values are counts of non-compliant
#' variables per subject, names are subject IDs
aggregate.subjects.wrong.type <- function(variable.summary) {
  all.instances <- unlist(lapply(variable.summary$variables, function(i) {
    i$subjects.wrong.type
  }))
  res <- table(all.instances)
  res.names <- names(res)
  res <- as.integer(res)
  names(res) <- res.names
  res
}

#' Find, for each variable, the number of subjects with entries
#' not matching the specified type.
#'
#' @description
#' This aggregates counts of non-compliant subjects per variable,
#' for downstream tabular reporting.
#'
#' @details
#' This is distinct from other reporting of non-compliant values,
#' which are used for screening and variable-specific NA exclusion.
#' In some instances, inflated counts may reflect either
#' corruption of the variable, due to technical reasons or changes
#' in questionnaire content midstream, or potentially configuration
#' failures in this software.
#'
#' @param variable.summary list, variable configuration and
#' summary data
#' @return named numeric vector; values are counts of non-compliant
#' subjects per variable, names are variable names
aggregate.variables.wrong.type <- function(variable.summary) {
  counts <- sapply(variable.summary$variables, function(i) {
    length(i$subjects.wrong.type)
  })
  names(counts) <- names(variable.summary$variables)
  counts
}
