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
#' @return named integer vector; values are counts of non-compliant
#' subjects per variable, names are variable names
aggregate.variables.wrong.type <- function(variable.summary) {
  counts <- sapply(variable.summary$variables, function(i) {
    length(i$subjects.wrong.type)
  })
  names(counts) <- names(variable.summary$variables)
  counts
}

#' Compute per-subject NA count across all variables
#'
#' @description
#' Computes a count for each subject of all NAs across
#' all variables, after all filtering and conversion has
#' been applied.
#'
#' @details
#' NAs can be present in input data or introduced at various
#' steps, and the overall count can be indicative of various
#' phenomena, including expected blockwise NA due to individual
#' subjects not completing all questionnaire sections.
#'
#' @param phenotype.data data frame, subject data, subjects
#' as rows, phenotypes as columns
#' @param variable.summary list, variable configuration
#' and summary data
#' @return named integer vector; values are counts of NAs
#' for each subject across all variables, names are subject IDs
compute.subject.na.count <- function(phenotype.data, variable.summary) {
  res <- apply(phenotype.data, 1, function(i) {
    length(which(is.na(i)))
  })
  names(res) <- phenotype.data[, find.subject.id.index(variable.summary)]
  res
}
#' Compute per-subject dependency failures counts across all variables
#'
#' @description
#' Computes a count for each subject of all dependency failures
#' across all variables, after all filtering and conversion has
#' been applied.
#'
#' @details
#' This obviously has a bunch of caveats associated with it.
#' The metric only means as much as the dependencies do.
#' Large numbers of dependency failures can be introduced
#' with particular discrepancies (not addressing atypical
#' "no" responses in some dependent questions, approximate
#' BMI deviations, atypical N/A values in string variables)
#' that will inflate these counts without useful meaning.
#' Discrepancies in early links in some dependency chains
#' can cause the same subject to fail an entire batch
#' of related variables (e.g. smoking, alcohol), and
#' while it's probably appropriate to penalize that datapoint
#' due to this phenomenon, it's not entirely clear
#' how many times that penalty should be applied.
#'
#' @param variable.summary list, variable configuration
#' and summary data
#' @return named integer vector; values are counts of
#' dependency failures for each subject across all variables,
#' names are subject IDs
aggregate.subject.dep.failures <- function(variable.summary) {
  all.instances <- unlist(lapply(variable.summary$variables, function(i) {
    unique(unlist(i$dependency.results))
  }))
  res <- table(all.instances)
  res.names <- names(res)
  res <- as.integer(res)
  names(res) <- res.names
  res
}
