#' Remove subjects from a data frame based on a pre-computed
#' metric and a bound on that metric.
#'
#' @description
#' Assuming metrics like per-subject type conversion failure
#' counts have been computed elsewhere, this function removes
#' rows exceeding some bound on that value.
#'
#' @param phenotype.data data.frame, input phenotype data
#' @param variable.summary list, variable configuration data
#' @param metric.vec integer vector, pre-computed values,
#' with corresponding subject IDs as names, to be used for
#' determining exclusion
#' @param upper.limit numeric, bound on computed metric
#' above which corresponding subjects will be removed
exclude.subjects.by.metric <- function(phenotype.data,
                                       variable.summary,
                                       metric.vec,
                                       upper.limit) {
  stopifnot(is.data.frame(phenotype.data))
  stopifnot(is.list(variable.summary))
  stopifnot(
    is.integer(metric.vec),
    length(which(is.na(metric.vec))) == 0
  )
  if (is.null(upper.limit)) {
    upper.limit <- Inf
  }
  stopifnot(
    is.numeric(upper.limit),
    length(upper.limit) == 1,
    !is.na(upper.limit)
  )
  subject.id.index <- find.subject.id.index(variable.summary)
  stopifnot(length(which(names(metric.vec) %in% phenotype.data[, subject.id.index])) == length(metric.vec))
  aligned.data <- metric.vec[phenotype.data[, subject.id.index]]
  aligned.data[is.na(aligned.data)] <- 0
  phenotype.data[aligned.data <= upper.limit, ]
}
