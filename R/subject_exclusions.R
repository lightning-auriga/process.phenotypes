#' Remove subjects from a data frame based on a pre-computed
#' metric and a bound on that metric.
#'
#' @description
#' Assuming metrics like per-subject type conversion failure
#' counts have been computed elsewhere, this function removes
#' rows exceeding some bound on that value.
#'
#' @param phenotype.data data.frame, input phenotype data
#' @param metric.vec numeric vector, pre-computed values,
#' one per row of input data frame, to be used for determining
#' exclusion
#' @param upper.limit numeric, bound on computed metric
#' above which corresponding subjects will be removed
exclude.subjects.by.metric <- function(phenotype.data,
                                       metric.vec,
                                       upper.limit) {
  stopifnot(is.data.frame(phenotype.data))
  stopifnot(
    is.numeric(metric.vec),
    length(which(is.na(metric.vec))) == 0,
    length(metric.vec) == nrow(phenotype.data)
  )
  if (is.null(upper.limit)) {
    upper.limit <- Inf
  }
  stopifnot(
    is.numeric(upper.limit),
    length(upper.limit) == 1,
    !is.na(upper.limit)
  )
  phenotype.data[metric.vec <= upper.limit, ]
}
