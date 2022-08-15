#' @title
#' Remove subjects from a data frame based on a pre-computed
#' metric and a bound on that metric
#'
#' @description
#' Assuming metrics like per-subject type conversion failure
#' counts have been computed elsewhere, this function removes
#' rows exceeding some bound on that value. This functionality
#' was added when the package was being used on particularly
#' malformed data, and certain subjects evidently contained
#' large numbers of entries that did not conform to the
#' corresponding variables' expected types. Better behaved
#' datasets should effectively never lose subjects to
#' these filters.
#'
#' @details
#' process.phenotypes tends to try to avoid removing subjects
#' entirely unless the subject is exceptionally toxic. The instances
#' when this happens are:
#'
#' - consent failures
#' - age exclusions
#' - subjects with invalid subject ID
#' - subjects failing these metric exclusions
#'
#' Such is the severity we ascribe to failures of data type
#' conformance metrics. Information about subjects removed
#' by this function is reported in the cleaning report.
#' If subjects are ever excluded from your dataset for
#' these metrics, you should inspect every single one
#' of them to diagnose what particularly led to the exclusion.
#'
#' @param phenotype.data Data frame of input phenotype data.
#' @param variable.summary List of dataset configuration.
#' @param metric.vec Integer vector of pre-computed values,
#' with corresponding subject IDs as names, to be used for
#' determining exclusion.
#' @param upper.limit Numeric bound on computed metric
#' above which corresponding subjects will be removed.
#' @return Data frame, a modified version of the input phenotype
#' data with any subjects (rows) failing the exclusion criteria
#' entirely removed.
#' @examples
#' phenotype.data <- data.frame(
#'   HW00001 = c("A", "B", "C", "D"),
#'   HW00002 = 18:21
#' )
#' variable.summary <- list(variables = list(
#'   HW00001 = list(params = list(
#'     name = "subjid",
#'     type = "string",
#'     subject_id = TRUE
#'   )),
#'   HW00002 = list(params = list(
#'     name = "subjage",
#'     type = "numeric"
#'   ))
#' ))
#' metric.vec <- 1:4
#' names(metric.vec) <- phenotype.data[, 1]
#' result <- process.phenotypes:::exclude.subjects.by.metric(
#'   phenotype.data, variable.summary,
#'   metric.vec, as.integer(2)
#' )
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
  aligned.data <- metric.vec[phenotype.data[, subject.id.index]]
  aligned.data[is.na(aligned.data)] <- 0
  phenotype.data[aligned.data <= upper.limit, ]
}
