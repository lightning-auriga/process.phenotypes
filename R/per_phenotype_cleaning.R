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
  stopifnot(ncol(phenotype.data) == length(variable.summary))
  for (i in seq_len(length(variable.summary))) {
    target.type <- variable.summary[[i]]$params$type
    if (is.null(target.type) || grepl("string", target.type, ignore.case = TRUE)) {
      ## string
      next
    } else if (grepl("^categorical$|^ordinal$|^binary$", target.type, ignore.case = TRUE)) {
      ## categorical or ordinal or binary
      result.list <- phenotypeprocessing::reformat.factor(phenotype.data[, i], variable.summary[[i]])
      if (grepl("ordinal", target.type, ignore.case = TRUE)) {
        result.list$phenotype.data <- ordered(result.list$phenotype.data,
          levels = levels(result.list$phenotype.data)
        )
      }
      phenotype.data[, i] <- result.list$phenotype.data
      variable.summary[[i]] <- result.list$variable.summary
    } else if (grepl("numeric", target.type, ignore.case = TRUE)) {
      ## numeric
      result.list <- phenotypeprocessing::reformat.numerics(phenotype.data[, i], variable.summary[[i]])
      phenotype.data[, i] <- result.list$phenotype.data
      variable.summary[[i]] <- result.list$variable.summary
    } else if (grepl("^blood[_ ]?pressure$|^bp$", target.type, ignore.case = TRUE)) {
      ## blood pressure
      result.list <- phenotypeprocessing::reformat.blood.pressure(phenotype.data[, i], variable.summary[[i]])
      phenotype.data[, i] <- result.list$phenotype.data
      variable.summary[[i]] <- result.list$variable.summary
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
