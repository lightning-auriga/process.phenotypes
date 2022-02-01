#' Remove bad columns from a phenotype data frame
#'
#' @details
#' Removes columns under the following criteria:
#'
#' - column name is NA
#'
#' - column name is empty string
#'
#' - others TBD
#'
#' @description
#' TBD
#'
#' @param df data frame, input phenotype content
#' @return modified version of input with certain bad columns
#' removed
#' @examples
#' \dontrun{
#' phenotype.data <- data.frame(1:3, 4:6, 7:9)
#' colnames(phenotype.data) <- c("", NA, "ok name")
#' phenotype.data <- remove.invalid.columns(phenotype.data)
#' }
remove.invalid.columns <- function(df) {
  res <- df
  res[, which(colnames(res) == "" | is.na(colnames(res)))] <- NULL
  res
}
