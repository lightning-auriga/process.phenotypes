#' @title
#' Remove bad columns from a phenotype data frame
#'
#' @description
#' Removes columns under the following criteria:
#' - column name is NA
#' - column name is empty string
#'
#' @details
#' This function is a legacy of the earliest and messiest
#' datasets we encountered during the development of this
#' package. If you're lucky, such columns will not manifest
#' themselves in your dataset at all. If you're unlucky
#' and you have malformed headers, please be cautious:
#' in our experience, this tends to be a proxy indicator
#' for many types of pernicious row shifting.
#'
#' @param df Data frame of input phenotype content with column headers.
#' @return Data frame containing modified version of input with
#' relevant bad columns removed.
#' @examples
#' phenotype.data <- data.frame(1:3, 4:6, 7:9)
#' colnames(phenotype.data) <- c("", NA, "ok name")
#' phenotype.data <- process.phenotypes:::remove.invalid.columns(phenotype.data)
remove.invalid.columns <- function(df) {
  res <- df
  res[, which(colnames(res) == "" | is.na(colnames(res)))] <- NULL
  res
}
