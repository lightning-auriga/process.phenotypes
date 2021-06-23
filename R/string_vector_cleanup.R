#' Basic global cleanup of entries in a phenotype data frame
#'
#' @details
#' Performs the following data modifications to non-numeric values:
#'
#' - convert all characters to lower case
#'
#' - remove all trailing and preceding whitespaces and collapse
#'   consecutive whitespaces
#'
#' - remove trailing and preceding non-alphanumerics
#'
#' - harmonize na, nan, not applicable values (unknowns?  blanks?)
#'
#' @description
#'
#' @param df data frame, input phenotype content
#' @return modified version of input with values cleaned as described
#' above
#' @export make.lowercase
#' @export remove.whitespace
#' @export remove.nonword.chars
#' @export normalize.missing.values
#' @examples
#' phenotype.data <- data.frame(
#'   c("WeiRd CaPs", "Trailing ", "NA"),
#'   c(";something", "Not Applicable", "Too   muchspace")
#' )
#' colnames(phenotype.data) <- c("col1", "col2")
#' phenotype.data <- make.lowercase(phenotype.data)
#' phenotype.data <- remove.whitespace(phenotype.data)
#' phenotype.data <- remove.nonword.chars(phenotype.data)
#' phenotype.data <- normalize.missing.values(phenotype.data)
make.lowercase <- function(df) {
  df <- data.frame(lapply(df, function(val) {
    tolower(val)
  }))
}
remove.whitespace <- function(df) {
  df <- data.frame(lapply(df, function(val) {
    stringr::str_squish(val)
  }))
}
remove.nonword.chars <- function(df) {
  df <- data.frame(lapply(df, function(val) {
    stringr::str_replace_all(val, "^\\W+|\\W+$", "")
  }))
}
normalize.missing.values <- function(df) {
  df <- data.frame(lapply(df, function(val) {
    stringr::str_replace_all(val,
      "^na$|^not applicable$|^nil$|^nan$|^$",
      replacement = NA_character_
    )
  }))
}
