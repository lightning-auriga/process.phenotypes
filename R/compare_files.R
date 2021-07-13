#' Compare two files for approximate identity of columns
#'
#' @details
#' as a utility function, this compares two datasets
#' for approximately identical columns, based not on
#' the header labels but on column values. this is
#' likely to completely fail until at least minimal
#' initial processing with the main methods of this
#' package.
#'
#' @description
#' TBD
#'
#' @param first.filename character vector, name of
#' first file to load and compare
#' @param first.id.colname character vector, name
#' of first file column to treat as subject ID
#' @param second.filename character vector, name of
#' second file to load and compare
#' @param second.id.colnmae character vector, name
#' of second file column to treat as subject ID
#'
#' note that this function is primary used for its
#' side effect, emitting content to screen.
#'
#' @export compare.files
compare.files <- function(first.filename,
                          first.id.colname,
                          second.filename,
                          second.id.colname) {
  data1 <- read.table(first.filename,
    header = TRUE,
    stringsAsFactors = FALSE,
    quote = "", sep = "\t",
    comment.char = ""
  )
  data2 <- read.table(second.filename,
    header = TRUE,
    stringsAsFactors = FALSE,
    quote = "", sep = "\t",
    comment.char = ""
  )
  ## compare all columns in dataset 1 to dataset 2
  for (i in seq_len(ncol(data1))) {
    compare.columns(data1[, i], data2)
  }
  ## compare all columns in dataset 2 to dataset 1
  for (i in seq_len(ncol(data2))) {
    compare.columns(data2[, i], data1)
  }
}

#' Compare a column to another data frame for approximate identity
#'
#' @details
#' a single column is compared against all columns of a data frame,
#' to try to find the most likely identical column in the target.
#' this is likely to completely fail until at least minimal
#' initial processing with the main methods of this
#' package.
#'
#' @description
#' TBD
#'
#' @param query.column vector, data to be queried against target columns
#'
compare.columns <- function(query.column, query.colname, target.df) {

}
