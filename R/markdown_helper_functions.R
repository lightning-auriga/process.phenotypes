#' Helper function to create highest/lowest ten tables in markdown report.
#'
#' @description
#' Takes a named vector containing values (e.g. subjects) and counts,
#' sorts by number of counts, takes the top ten, and returns a data.frame
#'
#' @details
#' This is a helper function used to grab the highest or lowest ten values
#' to print as a table in the markdown report.
#'
#' @param decreasing logical, passed to sort to control increasing or decreasing order
#' @param vec named vector, containing values and counts
#' @param column.label string, name of first column (e.g. Subjects, Variables)
#' @return data.frame with ten rows containing values and counts
get.top.ten <- function(decreasing, vec, column.label) {
  ten.df <- data.frame(c(), c())
  if (length(vec) > 0) {
    vec.sorted <- sort(vec, decreasing = decreasing)[1:min(10, length(vec))]
    ten.df <- data.frame(names(vec.sorted), vec.sorted)
    colnames(ten.df) <- c(column.label, "Counts")
    rownames(ten.df) <- NULL
  }
  ten.df
}
