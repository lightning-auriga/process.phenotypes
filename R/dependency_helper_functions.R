#' Test dependency of one variable on "yes" response of another
#'
#' @details
#' Specifically, this function tests two variables where one result depends
#' on a "yes" result in the other.
#'
#' @description
#' Sanity check to ensure that related rules are behaving as expected.
#'
#' @param dependent.variable string, name of dependent variable to operate on
#' @param independent.variable string, name of independent variable
#' @param allow.no logical, whether to allow "no" answers as equivalent to NA
#' @return a vector of length nrow(phenotype.data) representing the results
#' of the dependency test
#' @export response.depends.on.yes
response.depends.on.yes <- function(dependent.variable, independent.variable, allow.no = FALSE) {
  if (allow.no) {
    is.na(dependent.variable) |
      dependent.variable == "no" |
      (!is.na(independent.variable) & independent.variable == "yes")
  } else {
    is.na(dependent.variable) |
      (!is.na(independent.variable) & independent.variable == "yes")
  }
}

#' Test dependency of one variable on non-NA response of another
#'
#' @details
#' Specifically, this function tests two variables where one result depends
#' on any non-NA result in the other.
#'
#' @description
#' Sanity check to ensure that related rules are behaving as expected.
#'
#' @param dependent.variable string, name of dependent variable to operate on
#' @param independent.variable string, name of independent variable
#' @param allow.no logical, whether to allow "no" answers as equivalent to NA
#' @param additional.na.levels vector, define alternative values to be treated
#' as NAs, e.g. "0 times"
#' @return a vector of length nrow(phenotype.data) representing the results
#' of the dependency test
#' @export response.depends.on.not.na
response.depends.on.not.na <- function(dependent.variable, independent.variable,
                                       allow.no = FALSE, additional.na.levels = c()) {
  if (allow.no) {
    is.na(dependent.variable) |
      dependent.variable == "no" |
      !(is.na(independent.variable) |
        independent.variable %in% additional.na.levels)
  } else {
    is.na(dependent.variable) |
      !(is.na(independent.variable) |
        independent.variable %in% additional.na.levels)
  }
}
