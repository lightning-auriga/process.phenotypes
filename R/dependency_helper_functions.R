#' Test dependency of one variable on "yes" response of another
#'
#' @details
#' This function tests two variables where one result depends
#' on a "yes" result in the other.
#'
#' @description
#' Sanity check to ensure that related variables are behaving as expected.
#'
#' @param dependent.variable string, name of dependent variable to operate on
#' @param independent.variable string, name of independent variable
#' @param yes.aliases character vector, values to be interpreted as "yes"
#' in the independent variable
#' @param allow.no logical, whether to allow "no" answers as equivalent to NA
#' in the dependent variable
#' @param no.aliases character vector, values to be interpreted as
#' "no" responses in the dependent variable
#' @param additional.na.levels vector, define alternative values to be treated
#' as NAs in the independent variable, e.g. "0 times"
#' @return a vector of length nrow(phenotype.data) representing the results
#' of the dependency test
#' @export response.depends.on.yes
response.depends.on.yes <- function(dependent.variable, independent.variable,
                                    yes.aliases = c("yes"),
                                    allow.no = FALSE,
                                    no.aliases = c("no"),
                                    additional.na.levels = c()) {
  stopifnot(length(dependent.variable) >= 1)
  stopifnot(length(independent.variable) >= 1)
  stopifnot(is.character(yes.aliases), length(yes.aliases) > 0)
  stopifnot(is.logical(allow.no), length(allow.no) == 1)
  stopifnot(is.character(no.aliases), length(no.aliases) > 0)
  stopifnot(length(dependent.variable) == length(independent.variable))

  if (allow.no) {
    is.na(dependent.variable) |
      dependent.variable %in% no.aliases |
      independent.variable %in% additional.na.levels |
      (!is.na(independent.variable) & independent.variable %in% yes.aliases)
  } else {
    is.na(dependent.variable) |
      independent.variable %in% additional.na.levels |
      (!is.na(independent.variable) & independent.variable %in% yes.aliases)
  }
}

#' Test dependency of one variable on non-NA response of another
#'
#' @details
#' This function tests two variables where one result depends
#' on any non-NA result in the other.
#'
#' @description
#' Sanity check to ensure that related variables are behaving as expected.
#'
#' @param dependent.variable string, name of dependent variable to operate on
#' @param independent.variable string, name of independent variable
#' @param allow.no logical, whether to allow "no" answers as equivalent to NA
#' in the dependent variable
#' @param no.aliases character vector, values to be interpreted as
#' "no" responses in the dependent variable
#' @param additional.na.levels vector, define alternative values to be treated
#' as NAs in the independent variable, e.g. "0 times"
#' @return a vector of length nrow(phenotype.data) representing the results
#' of the dependency test
#' @export response.depends.on.not.na
response.depends.on.not.na <- function(dependent.variable, independent.variable,
                                       allow.no = FALSE, no.aliases = c("no"),
                                       additional.na.levels = c()) {
  stopifnot(length(dependent.variable) >= 1)
  stopifnot(length(independent.variable) >= 1)
  stopifnot(is.logical(allow.no), length(allow.no) == 1)
  stopifnot(is.character(no.aliases), length(no.aliases) > 0)
  stopifnot(length(dependent.variable) == length(independent.variable))

  if (allow.no) {
    is.na(dependent.variable) |
      dependent.variable %in% no.aliases |
      !(is.na(independent.variable) |
        independent.variable %in% additional.na.levels)
  } else {
    is.na(dependent.variable) |
      !(is.na(independent.variable) |
        independent.variable %in% additional.na.levels)
  }
}

#' Test that one variable is less than the other
#'
#' @details
#' This function tests that one variable is less than another.
#'
#' @description
#' Sanity check to ensure that related variables are behaving as expected.
#'
#' @param dependent.variable string, name of dependent variable to operate on
#' @param independent.variable string, name of independent variable
#' @return a vector of length nrow(phenotype.data) representing the results
#' of the dependency test
#' @export response.is.less.than
response.is.less.than <- function(dependent.variable, independent.variable) {
  stopifnot(is.numeric(dependent.variable), length(dependent.variable) > 1 |
    (!is.na(dependent.variable) & length(dependent.variable) == 1))
  stopifnot(is.numeric(independent.variable), length(independent.variable) > 1 |
    (!is.na(independent.variable) & length(independent.variable) == 1))
  stopifnot(length(dependent.variable) == length(independent.variable))

  is.na(dependent.variable) |
    is.na(independent.variable) |
    dependent.variable < independent.variable
}

#' Test concordance of duplicate variables
#'
#' @details
#' This function tests that two non-NA values are concordant.  Note
#' that non-NA/NA mismatches are allowed here, to focus on the most
#' toxic discordant cases.
#'
#' @description
#' Sanity check to ensure that suspected duplicate variables are concordant.
#'
#' @param dependent.variable string, name of dependent variable to operate on
#' @param independent.variable string, name of independent variable
#' @return a vector of length nrow(phenotype.data) representing the results
#' of the dependency test
#' @export response.is.duplicate.of
response.is.duplicate.of <- function(dependent.variable, independent.variable) {
  stopifnot(length(dependent.variable) == length(independent.variable))
  stopifnot(class(dependent.variable) == class(independent.variable))

  is.na(dependent.variable) |
    is.na(independent.variable) |
    dependent.variable == independent.variable
}

#' Test consistency of age and year of birth
#'
#' @details
#' This function tests that reported and calculated age is within a certain
#' user-defined tolerance.
#'
#' @description
#' Sanity check to ensure that reported age in years and calculated age from
#' reported year of birth are roughly equivalent.  Note that this can also be
#' used for other age comparisons, e.g. age at diagnosis vs. date of diagnosis.
#'
#' @param dependent.variable string, name of dependent variable to operate on,
#' should represent DOB
#' @param independent.variable string, name of independent variable, should
#' represent age
#' @param reference.year numeric or string, year from which to subtract year
#' of birth to calculate age (either as a constant, or the name of a variable
#' containing numeric values')
#' @param acceptable.tolerance numeric, range above or below reported age within
#' which to call reported and calculated age consistent
#' @return a vector of length nrow(phenotype.data) representing the results
#' of the dependency test
#' @export year.is.consistent.with.age
year.is.consistent.with.age <- function(dependent.variable, independent.variable,
                                        reference.year, acceptable.tolerance) {
  stopifnot(is.numeric(dependent.variable))
  stopifnot(is.numeric(independent.variable))
  stopifnot(is.numeric(reference.year), length(reference.year) > 1 |
    (!is.na(reference.year) & length(reference.year) == 1))
  stopifnot(
    is.numeric(acceptable.tolerance),
    (!is.na(acceptable.tolerance) & length(acceptable.tolerance) == 1)
  )
  stopifnot(length(dependent.variable) == length(independent.variable))
  stopifnot(length(independent.variable) >= length(reference.year))

  is.na(dependent.variable) |
    is.na(independent.variable) |
    is.na(reference.year) |
    ((reference.year - dependent.variable - acceptable.tolerance) < independent.variable &
      independent.variable < (reference.year - dependent.variable + acceptable.tolerance))
}
