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
#' @param independent.na.aliases character vector, define alternative values to be treated
#' as NAs in the independent variable, e.g. "0 times"
#' @param dependent.na.aliases character vector, define alternative values to be treated
#' as NAs in the dependent variable, e.g. "0 times"
#' @return a vector of length nrow(phenotype.data) representing the results
#' of the dependency test
#' @export response.depends.on.yes
response.depends.on.yes <- function(dependent.variable, independent.variable,
                                    yes.aliases = c("yes"),
                                    independent.na.aliases = character(),
                                    dependent.na.aliases = character()) {
  stopifnot(length(dependent.variable) >= 1)
  stopifnot(length(independent.variable) >= 1)
  stopifnot(is.character(yes.aliases), length(yes.aliases) > 0)
  stopifnot(length(dependent.variable) == length(independent.variable))
  stopifnot(is.character(independent.na.aliases))
  stopifnot(is.character(dependent.na.aliases))

  is.na(dependent.variable) |
    dependent.variable %in% dependent.na.aliases |
    independent.variable %in% independent.na.aliases |
    (!is.na(independent.variable) & independent.variable %in% yes.aliases)
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
#' @param independent.na.aliases character vector, define alternative values to be treated
#' as NAs in the independent variable, e.g. "0 times"
#' @param dependent.na.aliases character vector, define alternative values to be treated
#' as NAs in the dependent variable, e.g. "0 times"
#' @return a vector of length nrow(phenotype.data) representing the results
#' of the dependency test
#' @export response.depends.on.not.na
response.depends.on.not.na <- function(dependent.variable, independent.variable,
                                       independent.na.aliases = character(),
                                       dependent.na.aliases = character()) {
  stopifnot(length(dependent.variable) >= 1)
  stopifnot(length(independent.variable) >= 1)
  stopifnot(length(dependent.variable) == length(independent.variable))
  stopifnot(is.character(independent.na.aliases))
  stopifnot(is.character(dependent.na.aliases))

  is.na(dependent.variable) |
    dependent.variable %in% dependent.na.aliases |
    !(is.na(independent.variable) |
      independent.variable %in% independent.na.aliases)
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

#' Test consistency of BMI with height and weight
#'
#' @description
#' This dependency will enforce BMI ~= weight/height^2,
#' with weight in kilograms and height in meters, within
#' a certain absolute tolerance.
#'
#' @details
#' Large numbers of deviations are expected when the
#' reported BMI has been arbitrarily entered by respondents
#' and not computed directly from the source data.
#'
#' @param bmi numeric vector, reported BMI
#' @param weight numeric vector, reported weight
#' @param height numeric vector, reported height
#' @param tolerance numeric, acceptable deviation
#' between computed and reported BMI
#' @return a vector of length nrow(phenotype.data) representing
#' the results of the dependency test
#' @export response.is.computed.bmi
response.is.computed.bmi <- function(bmi, weight, height, tolerance) {
  stopifnot(is.numeric(bmi))
  stopifnot(is.numeric(weight))
  stopifnot(is.numeric(height))
  stopifnot(
    length(bmi) == length(weight),
    length(bmi) == length(height)
  )
  stopifnot(
    is.numeric(tolerance),
    length(tolerance) == 1
  )
  is.na(bmi) | is.na(height) | is.na(weight) |
    abs(weight / height^2 - bmi) <= tolerance
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

#' Evaluate relative magnitude of variables
#'
#' @param larger.var numeric vector, expected to be larger
#' @param smaller.var numeric vector, expected to be smaller
#' @param allow.equal logical, whether equality is permitted
#' @return logical vector of same length as input vectors, denoting
#' whether first vector values are larger than second's
response.greater.than <- function(larger.var, smaller.var, allow.equal = TRUE) {
  stopifnot(
    is.numeric(larger.var),
    is.numeric(smaller.var),
    length(larger.var) == length(smaller.var)
  )
  if (allow.equal) {
    is.na(larger.var) | is.na(smaller.var) | larger.var >= smaller.var
  } else {
    is.na(larger.var) | is.na(smaller.var) | larger.var > smaller.var
  }
}
