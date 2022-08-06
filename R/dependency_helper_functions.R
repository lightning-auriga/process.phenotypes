#' Test dependency of one variable on "yes" response of another
#'
#' @description
#' This is a function designed for use within the user configuration
#' file, as a convenience to handle structured comparison between
#' a yes/no style question and its dependent downstream response.
#' In the formal parameters, `dependent` refers to the logically
#' downstream question, and `independent` refers to the upstream
#' question on which the `dependent` question depends.
#'
#' Note that the input vectors are specified as _names of variables_
#' within the input phenotype dataset. At the time the dependencies
#' are evaluated, the phenotype dataset has had the user-configured
#' variable names applied as its column headers, and as such the
#' variables should be referred to by those labels directly.
#'
#' @details
#' This dependency test assumes the input vectors are factors or character vectors.
#' Though the function is modeled on the idea of a yes/no response,
#' the definition of `yes` can be expanded in the `yes.aliases` parameter.
#' Thus, this function will also serve if the independent question is,
#' for example: `How often do you smoke? [daily/sometimes/never]`, and
#' the response set that triggers the downstream question for respondents
#' who smoke at least some amount. In this case, setting `yes.aliases`
#' to `c("daily", "sometimes") triggers the correct dependency test.
#'
#' @param dependent.variable Character vector name of dependent/upstream variable to operate on
#' within the package's representation of the input phenotype data.
#' @param independent.variable Character vector name of independent/downstream variable to operate on
#' within the package's representation of the input phenotype data.
#' @param yes.aliases Character vector values to be interpreted as `yes`
#' in the independent variable.
#' @param independent.na.aliases Character vector alternative values to be treated
#' as NAs in the independent variable. For example, depending on the question,
#' `0 times` or `none` could be considered non-response.
#' @param dependent.na.aliases Character vector alternative values to be treated
#' as NAs in the dependent variable. For example, depending on the question,
#' `never` or `none` could be considered non-response.
#' @return Logical vector representing the results
#' of the dependency test between the two provided vectors. Test
#' represents whether the specified dependency is satisfied.
#' @export response.depends.on.yes
#' @seealso response.depends.on.not.na,
#' response.is.less.than, response.is.greater.than,
#' response.is.duplicate.of, response.is.computed.bmi,
#' year.is.consistent.with.age
#' @examples
#' indep.var <- c("yes", "sometimes", "no response", "yes", "never")
#' dep.var <- 0:4
#' phenotype.data <- data.frame(
#'   HW00001 = indep.var,
#'   HW00002 = dep.var
#' )
#' ## this function is designed as a utility to be
#' ## deployed in the dependency block of a user configuration
#' ## file. it requires the bindings of the phenotype data
#' ## matrix to be available in the current context
#' \dontrun{
#' dep.test <- response.depends.on.yes(
#'   HW00002,
#'   HW00001,
#'   c("sometimes"),
#'   c("no response"),
#'   c("0")
#' )
#' }
#' ## expected: TRUE TRUE FALSE TRUE FALSE
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

#' Test dependency of one variable on non-NA response of another.
#'
#' @description
#' This is a function designed for use within the user configuration
#' file, as a convenience to handle structured comparison between
#' a question and its dependent downstream response.
#' In the formal parameters, `dependent` refers to the logically
#' downstream question, and `independent` refers to the upstream
#' question on which the `dependent` question depends.
#'
#' Note that the input vectors are specified as _names of variables_
#' within the input phenotype dataset. At the time the dependencies
#' are evaluated, the phenotype dataset has had the user-configured
#' variable names applied as its column headers, and as such the
#' variables should be referred to by those labels directly.
#'
#' @details
#' This dependency test assumes the input vectors are factors or character vectors.
#' This is a very general test that supposes that anyone who responds
#' at all to the upstream question is a valid respondent for the downstream
#' one. The definition of `NA` can be expanded for each compared variable.
#' In this dependency test, an `NA` response in the independent variable
#' indicates a subject that should never provide a non-`NA` response
#' in the dependent variable; an `NA` response in the dependent variable
#' indicates a subject that did not in fact provide a response in the dependent variable.
#'
#' @param dependent.variable Character vector name of dependent/upstream variable to operate on
#' within the package's representation of the input phenotype data.
#' @param independent.variable Character vector name of independent/downstream variable to operate on
#' within the package's representation of the input phenotype data.
#' @param independent.na.aliases Character vector alternative values to be treated
#' as NAs in the independent variable. For example, depending on the question,
#' `0 times` or `none` could be considered non-response.
#' @param dependent.na.aliases Character vector alternative values to be treated
#' as NAs in the dependent variable. For example, depending on the question,
#' `never` or `none` could be considered non-response.
#' @return Logical vector representing the results
#' of the dependency test between the two provided vectors. Test
#' represents whether the specified dependency is satisfied.
#' @seealso response.depends.on.yes
#' @export response.depends.on.not.na
#' @seealso response.depends.on.yes,
#' response.is.less.than, response.is.greater.than,
#' response.is.duplicate.of, response.is.computed.bmi
#' year.is.consistent.with.age
#' @examples
#' indep.var <- c("yes", "sometimes", "no response", "yes", "never")
#' dep.var <- 0:4
#' phenotype.data <- data.frame(
#'   HW00001 = indep.var,
#'   HW00002 = dep.var
#' )
#' ## this function is designed as a utility to be
#' ## deployed in the dependency block of a user configuration
#' ## file. it requires the bindings of the phenotype data
#' ## matrix to be available in the current context
#' \dontrun{
#' dep.test <- response.depends.on.not.na(
#'   HW00002,
#'   HW00001,
#'   c("no response"),
#'   c("0")
#' )
#' }
#' ## expected: TRUE TRUE FALSE TRUE TRUE
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

#' Test that one variable is less than the other.
#'
#' @description
#' This is a function designed for use within the user configuration
#' file, as a convenience to handle structured comparison between
#' a question and its dependent downstream response.
#' In the formal parameters, `dependent` refers to the logically
#' downstream question, and `independent` refers to the upstream
#' question on which the `dependent` question depends.
#'
#' Note that the input vectors are specified as _names of variables_
#' within the input phenotype dataset. At the time the dependencies
#' are evaluated, the phenotype dataset has had the user-configured
#' variable names applied as its column headers, and as such the
#' variables should be referred to by those labels directly.
#'
#' @details
#' This dependency test assumes the input vectors are numeric or support '<'.
#' For the purposes of this comparison, NA response in either independent
#' or dependent variable leads to the dependency test returning success;
#' the goal of the function is to flag explicit situations when the
#' dependency definitely fails.
#'
#' @param dependent.variable Character vector name of dependent/upstream variable to operate on
#' within the package's representation of the input phenotype data. In this context,
#' this is the variable that is expected to be less than the other.
#' @param independent.variable Character vector name of independent/downstream variable to operate on
#' within the package's representation of the input phenotype data. In this context,
#' this is the variable that is expected to be greater than the other.
#' @return Logical vector representing the results
#' of the dependency test between the two provided vectors. Test
#' represents whether the specified dependency is satisfied.
#' @export response.is.less.than
#' @seealso response.depends.on.yes, response.depends.on.not.na,
#' response.is.greater.than,
#' response.is.duplicate.of, response.is.computed.bmi
#' year.is.consistent.with.age
#' @examples
#' indep.var <- c(1:5, NA, 5)
#' dep.var <- c(5:1, 1, NA)
#' phenotype.data <- data.frame(
#'   HW00001 = indep.var,
#'   HW00002 = dep.var
#' )
#' ## this function is designed as a utility to be
#' ## deployed in the dependency block of a user configuration
#' ## file. it requires the bindings of the phenotype data
#' ## matrix to be available in the current context
#' \dontrun{
#' dep.test <- response.is.less.than(
#'   HW00002,
#'   HW00001
#' )
#' }
#' ## expected: FALSE FALSE FALSE TRUE TRUE TRUE TRUE
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

#' @title
#' Test concordance of duplicate variables.
#'
#' @description
#' This is a function designed for use within the user configuration
#' file, as a convenience to handle structured comparison between
#' a question and its dependent downstream response.
#' In the formal parameters, `dependent` refers to the logically
#' downstream question, and `independent` refers to the upstream
#' question on which the `dependent` question depends.
#'
#' Note that the input vectors are specified as _names of variables_
#' within the input phenotype dataset. At the time the dependencies
#' are evaluated, the phenotype dataset has had the user-configured
#' variable names applied as its column headers, and as such the
#' variables should be referred to by those labels directly.
#'
#' @details
#' This dependency test only assumes the input vectors can be compared with
#' the equality operator.
#' This function tests that two non-NA values are concordant.  Note
#' that non-NA/NA mismatches are allowed here, to focus on the most
#' toxic discordant cases.
#'
#' @param dependent.variable Character vector name of dependent/upstream variable to operate on
#' within the package's representation of the input phenotype data.
#' @param independent.variable Character vector name of independent/downstream variable to operate on
#' within the package's representation of the input phenotype data.
#' @return Logical vector representing the results
#' of the dependency test between the two provided vectors. Test
#' represents whether the specified dependency is satisfied.
#' @export response.is.duplicate.of
#' @seealso response.depends.on.yes, response.depends.on.not.na,
#' response.is.less.than, response.is.greater.than,
#' response.is.computed.bmi
#' year.is.consistent.with.age
#' @examples
#' indep.var <- c("yes", "sometimes", "no response", NA, "never")
#' dep.var <- c("yes", "sometimes", NA, "sometimes", "sometimes")
#' phenotype.data <- data.frame(
#'   HW00001 = indep.var,
#'   HW00002 = dep.var
#' )
#' ## this function is designed as a utility to be
#' ## deployed in the dependency block of a user configuration
#' ## file. it requires the bindings of the phenotype data
#' ## matrix to be available in the current context
#' \dontrun{
#' dep.test <- response.is.duplicate.of(
#'   HW00002,
#'   HW00001
#' )
#' }
#' ## expected: TRUE TRUE TRUE TRUE FALSE
response.is.duplicate.of <- function(dependent.variable, independent.variable) {
  stopifnot(length(dependent.variable) == length(independent.variable))
  stopifnot(class(dependent.variable) == class(independent.variable))

  is.na(dependent.variable) |
    is.na(independent.variable) |
    dependent.variable == independent.variable
}

#' @title
#' Test consistency of BMI with height and weight.
#'
#' @description
#' This is a function designed for use within the user configuration
#' file, as a convenience to handle structured comparison between
#' a question and its dependent downstream response.
#' In the formal parameters, `dependent` refers to the logically
#' downstream question, and `independent` refers to the upstream
#' question on which the `dependent` question depends.
#'
#' Note that the input vectors are specified as _names of variables_
#' within the input phenotype dataset. At the time the dependencies
#' are evaluated, the phenotype dataset has had the user-configured
#' variable names applied as its column headers, and as such the
#' variables should be referred to by those labels directly.
#'
#' @details
#' This dependency test assumes the input vectors are numeric.
#' This dependency will enforce BMI ~= weight/height^2,
#' with weight in kilograms and height in meters, within
#' a certain absolute tolerance.
#' Large numbers of deviations are expected when the
#' reported BMI has been arbitrarily entered by respondents
#' and not computed directly from the source data. If sufficiently
#' many dependency failures are encountered, consider either
#' generating a derived variable that computes BMI in precisely
#' the manner you prefer, or adjusting the tolerance of this test.
#'
#' As the objective of the function is to flag conclusive dependency
#' failures, subjects with NA responses for any of BMI, height, or weight
#' are considered to pass this dependency test.
#'
#' @param bmi Character vector name of numeric vector of reported BMI.
#' @param weight Character vector name of numeric vector of
#' reported weight in kilograms.
#' @param height Character vector name of numeric vector of
#' reported height in meters.
#' @param tolerance Numeric acceptable absolute deviation
#' between computed and reported BMI.
#' @return Logical vector representing the results
#' of the dependency test between the two provided vectors. Test
#' represents whether the specified dependency is satisfied.
#' @export response.is.computed.bmi
#' @seealso response.depends.on.yes, response.depends.on.not.na,
#' response.is.less.than, response.is.greater.than,
#' response.is.duplicate.of,
#' year.is.consistent.with.age
#' @examples
#' bmi <- c(35, 54, 40, 40, NA)
#' height <- c(1.5, 1.7, NA, 1.23, 1.7)
#' weight <- c(80, 90, 70, NA, 90)
#' phenotype.data <- data.frame(
#'   HW00001 = bmi,
#'   HW00002 = height,
#'   HW00003 = weight
#' )
#' ## this function is designed as a utility to be
#' ## deployed in the dependency block of a user configuration
#' ## file. it requires the bindings of the phenotype data
#' ## matrix to be available in the current context
#' \dontrun{
#' dep.test <- response.is.computed.bmi(
#'   HW00001,
#'   HW00003,
#'   HW00002,
#'   1.0
#' )
#' }
#' ## expected: TRUE FALSE TRUE TRUE TRUE
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

#' @title
#' Test consistency of age and year of birth.
#'
#' @description
#' This is a function designed for use within the user configuration
#' file, as a convenience to handle structured comparison between
#' a question and its dependent downstream response.
#' In the formal parameters, `dependent` refers to the logically
#' downstream question, and `independent` refers to the upstream
#' question on which the `dependent` question depends.
#'
#' Note that the input vectors are specified as _names of variables_
#' within the input phenotype dataset. At the time the dependencies
#' are evaluated, the phenotype dataset has had the user-configured
#' variable names applied as its column headers, and as such the
#' variables should be referred to by those labels directly.
#'
#' @details
#' This dependency test assumes the input vectors are numeric.
#' This function tests that reported and calculated age is within a certain
#' user-defined absolute tolerance.
#' Large numbers of deviations are sometimes apparent and are often
#' correlated with approximate generation of respondent. Sufficiently
#' many deviations may indicate delinking between the two dependent variables.
#' In our observations, self-reported age tends to be more reliable,
#' but that will likely vary by dataset.
#'
#' Note that while this was originally designed for testing age at collection
#' versus date of birth, it can also be
#' used for other age comparisons, e.g. age at diagnosis vs. date of diagnosis.
#'
#' The reference year, used to compute age from reported date, is generally
#' the approximate date of data collection. This can either be provided as
#' a numeric constant, or as another input vector with a potentially variable
#' entry per subject.
#'
#' As the objective of the function is to flag conclusive dependency
#' failures, subjects with NA responses for any of reported year, reported
#' age, or reference year are considered to pass this dependency test.
#'
#' @param reported.year Character vector name of date variable, for example
#' date of birth.
#' @param reported.age Character vector name of age variable, for example
#' self-reported age at data collection.
#' @param reference.year Either character vector name of reference year variable
#' indicating a potentially different reference year per subject, or a numeric
#' reference year to be used to compute age for all subjects.
#' @param acceptable.tolerance Numeric absolute range above or below reported age
#' within which to call reported and calculated age consistent.
#' @return Logical vector representing the results
#' of the dependency test between the two provided vectors. Test
#' represents whether the specified dependency is satisfied.
#' @export year.is.consistent.with.age
#' @seealso response.depends.on.yes, response.depends.on.not.na,
#' response.is.less.than, response.is.greater.than,
#' response.is.duplicate.of, response.is.computed.bmi
#' @examples
#' reported.year <- c(1985, 1990, 1995, 2000, NA)
#' reported.age <- c(10, 20, 25, NA, 30)
#' reference.year <- c(1995, 2020, NA, 2015, 2010)
#' phenotype.data <- data.frame(
#'   HW00001 = reported.year,
#'   HW00002 = reported.age,
#'   HW00003 = reference.age
#' )
#' ## this function is designed as a utility to be
#' ## deployed in the dependency block of a user configuration
#' ## file. it requires the bindings of the phenotype data
#' ## matrix to be available in the current context
#' \dontrun{
#' dep.test <- year.is.consistent.with.age(
#'   HW00001,
#'   HW00002,
#'   HW00003,
#'   4.0
#' )
#' }
#' ## expected: TRUE FALSE TRUE TRUE TRUE
#'
#' ## if preferred, use a constant reference year for all subjects
#' \dontrun{
#' dep.test <- year.is.consistent.with.age(
#'   HW00001,
#'   HW00002,
#'   2022,
#'   1.0
#' )
#' }
#' ## expected: FALSE FALSE FALSE TRUE TRUE
year.is.consistent.with.age <- function(reported.year, reported.age,
                                        reference.year, acceptable.tolerance) {
  stopifnot(is.numeric(reported.year))
  stopifnot(is.numeric(reported.age))
  stopifnot(is.numeric(reference.year), length(reference.year) > 1 |
    (!is.na(reference.year) & length(reference.year) == 1))
  stopifnot(
    is.numeric(acceptable.tolerance),
    (!is.na(acceptable.tolerance) & length(acceptable.tolerance) == 1)
  )
  stopifnot(length(reported.year) == length(reported.age))
  stopifnot(length(reported.year) >= length(reference.year))

  is.na(reported.year) |
    is.na(reported.age) |
    is.na(reference.year) |
    ((reference.year - reported.year - acceptable.tolerance) < reported.age &
      reported.age < (reference.year - reported.year + acceptable.tolerance))
}

#' Test that one variable is greater than, or possibly equal to, the other.
#'
#' @description
#' This is a function designed for use within the user configuration
#' file, as a convenience to handle structured comparison between
#' a question and its dependent downstream response.
#' In the formal parameters, `dependent` refers to the logically
#' downstream question, and `independent` refers to the upstream
#' question on which the `dependent` question depends.
#'
#' Note that the input vectors are specified as _names of variables_
#' within the input phenotype dataset. At the time the dependencies
#' are evaluated, the phenotype dataset has had the user-configured
#' variable names applied as its column headers, and as such the
#' variables should be referred to by those labels directly.
#'
#' @details
#' This dependency test assumes the input vectors are numeric or support '>',
#' or optionally '>='.
#' For the purposes of this comparison, NA response in either independent
#' or dependent variable leads to the dependency test returning success;
#' the goal of the function is to flag explicit situations when the
#' dependency definitely fails.
#'
#' @param dependent.variable Character vector name of dependent/upstream variable to operate on
#' within the package's representation of the input phenotype data. In this context,
#' this is the variable that is expected to be greater than the other.
#' @param independent.variable Character vector name of independent/downstream variable to operate on
#' within the package's representation of the input phenotype data. In this context,
#' this is the variable that is expected to be less than the other.
#' @param allow.equal Logical indicating whether equal values should be considered to pass.
#' @return Logical vector representing the results
#' of the dependency test between the two provided vectors. Test
#' represents whether the specified dependency is satisfied.
#' @export response.is.greater.than
#' @seealso response.depends.on.yes, response.depends.on.not.na,
#' response.is.less.than,
#' response.is.duplicate.of, response.is.computed.bmi
#' year.is.consistent.with.age
#' @examples
#' indep.var <- c(1:5, NA, 5)
#' dep.var <- c(5:1, 1, NA)
#' phenotype.data <- data.frame(
#'   HW00001 = indep.var,
#'   HW00002 = dep.var
#' )
#' ## this function is designed as a utility to be
#' ## deployed in the dependency block of a user configuration
#' ## file. it requires the bindings of the phenotype data
#' ## matrix to be available in the current context
#' \dontrun{
#' dep.test <- response.is.less.than(
#'   HW00002,
#'   HW00001,
#'   TRUE
#' )
#' }
#' ## expected: TRUE TRUE TRUE FALSE FALSE TRUE TRUE
response.is.greater.than <- function(dependent.variable,
                                     independent.variable,
                                     allow.equal = TRUE) {
  stopifnot(
    is.numeric(dependent.variable),
    is.numeric(independent.variable),
    length(dependent.variable) == length(independent.variable)
  )
  if (allow.equal) {
    is.na(dependent.variable) | is.na(independent.variable) |
      dependent.variable >= independent.variable
  } else {
    is.na(dependent.variable) | is.na(independent.variable) |
      dependent.variable > independent.variable
  }
}
