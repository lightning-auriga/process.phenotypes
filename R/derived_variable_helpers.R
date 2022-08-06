#' @title
#' Derive binary yes/no first-degree relatives from family member data.
#'
#' @description
#' This logic converts family member lists to a binary variable indicating
#' whether or not first degree relatives are affected.
#'
#' Note that the input vectors are specified as _names of variables_
#' within the input phenotype dataset. At the time the dependencies
#' are evaluated, the phenotype dataset has had the user-configured
#' variable names applied as its column headers, and as such the
#' variables should be referred to by those labels directly.
#'
#' @details
#' This helper function allows one to convert variables that are
#' lists of family members (e.g. "father,mother,grandmother") to
#' a yes/no variable that simply reports whether first degree
#' relatives are included in the list.  We are assuming that the
#' level of granularity required for downstream analysis is more
#' along the lines of whether or not first-degree relations are
#' affected.
#'
#' @param variable.name Character vector to be parsed.
#' This function is intended to be called from the `derived` variable
#' block of a dataset configuration file, in which case the mapping keys
#' under `variables` and `derived` (e.g. HW00001) can be called directly.
#' @return Character vector of yes/no responses indicating whether the
#' input vector implicated a first degree relationship.
#' @export derive.first.degree
#' @seealso create.derived.variables
#' @examples
#' data <- c("mother", "grandfather", "child", "stepbrother", NA)
#' phenotype.data <- data.frame(HW00001 = data)
#' der.var <- derive.first.degree(phenotype.data$HW00001)
#' ## this function is designed as a utility to be
#' ## deployed in the dependency block of a user configuration
#' ## file. it requires the bindings of the phenotype data
#' ## matrix to be available in the current context
#' \dontrun{
#' derive.first.degree(HW00001)
#' }
#' ## expected: TRUE FALSE TRUE FALSE FALSE
derive.first.degree <- function(variable.name) {
  first.degree <- paste("^mother$", "^father$", "^brother$",
    "^sister$", "^sibling['s]*$", "^parent['s]*$",
    "^son['s]*$", "^daughter['s]*$", "^child[ren]*$",
    sep = "|"
  )
  pat <- (sapply(
    strsplit(variable.name, ":|,|;"),
    function(i) {
      any(stringr::str_detect(stringr::str_replace_all(i, "\\s", ""), first.degree))
    }
  ))
  derived.variable <- rep(NA, length(variable.name))
  derived.variable[!is.na(variable.name) & pat] <- "yes"
  derived.variable[!is.na(variable.name) & !pat] <- "no"
  derived.variable
}

#' @title
#' Apply inverse/rank normal transformation to numeric variable.
#'
#' @description
#' Transforms a numeric vector to a normally distributed variable
#' with certain configurable settings.
#'
#' @details
#' The outcome of this transformation is guaranteed to be normal,
#' but may not behave as you want it to. Parametric information
#' about the variable is lost. Ties are resolved by random resolution,
#' which has desirable statistical properties but which breaks down
#' (as do all methods) as the number of ties increases relative to
#' the sample size. Multiple transformations result in different
#' variables, with the extent of the differences again dependent
#' on the number of ties.
#'
#' We really seek to emphasize here: the presence of an abundance
#' of ties will cause this function to have extremely undesirable
#' properties. Please use this with caution.
#'
#' The corresponding unit test is only probabilistically passing,
#' as it uses the Shapiro p-value to determine success. Rerunning
#' the test even a single time should generally do the trick. We
#' may reduce the stringency of the test p-value cutoff to prevent
#' sporadic issues.
#'
#' This function was originally intended for use in the derived
#' variable block of the input configuration file. In practice,
#' you may find that the intention is to merge your cleaned phenotype
#' data with other sources. Depending on the context, it may or
#' may not be appropriate to wait to apply the inverse normal transform
#' until after the merge operation is complete. Due to the complexity
#' of making sure variables are actually compatible with one another,
#' merging is not directly supported in this library. However,
#' the shared models system was created with the intention of
#' making data merging possible when appropriate, and should
#' be favored if possible.
#'
#' @param variable Numeric vector to be transformed.
#' This function is intended to be called from the `derived` variable
#' block of a dataset configuration file, in which case the mapping keys
#' under `variables` and `derived` (e.g. HW00001) can be called directly.
#' @param offset Numeric small decimal offset for transform
#' adjustment. Generally should be \[0.3, 0.5\].
#' @param stratification.vars List of input variables from package's
#' representation of the input phenotype data to be used
#' as factors for stratifying data before applying inverse
#' normal transform. The most common variable specified here
#' would be binary genetic sex, as applicable and with appropriate caveats.
#' @param include.subjects Logical vector or NA. If an indicator vector,
#' this indicates which subjects should be considered for current transformation.
#' This parameter is part of recursive transformation, and generally
#' should not be called by the user. NA indicates all subjects should
#' be considered in the current transformation.
#' @param primary.call Logical indicating whether this is the top-level
#' call of this function. Do not set or change this, unless you want really
#' awful behavior.
#' @export derive.rank.normal.transform
#' @examples
#' numeric.data <- runif(1000, 0, 1)
#' strat <- sample(c("yes", "no"), 1000, replace = TRUE)
#' phenotype.data <- data.frame(
#'   HW00001 = numeric.data,
#'   HW00002 = factor(strat)
#' )
#' der.var <- derive.rank.normal.transform(phenotype.data$HW00001,
#'   stratification = list(phenotype.data$HW00002)
#' )
#' ## this function is designed as a utility to be
#' ## deployed in the dependency block of a user configuration
#' ## file. it requires the bindings of the phenotype data
#' ## matrix to be available in the current context
#' \dontrun{
#' derive.rank.normal.transform(HW00001, stratification = list(HW00002))
#' }
derive.rank.normal.transform <- function(variable,
                                         offset = 0.5,
                                         stratification.vars = list(),
                                         include.subjects = NA,
                                         primary.call = TRUE) {
  stopifnot(is.vector(variable, mode = "numeric"))
  stopifnot(
    is.numeric(offset),
    length(offset) == 1
  )
  stopifnot(is.list(stratification.vars) | length(stratification.vars) == 0)
  for (var in stratification.vars) {
    stopifnot(is.factor(var))
  }
  stopifnot((is.vector(include.subjects, mode = "logical") & length(include.subjects) == length(variable)) |
    isTRUE(all.equal(is.na(include.subjects), rep(TRUE, length(include.subjects)))))
  stopifnot(
    is.logical(primary.call),
    length(primary.call) == 1
  )
  ## if the user hasn't specified an inclusion set, specify all subjects
  if (isTRUE(all.equal(is.na(include.subjects), rep(TRUE, length(include.subjects))))) {
    include.subjects <- rep(TRUE, length(variable))
  }
  ## if there are stratification variables left, recurse
  res <- rep(NA, length(variable))
  if (length(stratification.vars) > 0) {
    ## just stratify by the first one; leave the rest to recursion
    for (lvl in levels(stratification.vars[[1]])) {
      subset.inclusion <- include.subjects & stratification.vars[[1]] == lvl & !is.na(stratification.vars[[1]])
      recurse.result <- derive.rank.normal.transform(
        variable,
        offset,
        stratification.vars[-1],
        subset.inclusion,
        FALSE
      )
      res[subset.inclusion & is.na(res)] <- recurse.result[subset.inclusion & is.na(res)]
    }
  } else {
    if (length(which(!is.na(variable[include.subjects]))) == 0) {
      ## all included subjects are NA, set them to NA
      res[include.subjects] <- rep(NA, length(which(include.subjects)))
    } else {
      ## some included subjects are not NA, transform them
      res[include.subjects] <- qnorm((rank(variable[include.subjects],
        na.last = "keep",
        ties.method = "random"
      ) - offset) /
        sum(!is.na(variable[include.subjects])))
    }
  }
  ## if stratification was requested, run a final pass to ensure
  ## the overlaid distributions behave correctly
  if (primary.call && length(stratification.vars) > 0) {
    res <- derive.rank.normal.transform(
      res,
      offset,
      list()
    )
  }
  res
}
