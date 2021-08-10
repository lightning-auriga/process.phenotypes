#' Derive binary yes/no first-degree relatives from family member data
#'
#' @details
#' Logic to convert family member lists to binary variable indicating
#' whether or not first degree relatives are affected.  Handled through
#' rlang::eval_tidy, which provides an environment and data mask to
#' prevent overwriting of data in memory (see functions in
#' derived_variables.R).
#'
#' @description
#' This helper function allows one to convert variables that are
#' lists of family members (e.g. "father,mother,grandmother") to
#' a yes/no variable that simply reports whether first degree
#' relatives are included in the list.  We are assuming that the
#' level of granularity required for downstream analysis is more
#' along the lines of whether or not first-degree relations are
#' affected.
#'
#' @param variable.name string, name of existing variable to operate on
#' @return a vector of length nrow(phenotype.data) representing the
#' derived variable
#' @export derive.first.degree
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

#' Apply inverse/rank normal transformation to numeric variable
#'
#' @details
#' Transforms a numeric vector to a normally distributed variable
#' with certain configurable settings.
#'
#' @description
#' The outcome of this transformation is guaranteed to be normal,
#' but may not behave as you want it to. Parametric information
#' about the variable is lost. Ties are resolved by random resolution,
#' which has desirable statistical properties but which breaks down
#' (as do all methods) as the number of ties increases relative to
#' the sample size. Multiple transformations result in different
#' variables, with the extent of the differences again dependent
#' on the number of ties.
#'
#' @param variable numeric vector, input variable
#' for transformation; using tidyverse masking, this can be
#' referred to by the name of a variable in the input config
#' @param offset numeric, small decimal offset for transform
#' adjustment
#' @param stratification.vars list, input variables to be used
#' as factors for stratifying data before applying inverse
#' normal transform; using tidyverse masking, these can be
#' referred to by the names of variables in the input config
#' @param include.subjects logical vector or NA, indicator vector for
#' which subjects should be considered for current transformation
#' @param primary.call logical, whether this is the top-level
#' call of this function. do not set or change this, unless you want really
#' awful behavior
#' @export derive.rank.normal.transform
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
  if (primary.call & length(stratification.vars) > 0) {
    res <- derive.rank.normal.transform(
      res,
      offset,
      list()
    )
  }
  res
}
