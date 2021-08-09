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
