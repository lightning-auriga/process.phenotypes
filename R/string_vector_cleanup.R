#' Basic global cleanup of entries in a phenotype data frame
#'
#' @details
#' Performs the following data modifications to non-numeric values:
#'
#' - convert all characters to lower case
#'
#' @description
#'
#' @param df data frame, input phenotype content
#' @return modified version of input with values cleaned as described
#' above
#' @export make.lowercase
#' @examples
#' phenotype.data <- data.frame(
#'   c("WeiRd CaPs", "Trailing ", "NA"),
#'   c(";something", "Not Applicable", "Too   muchspace")
#' )
#' colnames(phenotype.data) <- c("col1", "col2")
#' phenotype.data <- make.lowercase(phenotype.data)
make.lowercase <- function(df) {
  data.frame(lapply(df, tolower))
}
#' Basic global cleanup of entries in a phenotype data frame
#'
#' @details
#' Performs the following data modifications to non-numeric values:
#'
#' - remove all trailing and preceding whitespaces and collapse
#'   consecutive whitespaces
#'
#' @description
#'
#' @param df data frame, input phenotype content
#' @return modified version of input with values cleaned as described
#' above
#' @export remove.whitespace
#' @examples
#' phenotype.data <- data.frame(
#'   c("WeiRd CaPs", "Trailing ", "NA"),
#'   c(";something", "Not Applicable", "Too   muchspace")
#' )
#' colnames(phenotype.data) <- c("col1", "col2")
#' phenotype.data <- remove.whitespace(phenotype.data)
remove.whitespace <- function(df) {
  df <- data.frame(lapply(df, stringr::str_squish))
  data.frame(lapply(df, stringr::str_replace_all, "[ \\-]*-[ \\-]*", "-"))
}

#' Basic global cleanup of entries in a phenotype data frame
#'
#' @details
#' Performs the following data modifications to non-numeric values:
#'
#' - collapses multiple instances of specified character to a single replacement
#'
#' @description
#'
#' @param df data frame, input phenotype content
#' @param targets character vector, input character(s) to replace duplicates of
#' @param replacements character vector, character to replace target duplicates with
#' @return modified version of input with values cleaned as described
#' @export collapse.repeats
#' @examples
#' phenotype.data <- data.frame(
#'   A = c("\\/", "0..112"),
#'   B = c("//////", "0..\\//\\..01")
#' )
#' phenotype.data <- collapse.repeats(df)
collapse.repeats <- function(df, targets = c("\\\\/", "\\."), replacements = c("/", ".")) {
  stopifnot(
    is.vector(targets, mode = "character"),
    is.vector(replacements, mode = "character")
  )
  stopifnot(length(targets) == length(replacements))
  res <- df
  for (i in seq_len(length(targets))) {
    target <- targets[i]
    replacement <- replacements[i]
    pattern <- paste("[", target, "]", "+", sep = "")
    res <- data.frame(lapply(res, stringr::str_replace_all, pattern, replacement))
  }
  res
}

#' Basic global cleanup of entries in a phenotype data frame
#'
#' @details
#' Performs the following data modifications to non-numeric values:
#'
#' - remove trailing and preceding non-alphanumerics
#'
#' @description
#'
#' @param df data frame, input phenotype content
#' @return modified version of input with values cleaned as described
#' above
#' @export remove.nonword.chars
#' @examples
#' phenotype.data <- data.frame(
#'   c("WeiRd CaPs", "Trailing ", "NA"),
#'   c(";something", "Not Applicable", "Too   muchspace")
#' )
#' colnames(phenotype.data) <- c("col1", "col2")
#' phenotype.data <- remove.nonword.chars(phenotype.data)
remove.nonword.chars <- function(df) {
  df <- data.frame(lapply(df, stringr::str_replace_all, "^\\.([0-9]+)$", "0.\\1"))
  data.frame(lapply(df, stringr::str_replace_all, "^\\W+|\\W*[^[\\w)}\\]]]$", ""))
}
#' Basic global cleanup of entries in a phenotype data frame
#'
#' @details
#' Performs the following data modifications to non-numeric values:
#'
#' - harmonize na, nan, not applicable values (unknowns?  blanks?)
#'
#' @description
#'
#' Assumes input content has already had `make.lowercase` applied.
#'
#' @param df data frame, input phenotype content
#' @return modified version of input with values cleaned as described
#' above
#' @export normalize.missing.values
#' @examples
#' phenotype.data <- data.frame(
#'   c("WeiRd CaPs", "Trailing ", "NA"),
#'   c(";something", "Not Applicable", "Too   muchspace")
#' )
#' colnames(phenotype.data) <- c("col1", "col2")
#' phenotype.data <- normalize.missing.values(phenotype.data)
normalize.missing.values <- function(df) {
  data.frame(lapply(df, stringr::str_replace_all,
    "^na$|^not applicable$|^nil$|^nan$|^$",
    replacement = NA_character_
  ))
}

#' Detect whether strings look like SBP/DBP measurements
#'
#' @details
#' Blood pressure measurements may be reported as ###/###
#' entries in a single vector, complicating numeric casts.
#'
#' @description
#'
#' @param vec character vector, input candidate values
#' @param allow.trailing logical, whether a blood pressure-like
#' entry with arbitrary trailing text should be considered possibly
#' a good result
#' @return logical vector, one per input value, whether the input
#' matches blood pressure format
#' @examples
#' is.blood.pressure(c("100/80", "100/", "100/80."))
is.blood.pressure <- function(vec, allow.trailing = FALSE) {
  # TODO: Note that this currently will massage values like
  # 100/80.98181818 to 100/80, for example.  What is the desired
  # behavior in this case?  Also, this won't allow decimals in
  # systolic.
  pattern <- "^\\d+ */ *\\d+"
  if (allow.trailing) {
    pattern <- paste(pattern, ".*$", sep = "")
  } else {
    pattern <- paste(pattern, "$", sep = "")
  }
  stringr::str_detect(vec, pattern)
}

#' Aggressively convert malformed numeric vectors
#'
#' @details
#' Given string vectors with malformed numeric entries, attempt
#' to force conversion to numerics by stripping common problems.
#'
#' @description
#'
#' @param df data frame, input phenotype content
#' @param accept.proportion numeric, proportion of data to match a type
#' required to enforce the match
#' @return modified version of input with values cleaned as described above
#' @export reformat.numerics
#' @examples
#' phenotype.data <- data.frame(
#'   A = c("yes", "yes", "no", NA),
#'   B = c("1.0", "100/80", "4g", "sparse wrong value")
#' )
#' phenotype.data <- reformat.numerics(phenotype.data)
reformat.numerics <- function(df, accept.proportion = 0.75) {
  data.frame(lapply(df, function(vec) {
    ## high percentage of values begin with numeric values
    n.numeric <- length(which(!is.na(suppressWarnings(as.vector(vec, mode = "numeric")))))
    if (n.numeric / nrow(df) >= accept.proportion) {
      ## treat this as arbitrary numeric data
      ## if the prefix of a value looks like a numeric, strip its suffix
      possible.numeric <- stringr::str_detect(vec, "^-?\\d+\\.?\\d*[^/]?.*$")
      res <- rep(NA, length(vec))
      res[possible.numeric] <- stringr::str_replace(
        vec[possible.numeric],
        "^(-?\\d+\\.?\\d*)[^/]?.*$", "\\1"
      )
      as.numeric(res)
    } else {
      vec
    }
  }))
}

#' Detect and reformat blood pressure measures
#'
#' @details
#' Given string vectors with potentially malformed blood pressure
#' entries, attempt to standardize to SBP/DBP by stripping
#' common problems.
#'
#' @description
#'
#' @param df data frame, input phenotype content
#' @param accept.proportion numeric, proportion of data to match a type
#' required to enforce the match
#' @return modified version of input with values cleaned as described above
#' @export reformat.blood.pressure
#' @examples
#' phenotype.data <- data.frame(
#'   A = c("a", "100/80", "100/80.", NA),
#'   B = c("1.0", "100/80mmhg", "other", ".")
#' )
#' phenotype.data <- reformat.blood.pressure(phenotype.data)
reformat.blood.pressure <- function(df, accept.proportion = 0.75) {
  data.frame(lapply(df, function(vec) {
    n.blood.pressure <- length(which(is.blood.pressure(vec)))
    if (n.blood.pressure / nrow(df) >= accept.proportion) {
      ## treat this as BP, eliminate anything else
      ## if the prefix of a value looks like BP, strip its suffix
      possible.blood.pressure <- is.blood.pressure(vec, allow.trailing = TRUE)
      res <- rep(NA, length(vec))
      res[possible.blood.pressure] <- stringr::str_replace(
        vec[possible.blood.pressure],
        "^(\\d+) */ *(\\d+).*$",
        "\\1/\\2"
      )
      res
    } else {
      vec
    }
  }))
}
