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
    paste(
      "^n[/]?a$",
      "^not a[pl]+ica[ble]+$",
      "^nil$",
      "^$",
      "^not sure$",
      "^nan$",
      "^not a[s]+e[s]+ed$",
      "^unav[ai]+la[bl]+e$",
      "^none$",
      "^u[nk]+[ow]+n$",
      "^not av[ai]+la[bl]+e$",
      "^not done$",
      sep = "|"
    ),
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

#' Aggressively convert malformed numeric vector
#'
#' @details
#' Given string vector with malformed numeric entries, attempt
#' to force conversion to numerics by stripping common problems.
#'
#' @description
#'
#' @param vec character vector, input phenotype content
#' @param var.summary list, variable summary entry for this particular variable
#' @return modified version of input with values cleaned as described above
#' @export reformat.numerics
reformat.numerics <- function(vec, var.summary) {
  ## treat this as arbitrary numeric data
  ## if the prefix of a value looks like a numeric, strip its suffix
  possible.numeric <- stringr::str_detect(vec, "^-?\\d+\\.?\\d*($| *[^ \\d/].*$)") & !is.na(vec)
  res <- rep(NA, length(vec))
  res[possible.numeric] <- stringr::str_replace(
    vec[possible.numeric],
    "^(-?\\d+\\.?\\d*)($| *[^ \\d/].*$)", "\\1"
  )
  res <- as.numeric(res)
  var.summary$invalid.numeric.entries <- vec[!possible.numeric & !is.na(vec)]
  list(phenotype.data = res, variable.summary = var.summary)
}

#' Detect and reformat blood pressure measures
#'
#' @details
#' Given string vector with potentially malformed blood pressure
#' entries, attempt to standardize to SBP/DBP by stripping
#' common problems.
#'
#' @description
#'
#' @param vec character vector, input phenotype content
#' @param var.summary list, variable summary information for this particular variable
#' @return modified version of input with values cleaned as described above
#' @export reformat.blood.pressure
reformat.blood.pressure <- function(vec, var.summary) {
  ## treat this as BP, eliminate anything else
  ## if the prefix of a value looks like BP, strip its suffix
  possible.blood.pressure <- is.blood.pressure(vec, allow.trailing = TRUE) & !is.na(vec)
  res <- rep(NA, length(vec))
  res[possible.blood.pressure] <- stringr::str_replace(
    vec[possible.blood.pressure],
    "^(\\d+) */ *(\\d+).*$",
    "\\1/\\2"
  )
  var.summary$invalid.blood.pressure.entries <- vec[!possible.blood.pressure & !is.na(vec)]
  list(phenotype.data = res, variable.summary = var.summary)
}

#' Convert character vector to factor, collapsing certain levels
#'
#' @details
#' Converts character vector to factor based on level specification in yaml config.
#' This will report entries that are converted to NA without first being set to NA intentionally.
#'
#' @description
#'
#' @param vec character vector, input string phenotype data
#' @param variable.summary list, summary information for a given variable
#' @return list, 'phenotype.data' contains converted phenotype information,
#' 'variable.summary' contains input variable summary and possibly information
#' about invalid factor levels converted to NA.
#' @export reformat.factor
reformat.factor <- function(vec, variable.summary) {
  stopifnot(!is.null(variable.summary$params$levels))
  ordered.levels <- c()
  for (level in variable.summary$params$levels) {
    stopifnot(!is.null(level$name))
    alternates <- level$alternate_patterns
    if (!is.null(alternates)) {
      ## replace alternates
      vec <- stringr::str_replace(
        vec,
        paste(paste("^", alternates, "$", sep = ""),
          collapse = "|"
        ),
        level$name
      )
    }
    ordered.levels <- c(ordered.levels, level$name)
  }
  res <- factor(vec, levels = ordered.levels)
  variable.summary$invalid.factor.entries <- unique(vec[!is.na(vec) & is.na(res)])
  list(
    phenotype.data = res,
    variable.summary = variable.summary
  )
}
