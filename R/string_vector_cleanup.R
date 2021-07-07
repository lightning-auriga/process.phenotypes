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
#' @param variable.summary list, variable-specific configuration data
#' @return modified version of input with values cleaned as described
#' above
#' @examples
#' phenotype.data <- data.frame(
#'   c("WeiRd CaPs", "Trailing ", "NA"),
#'   c(";something", "Not Applicable", "Too   muchspace")
#' )
#' colnames(phenotype.data) <- c("col1", "col2")
#' phenotype.data <- remove.nonword.chars(phenotype.data)
remove.nonword.chars <- function(df, variable.summary) {
  for (i in seq_len(ncol(df))) {
    if (is.null(variable.summary$variables[[i]]$params$type)) next
    if (grepl("string", variable.summary$variables[[i]]$params$type)) next
    ## prefix entries starting with decimal places with the placeholder 0
    df[, i] <- stringr::str_replace_all(df[, i], "^\\.([0-9]+)$", "0.\\1")
    ## replace leading ">/<" characters meaning "greater/less than" with words
    df[, i] <- stringr::str_replace_all(df[, i], "^ *> *(\\w+)", "greater than \\1")
    df[, i] <- stringr::str_replace_all(df[, i], "^ *< *(\\w+)", "less than \\1")
    ## take anything that looks like a negative number and cast it into the void
    df[, i] <- stringr::str_replace_all(df[, i], "^ *- *[0-9]+.*$", "na")
    ## strip nonword characters from extremes
    df.orig <- df[, i]
    df[, i] <- stringr::str_replace_all(df[, i], "^\\W+|\\W*[^[\\w)}\\]]]$", "")
    ## TODO: remove crappy logging
    diff.data <- cbind(
      df.orig[df[, i] != df.orig],
      df[df[, i] != df.orig, i]
    )
    if (nrow(diff.data) > 0) {
      print(diff.data)
    }
  }
  df
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
      "^not a[pl]+ica[ble]+[,:;]*$",
      "^nil$",
      "^$",
      "^not sure$",
      "^nan$",
      "^not a[s]+e[s]+ed$",
      "^unav[ai]+la[bl]+e$",
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
  variable.summary$invalid.factor.entries <- vec[!is.na(vec) & is.na(res)]
  list(
    phenotype.data = res,
    variable.summary = variable.summary
  )
}

#' Convert sporadic Unicode-only characters into lesser equivalents
#'
#' @details
#' Sporadic Unicode characters are being inserted into the dataset by
#' upstream text editors. This function attempts to take them right
#' back out again. Note that the replacements should be lowercase
#' to be consistent with other string processing functions.
#'
#' @description
#' Note that there's something particularly strange going on with
#' Excel's "=#ERROR!" code, as in one instance it's getting converted
#' into, of all things, an emoji.
#'
#' @param phenotype.data data.frame, input phenotype data
#' @return data.frame input data with Unicode characters converted
#' into more manageable equivalents.
process.unicode.characters <- function(phenotype.data) {
  for (i in seq_len(ncol(phenotype.data))) {
    phenotype.data[, i] <- stringr::str_replace_all(phenotype.data[, i], "\U00B1", "+/-")
    phenotype.data[, i] <- stringr::str_replace_all(phenotype.data[, i], "\U2192", "->")
    phenotype.data[, i] <- stringr::str_replace_all(phenotype.data[, i], "\U1F645", "#error!")
    phenotype.data[, i] <- stringr::str_replace_all(phenotype.data[, i], "\U00B0", "degrees")
    phenotype.data[, i] <- stringr::str_replace_all(phenotype.data[, i], "\U2018|\U2019", "'")
    phenotype.data[, i] <- stringr::str_replace_all(phenotype.data[, i], "\U201C|\U201D", "\"")
    phenotype.data[, i] <- stringr::str_replace_all(phenotype.data[, i], "\U00B2", "2")
    phenotype.data[, i] <- stringr::str_replace_all(phenotype.data[, i], "\U00B9", "1")
    phenotype.data[, i] <- stringr::str_replace_all(phenotype.data[, i], "\U2715", "x")
    phenotype.data[, i] <- stringr::str_replace_all(phenotype.data[, i], "\U00E7", "c")
    phenotype.data[, i] <- stringr::str_replace_all(phenotype.data[, i], "\U2022", "*")
    phenotype.data[, i] <- stringr::str_replace_all(phenotype.data[, i], "\U202[89A-F]", "")
  }
  phenotype.data
}

#' Set Excel error messages to NA and flag them as special problems
#'
#' @details
#' This function removes "#error!" and "#value!" Excel error codes and reports
#' the count of any such removals to the per-variable summary list, for inclusion
#' in the downstream report.
#'
#' @description
#' The function `remove.nonword.chars` will remove the leading and trailing characters
#' from default Excel error codes, so for the least ambiguity, it should be called
#' after Excel errors are already removed.
#'
#' @param phenotype.data data.frame, input phenotype data
#' @param variable.summary list, per-variable summary information and configuration data
#' @return list, first entry the input phenotype data with Excel error codes set to NA;
#' second entry the variable summary list with injected reporting information about
#' any Excel errors found for each variable
exclude.excel.failures <- function(phenotype.data, variable.summary) {
  excel.problem.regex <- "^=?#error!$|^=?#value!$|^=?#ERROR!$|^=?#VALUE!$"
  for (i in seq_len(ncol(phenotype.data))) {
    excel.problems <- stringr::str_detect(phenotype.data[, i], excel.problem.regex)
    if (length(which(excel.problems))) {
      phenotype.data[excel.problems, i] <- NA
      variable.summary$variables[[i]]$excel.problem.count <- sum(excel.problems)
    }
  }
  list(
    phenotype.data = phenotype.data,
    variable.summary = variable.summary
  )
}

#' Detects and reports residual Unicode characters
#'
#' @details
#' Unicode characters should be removed when detected, as they are typically
#' only sporadically injected by upstream text processors and will interfere
#' with any downstream attempts at standardization. `process.unicode.characters`
#' attempts to replace an existing set of these nefarious characters with their
#' ASCII-style equivalents, but others may be introduced later, and can
#' be flagged here.
#'
#' @description
#' TBD
#'
#' @param phenotype.data data.frame, input phenotype data
#' @param variable.summary list, per-variable summary information and config data
#' @return list, input variable summary with updated logging information
#' if any residual Unicode characters are detected.
detect.unicode.characters <- function(phenotype.data, variable.summary) {
  for (i in seq_len(ncol(phenotype.data))) {
    unicode.detected <- stringr::str_detect(phenotype.data[, i], "[^\001-\177]")
    if (length(which(unicode.detected))) {
      variable.summary$variables[[i]]$unicode.entries <- table(phenotype.data[unicode.detected, i])
    }
  }
  variable.summary
}
