#' @title
#' Harmonize all input phenotype entries to lowercase
#'
#' @description
#' To facilitate downstream operations comparing values
#' to configuration data and each other, all string input
#' values are set to lowercase. This is primarily to
#' address issues with files that have been constructed
#' via manual entry.
#'
#' @details
#' The output data frame will have all string values in lowercase.
#' This is usually fine, but can cause some issues when input
#' subject IDs contain uppercase letters. This package assumes
#' that subject IDs are not ultimately case sensitive. If this causes
#' substantial problems for users, we could potentially patch this
#' to restore ID case in the output data; post an issue if you are
#' interested in this functionality.
#'
#' @param df Data frame of input phenotype data as character vectors.
#' @return Data frame containing input entries with all alphabetical
#' values set to their lowercase versions.
#' @examples
#' phenotype.data <- data.frame(
#'   HW00001 = c("A", "B", "C", "D"),
#'   HW00002 = c("1.03mL", "2.33", "2.22 L", "4")
#' )
#' result <- process.phenotypes:::make.lowercase(phenotype.data)
make.lowercase <- function(df) {
  data.frame(lapply(df, tolower))
}

#' @title
#' Harmonize whitespace formatting in input data frame
#'
#' @description
#' This function collapses consecutive whitespace into
#' single whitespace, and removes padding whitespace
#' at beginning or end of input strings. It furthermore
#' sets assorted variants of {whitespace}-{whitespace}
#' to be simply -, facilitating tabulation of repeated responses.
#' This cleaning is basically mandatory on datasets
#' that have been collected with manual entry, but is
#' generally harmless with other types of input data.
#'
#' @param df Data frame of input phenotype data as character vectors.
#' @return Data frame containing input entries with whitespace collapsed
#' and padding whitespace removed.
#' @examples
#' phenotype.data <- data.frame(
#'   HW00001 = c("A", "B ", " C", "  D"),
#'   HW00002 = c("entry 1", "entry  2", "1- 2", "3 -4")
#' )
#' result <- process.phenotypes:::remove.whitespace(phenotype.data)
remove.whitespace <- function(df) {
  df <- data.frame(lapply(df, stringr::str_squish))
  data.frame(lapply(df, stringr::str_replace_all, "[ \\-]*-[ \\-]*", "-"))
}

#' @title
#' Collapse multiple consecutive instances of specified characters
#' to a single instance
#'
#' @description
#' Repeated instances of certain types of characters generally indicate
#' manual input errors, and prevent harmonization of repeated entries.
#' This function collapses specified characters into single instances of
#' a corresponding replacement character. In the case of the `create.phenotype.report`
#' processing chain, multiple '.' are collapsed into a single '.', and multiple
#' entries of any combination of '/' and '\\' are collapsed into a single '/'.
#'
#' @param df Data frame of input phenotype data as character vectors.
#' @param targets Character vector of targets to collapse into single instances.
#' @param replacements Character vector of what the repeats should be collapsed to.
#' @return Data frame containing input entries with specified repeat characters
#' collapsed into single instances.
#' @examples
#' phenotype.data <- data.frame(
#'   HW00001 = c("A", "B", "C", "D"),
#'   HW00002 = c("//", "\\\\//", "..", "1..1")
#' )
#' result <- process.phenotypes:::collapse.repeats(phenotype.data)
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

#' @title
#' Apply custom handling to various kinds of unexpected characters
#'
#' @description
#' Unstructured input datasets feature various kinds of systematic
#' data entry deviations. Some of these are sufficiently unambiguously
#' unintentional that they can be handled automatically with
#' limited user intervention. Note that if any of these steps are considered
#' too stringent, setting the configured variable type to "string" will disable
#' this function entirely.
#'
#' @details
#' The following steps are applied by this function in particular contexts:
#' - for numerics only, pad a leading decimal point with a leading 0
#' - for non-strings, replace '>' or '<' with 'greater than' or 'less than'
#' - for non-strings, set anything that looks like a negative number to NA
#'   - this may be too stringent for some applications. if so, use `type="string"`
#' - for non-strings, remove padding non-word characters
#'
#' @param df Data frame of input phenotype data as character vectors.
#' @param variable.summary List of variable-specific configuration data.
#' @return Data frame containing modified version of input with nonword characters
#' modified as described in Details.
#' @examples
#' phenotype.data <- data.frame(
#'   HW00001 = c("A", "B", "C", "D"),
#'   HW00002 = c("1.0m", "-1.0", "{1.0}", "<1")
#' )
#' variable.summary <- list(variables = list(
#'   HW00001 = list(params = list(
#'     name = "subjid",
#'     type = "string",
#'     subject_id = TRUE
#'   )),
#'   HW00002 = list(params = list(
#'     name = "metric",
#'     type = "numeric"
#'   ))
#' ))
#' result <- process.phenotypes:::remove.nonword.chars(phenotype.data, variable.summary)
remove.nonword.chars <- function(df, variable.summary) {
  for (i in seq_len(ncol(df))) {
    if (is.null(variable.summary$variables[[i]]$params$type)) next
    if (grepl("string", variable.summary$variables[[i]]$params$type)) next
    ## prefix entries starting with decimal places with the placeholder 0
    if (grepl("^numeric$", variable.summary$variables[[i]]$params$type)) {
      df[, i] <- stringr::str_replace_all(df[, i], "^\\.([0-9]+)[ %a-zA-Z\\-]*$", "0.\\1")
    }
    ## replace leading ">/<" characters meaning "greater/less than" with words
    df[, i] <- stringr::str_replace_all(df[, i], "^ *> *(\\w+)", "greater than \\1")
    df[, i] <- stringr::str_replace_all(df[, i], "^ *< *(\\w+)", "less than \\1")
    df.orig <- df[, i]
    ## take anything that looks like a negative number and cast it into the void
    df[, i] <- stringr::str_replace_all(df[, i], "^ *- *[0-9]+.*$", "na")
    ## strip nonword characters from extremes
    df[, i] <- stringr::str_replace_all(df[, i], "^\\W+|\\W*[^[\\w)}\\]]]$", "")
    ## TODO: remove crappy logging
    diff.data <- cbind(
      df.orig[df[, i] != df.orig & !is.na(df[, i])],
      df[df[, i] != df.orig & !is.na(df[, i]), i]
    )
  }
  df
}

#' @title
#' Replace a variety of freetext aliases for NA with actual NA
#'
#' @description
#' Freetext responses tend to involve various standard responses
#' that effectively mean non-response. Since the responses are freetext,
#' this will also include a complicated assortment of typos of these
#' values. This function replaces these values with NA, to try
#' to streamline the process of encoding missingness.
#'
#' @details
#' This package does not make a distinction between different
#' forms of missingness, as some statistical packages do. This
#' is largely because the primary downstream applications of this
#' package's results make no meaningful distinction, and only operate
#' on complete-case data. If finer granularity is required, particularly
#' for phenotype multiple imputation, this package may not be entirely
#' appropriate for the application. Alternatively, you can post
#' an issue and propose how we might go about handling this distinction.
#'
#' This mapping attempts to avoid automatically aliasing certain
#' types of responses that are ambiguously NA. The most common example
#' is the response 'none', which can either indicate missingness or
#' a valid response to some sort of ordinal response. If a response
#' that isn't listed here truly does indicate NA for a particular
#' variable, it is appropriate to add that value to `na-values`
#' for that particular variable's configuration.
#'
#' @param df Data frame of input phenotype data as character vectors.
#' @return Data frame of input data with recognized patterns reset to NA.
#' @examples
#' phenotype.data <- data.frame(
#'   HW00001 = c("A", "B", "C", "D"),
#'   HW00002 = c("not sure", "hello", "not available", "none")
#' )
#' result <- process.phenotypes:::normalize.missing.values(phenotype.data)
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

#' @title
#' Detect whether strings look like SBP/DBP measurements
#'
#' @description
#' Blood pressure measurements may be reported as ###/###
#' entries in a single vector, complicating numeric casts.
#' As more datasets have been tested with this package,
#' more creative BP reporting formats have been detected,
#' and in some of those cases the recognized formats have been
#' extended to adapt to these situations, so long as the
#' values are reasonably unambiguous.
#'
#' @details
#' The accepted format is, generally, any amount of numbers indicating
#' systolic blood pressure; optionally whitespace; one of -/, delimiting
#' the blood pressure values; optionally whitespace; and any amount of
#' numbers indicating diastolic blood pressure.
#'
#' Optionally, this function will allow or forbid trailing meta-text,
#' which generally is expected to involve some variant of 'mmhg'.
#' Depending on whether you expect such a deviation in your input data,
#' it may or may not be appropriate to relax this pattern and potentially
#' permit more false positives.
#'
#' In extreme cases, no delimiter is present at all between systolic
#' and diastolic blood pressure, and the values are reported as
#' simply '#####'. Traditionally, we had strongly advocated for excluding
#' these values. However, we ended up with a dataset that had a substantial
#' preponderance of such values. There are situations in which the
#' numbers may be intuitively partitioned based on number of digits.
#' In that case, the resolution is as follows (S: assigned to systolic;
#' D: assigned to diastolic):
#'
#' - six digits -> SSSDDD
#' - five digits -> SSSDD
#' - four digits -> SSDD
#'
#' `allow_undelimited_bp` is exposed to user configuration, but disabled by
#' default. Please only activate this behavior if you are absolutely
#' certain that you can tolerate this aggressive data conversion behavior.
#'
#' @param vec Character vector of input data values.
#' @param allow.trailing Logical indicating whether a blood pressure-like
#' entry with arbitrary trailing text (e.g. '120/80mmhg') should be considered possibly
#' a valid blood pressure result.
#' @param allow.undelimited Logical indicating whether a 4-6 digit
#' integer should be considered a two-part blood pressure measurement
#' with no valid delimiter. See Details for description of the conversion heuristic.
#' Note that this is disabled by default and expected
#' to be noisy, and should be avoided if possible.
#' @return Logical vector of same length as input data, indicating whether the input
#' matches blood pressure format.
#' @examples
#' data <- c("120/80", "120/80mmhg", "120 / 80", "12080")
#' results <- process.phenotypes:::is.blood.pressure(data)
is.blood.pressure <- function(vec, allow.trailing = FALSE, allow.undelimited = FALSE) {
  # TODO: Note that this currently will massage values like
  # 100/80.98181818 to 100/80, for example.  What is the desired
  # behavior in this case?  Also, this won't allow decimals in
  # systolic.
  pattern <- "^\\d+ *[-/,] *\\d+"
  if (allow.trailing) {
    pattern <- paste(pattern, ".*$", sep = "")
  } else {
    pattern <- paste(pattern, "$", sep = "")
  }
  res <- stringr::str_detect(vec, pattern)
  if (allow.undelimited) {
    pattern <- "^\\d{4}\\d?\\d?$"
    res <- res | stringr::str_detect(vec, pattern)
  }
  res
}

#' @title
#' Attempt to convert somewhat malformed numeric vectors to actual numerics
#'
#' @description
#' Given a string vector with malformed numeric entries, this function attempt
#' to force conversion to numerics by stripping common problems.
#'
#' @details
#' This conversion is potentially _very_ aggressive for certain types of inputs.
#' As such, anything that fails conversion is recorded and reported out as
#' a table in the `create.phenotype.report` cleaning report. Each time there's
#' anything in such a table for any variable, the values should be evaluated
#' and inspected for possible issues, or potentially any requirements for
#' updating the accepted numeric format definition.
#'
#' @param vec Character vector of input candidate numeric data.
#' @param var.summary List of input variable configuration data for this data vector.
#' @return List, with first entry `phenotype.data` modified input data vector with
#' numeric conversion applied, and second entry `variable.summary` the input
#' configuration data with invalid numeric entries annotated for later reporting.
#' @examples
#' data <- c("100", "120.1", "1.5 meters")
#' var.summary <- list()
#' results <- process.phenotypes:::reformat.numerics(data, var.summary)
reformat.numerics <- function(vec, var.summary) {
  ## treat this as arbitrary numeric data
  ## if the prefix of a value looks like a numeric, strip its suffix
  possible.numeric <- stringr::str_detect(vec, "^-?\\d+[\\.,]?\\d*($| *[^ \\d/].*$)") & !is.na(vec)
  res <- rep(NA, length(vec))
  res[possible.numeric] <- stringr::str_replace(
    vec[possible.numeric],
    "^(-?\\d+[\\.,]?\\d*)($| *[^ \\d/].*$)", "\\1"
  )
  res <- as.numeric(res)
  var.summary$invalid.numeric.entries <- vec[!possible.numeric & !is.na(vec)]
  list(phenotype.data = res, variable.summary = var.summary)
}

#' Reformat blood pressure measures to standardized format
#'
#' @description
#' Given string vector with potentially malformed blood pressure
#' entries, attempt to standardize to SBP/DBP by stripping
#' common problems and reporting as a standard string. The process
#' of converting these blood pressure data to numeric representations
#' of systolic and diastolic blood pressure data separately is expected
#' to be conducted in derived variables.
#'
#' @details
#' Please see the Details of `is.blood.pressure` for a discussion of
#' accepted blood pressure formats for this function.
#'
#' @param vec Character vector of input blood pressure measurements.
#' @param var.summary List of input configuration data for this particular variable.
#' @return List, with first entry `phenotype.data` containing input data
#' reformatted into standard `SBP/DBP` format or NA, and second entry `variable.summary`
#' containing input configuration data with additional annotation for values that
#' failed format recognition and conversion.
#' @examples
#' phenotype.data <- c("120/80", "120 / 80", "120/80mmhg", "12080")
#' var.summary <- list(params = list(name = "variable", type = "bp"))
#' results <- process.phenotypes:::reformat.blood.pressure(phenotype.data, var.summary)
reformat.blood.pressure <- function(vec, var.summary) {
  ## treat this as BP, eliminate anything else
  ## if the prefix of a value looks like BP, strip its suffix
  allow.undelimited <- var.summary$params$allow_undelimited_bp
  if (is.null(allow.undelimited)) {
    allow.undelimited <- FALSE
  }
  possible.blood.pressure <- is.blood.pressure(vec,
    allow.trailing = TRUE,
    allow.undelimited = allow.undelimited
  ) & !is.na(vec)
  res <- rep(NA, length(vec))
  undelimited.format <- stringr::str_detect(vec[possible.blood.pressure], "^\\d{4}\\d?\\d?$")
  if (allow.undelimited) {
    four.digit <- possible.blood.pressure & stringr::str_detect(vec, "^\\d{4}$")
    five.digit <- possible.blood.pressure & stringr::str_detect(vec, "^\\d{5}$")
    six.digit <- possible.blood.pressure & stringr::str_detect(vec, "^\\d{6}$")
    other.bp <- possible.blood.pressure & !four.digit & !five.digit & !six.digit
    res[four.digit] <- stringr::str_replace(
      vec[four.digit],
      "^(\\d{2})(\\d{2})$",
      "\\1/\\2"
    )
    res[five.digit] <- stringr::str_replace(
      vec[five.digit],
      "^(\\d{3})(\\d{2})$",
      "\\1/\\2"
    )
    res[six.digit] <- stringr::str_replace(
      vec[six.digit],
      "^(\\d{3})(\\d{3})$",
      "\\1/\\2"
    )
    res[other.bp] <- stringr::str_replace(vec[other.bp], "^(\\d+) *[-/,] *(\\d+).*$", "\\1/\\2")
  } else {
    res[possible.blood.pressure] <- stringr::str_replace(
      vec[possible.blood.pressure],
      "^(\\d+) *[-/,] *(\\d+).*$",
      "\\1/\\2"
    )
  }
  var.summary$invalid.blood.pressure.entries <- vec[!possible.blood.pressure & !is.na(vec)]
  list(phenotype.data = res, variable.summary = var.summary)
}

#' @title
#' Convert character vector to factor, respecting configuration aliases
#'
#' @description
#' Categorical, ordinal, and binary type configured variables in the input
#' dataset configuration are converted into factors. Factors are matched
#' against each level's primary name. In situations
#' where harmonization is required, for example to create compatible factor
#' levels with other variables or rename factor levels, the `alternate_patterns`
#' sequence of regular expressions is used to detect and alias responses to harmonized
#' factor levels.
#'
#' @details
#' Note that pattern matching is expected to be conducted versus data inputs
#' that have already passed through upstream cleaning and have been made lowercase,
#' had nonword characters removed, etc. If there are issues, these will be reported
#' as invalid type conversions in the `create.phenotype.report` cleaning report,
#' and the specified patterns can be updated accordingly.
#'
#' @param vec Character vector of input phenotype data.
#' @param variable.summary List of input configuration data for the input variable.
#' @return List, with first entry 'phenotype.data' containing factor converted
#' phenotype information, and second entry 'variable.summary' containing input
#' configuration with possible additional information
#' about invalid factor levels converted to NA.
#' @examples
#' data <- c("apple", "banana", "red apple")
#' variable.summary <- list(params = list(
#'   name = "fruit",
#'   type = "categorical",
#'   levels = list(
#'     "1" = list(
#'       name = "apple",
#'       alternate_patterns = c("red apple")
#'     ),
#'     "2" = list(name = "banana")
#'   )
#' ))
#' results <- process.phenotypes:::reformat.factor(data, variable.summary)
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

#' @title
#' Convert sporadic Unicode-only characters into lesser equivalents
#'
#' @description
#' Sporadic Unicode characters can be inserted into datasets by
#' upstream text editors. This function attempts to take them right
#' back out again. Replacements are defined in a text linker function
#' under system.file("unicode_pattern_replacements.tsv", package = "process.phenotypes").
#'
#' @details
#' Via unknown reasons, something particularly strange can happen with
#' Excel's "=#ERROR!" code, as in one instance it can be converted
#' into, of all things, an emoji. The particular instance of this that we've
#' encountered should be automatically handled, but every time we process
#' a new dataset we find new, creative instances of things that were
#' supposed to be other things.
#'
#' The text linker file does a much better job of transparently exposing
#' the pattern conversions than previous implementations. However, there
#' are supposedly, according to relevant documentation, certain issues
#' that may be introduced when such pattern linking is attempted on Windows.
#' We have yet to actually observe this phenomenon ourselves, but it
#' is nevertheless theoretically possible. If anyone reading this ever
#' observes any such issue, please post an issue about it to the repo.
#'
#' @param phenotype.data Data frame of input phenotype data as character vectors.
#' @return Data frame, containing input data with recognized Unicode characters converted
#' into more manageable equivalents.
#' @examples
#' phenotype.data <- data.frame(subjid = c("A", "B", "C", "\u2070"))
#' result <- process.phenotypes:::process.unicode.characters(phenotype.data)
process.unicode.characters <- function(phenotype.data) {
  pattern.replacements <- read.table(system.file("unicode_pattern_replacements.tsv", package = "process.phenotypes"),
    header = TRUE, stringsAsFactors = FALSE, comment.char = "", quote = "", sep = "\t"
  )
  pattern.replacement.vec <- pattern.replacements[, 2]
  names(pattern.replacement.vec) <- stringi::stri_unescape_unicode(pattern.replacements[, 1])
  for (i in seq_len(ncol(phenotype.data))) {
    phenotype.data[, i] <- stringr::str_replace_all(phenotype.data[, i], pattern.replacement.vec)
  }
  phenotype.data
}

#' @title
#' Set Excel error messages to NA and flag them as special problems
#'
#' @description
#' This function removes "#error!" and "#value!" Excel error codes and reports
#' the count of any such removals to a per-variable summary list, for inclusion
#' in the downstream report.
#'
#' @details
#' This processing step will be specifically reported in the output report from `create.phenotype.report`
#' as a possible indicator of data malformation. Excel (Calc, Sheets, etc.) should generally not
#' be used to process biological data at any step. for more information on why,
#' please see \url{https://www.nature.com/articles/d41586-021-02211-4}.
#'
#' The function `remove.nonword.chars` will remove the leading and trailing characters
#' from default Excel error codes, so for the least ambiguity, it should be called
#' after Excel errors are already removed.
#'
#' @param phenotype.data Data frame of input phenotype data as character vectors.
#' @param variable.summary List of dataset-specific summary information and configuration data.
#' @return List, with first entry `phenotype.data` the input phenotype data with Excel
#' error codes set to NA, and second entry `variable.summary` the input variable configuration list
#' with injected reporting information about any Excel errors found for each variable.
#' @examples
#' phenotype.data <- data.frame(
#'   HW00001 = c("A", "B", "C", "D"),
#'   HW00002 = c("10.2", "12.2", "14.4", "=#ERROR!")
#' )
#' variable.summary <- list(variables = list(
#'   HW00001 = list(params = list(
#'     name = "subjid",
#'     type = "string",
#'     subject_id = TRUE
#'   )),
#'   HW00002 = list(params = list(
#'     name = "computedvar",
#'     type = "numeric"
#'   ))
#' ))
#' result <- process.phenotypes:::exclude.excel.failures(phenotype.data, variable.summary)
exclude.excel.failures <- function(phenotype.data, variable.summary) {
  excel.problem.regex <- "^=?#div/0!$|^=?#error!$|^=?#value!$|^=?#ERROR!$|^=?#VALUE!$|^=?#DIV/0!$"
  for (i in seq_len(ncol(phenotype.data))) {
    excel.problems <- stringr::str_detect(phenotype.data[, i], excel.problem.regex)
    if (length(which(excel.problems))) {
      phenotype.data[excel.problems & !is.na(excel.problems), i] <- NA
      variable.summary$variables[[i]]$excel.problem.count <- sum(excel.problems, na.rm = TRUE)
    }
  }
  list(
    phenotype.data = phenotype.data,
    variable.summary = variable.summary
  )
}

#' @title
#' Detect and report residual Unicode characters remaining after recognized patterns are handled
#'
#' @description
#' Unicode characters should be removed when detected, as they are typically
#' only sporadically injected by upstream text processors and will interfere
#' with any downstream attempts at standardization. `process.unicode.characters`
#' attempts to replace an existing set of these nefarious characters with their
#' ASCII-style equivalents, but others may be introduced later, and can
#' be flagged here.
#'
#' @details
#' The reporting output of this function is the primary source of information
#' for adding additional pattern replacements to the package linker file
#' in system.files("unicode_pattern_replacements.tsv", package = "process.phenotypes").
#' This function would be more helpful if it directly ejected the Unicode hex encoding
#' along with the rendered Unicode character itself.
#'
#' @param phenotype.data Data frame of input phenotype data as character vectors.
#' @param variable.summary List of dataset-specific summary information and configuration data.
#' @return List of input variable summary data with updated logging information
#' if any residual Unicode characters are detected.
#' @examples
#' phenotype.data <- data.frame(
#'   HW00001 = c("A", "B", "C", "D"),
#'   HW00002 = c("1.01", "2.02", "3.03", "\u0433")
#' )
#' variable.summary <- list(variables = list(
#'   HW00001 = list(params = list(
#'     name = "subjid",
#'     type = "string",
#'     subject_id = TRUE
#'   )),
#'   HW00002 = list(params = list(
#'     name = "var",
#'     type = "numeric"
#'   ))
#' ))
#' result <- process.phenotypes:::detect.unicode.characters(phenotype.data, variable.summary)
detect.unicode.characters <- function(phenotype.data, variable.summary) {
  for (i in seq_len(ncol(phenotype.data))) {
    unicode.detected <- stringr::str_detect(phenotype.data[, i], "[^\001-\177]") & !is.na(phenotype.data[, i])
    if (length(which(unicode.detected))) {
      variable.summary$variables[[i]]$unicode.entries <- table(phenotype.data[unicode.detected, i])
    }
  }
  variable.summary
}
