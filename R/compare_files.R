#' Generate an html report of pre- and post-audit
#' comparison
#'
#' @details
#' Calling rmarkdown directly is hard.
#'
#' @description
#' TBD
#'
#' @param first.filename character vector, name of
#' first file to load and compare. should be the
#' file with identity tags linking its variables
#' to the second dataset
#' @param first.config.yaml character vector, name
#' of first file's config yaml
#' @param second.filename character vector, name of
#' second file to load and compare
#' @param second.config.yaml character vector, name
#' of second file's config yaml
#' @param shared.model.yaml character vector,
#' name of shared model configuration for both
#' input files
#'
#' note that this function is primary used for its
#' side effect, emitting content to screen.
#'
#' @export compare.files.report
compare.files.report <- function(first.filename,
                                 first.config.yaml,
                                 second.filename,
                                 second.config.yaml,
                                 shared.model.yaml,
                                 out.filename) {
  stopifnot(
    is.vector(first.filename, mode = "character"),
    length(first.filename) == 1
  )
  stopifnot(
    is.vector(first.config.yaml, mode = "character"),
    length(first.config.yaml) == 1
  )
  stopifnot(
    is.vector(second.filename, mode = "character"),
    length(second.filename) == 1
  )
  stopifnot(
    is.vector(second.config.yaml, mode = "character"),
    length(second.config.yaml) == 1
  )
  stopifnot(
    is.vector(shared.model.yaml, mode = "character"),
    length(shared.model.yaml) == 1
  )
  stopifnot(
    is.vector(out.filename, mode = "character"),
    length(out.filename) == 1
  )
  ## find Rmd file from system installation
  rmarkdown.template <- system.file("rmd", "audit_comparisons.Rmd",
    package = "process.phenotypes"
  )
  rmarkdown::render(rmarkdown.template,
    output_file = out.filename,
    output_dir = dirname(out.filename),
    params = list(
      pre.audit.filename = first.filename,
      pre.audit.config = first.config.yaml,
      post.audit.filename = second.filename,
      post.audit.config = second.config.yaml,
      shared.model.yaml = shared.model.yaml
    )
  )
}


#' Compare two files for approximate identity of columns
#'
#' @details
#' as a utility function, this compares two datasets
#' for approximately identical columns, based not on
#' the header labels but on column values. this is
#' likely to completely fail until at least minimal
#' initial processing with the main methods of this
#' package.
#'
#' @description
#' TBD
#'
#' @param first.filename character vector, name of
#' first file to load and compare. should be the
#' file with identity tags linking its variables
#' to the second dataset
#' @param first.config.yaml character vector, name
#' of first file's config yaml
#' @param second.filename character vector, name of
#' second file to load and compare
#' @param second.config.yaml character vector, name
#' of second file's config yaml
#' @param shared.model.yaml character vector,
#' name of shared model configuration for both
#' input files
#'
#' note that this function is primary used for its
#' side effect, emitting content to screen.
#'
#' @export compare.files
compare.files <- function(first.filename,
                          first.config.yaml,
                          second.filename,
                          second.config.yaml,
                          shared.model.yaml) {
  stopifnot(
    is.vector(first.filename, mode = "character"),
    length(first.filename) == 1
  )
  stopifnot(
    is.vector(first.config.yaml, mode = "character"),
    length(first.config.yaml) == 1
  )
  stopifnot(
    is.vector(second.filename, mode = "character"),
    length(second.filename) == 1
  )
  stopifnot(
    is.vector(second.config.yaml, mode = "character"),
    length(second.config.yaml) == 1
  )
  stopifnot(
    is.vector(shared.model.yaml, mode = "character"),
    length(shared.model.yaml) == 1
  )

  data1 <- read.table(first.filename,
    header = TRUE,
    stringsAsFactors = FALSE,
    quote = "\"", sep = "\t",
    comment.char = ""
  )
  config1 <- load.configuration(
    first.config.yaml,
    shared.model.yaml
  )
  config1 <- map.header(data1, "CV_raw", config1,
    force.header.mapping = TRUE
  )
  subject.id.index.1 <- find.subject.id.index(config1)
  data1 <- data1[!duplicated(data1[, subject.id.index.1]), ]
  rownames(data1) <- data1[, subject.id.index.1]
  data2 <- read.table(second.filename,
    header = TRUE,
    stringsAsFactors = FALSE,
    quote = "", sep = "\t",
    comment.char = ""
  )
  config2 <- load.configuration(
    second.config.yaml,
    shared.model.yaml
  )
  config2 <- map.header(data2, "CV", config2,
    force.header.mapping = TRUE
  )
  subject.id.index.2 <- find.subject.id.index(config2)
  data2 <- data2[!duplicated(data2[, subject.id.index.2]), ]
  rownames(data2) <- data2[, subject.id.index.2]

  data1 <- data1[rownames(data1) %in% rownames(data2), ]
  data2 <- data2[rownames(data2) %in% rownames(data1), ]
  data1 <- data1[order(data1[, subject.id.index.1]), ]
  data2 <- data2[order(data2[, subject.id.index.2]), ]
  ## compare all columns in dataset 1 to dataset 2
  for (i in seq_len(ncol(data1))) {
    var.name <- names(config1$variables)[i]
    cat("#### Variable ", var.name, "\n\n", sep = "")
    var.identity <- config1$variables[[var.name]]$params$identity
    stopifnot(!is.null(var.identity))
    is.unknown <- grepl("^unknown.*$", var.identity)
    is.tagged <- stringr::str_detect(var.identity, "\\((CV\\d+)\\)")
    if (is.tagged) {
      ## scan just against specific target
      var.target <- stringr::str_replace(var.identity, "^.*\\((CV\\d+)\\).*$", "\\1")
      stopifnot(!is.null(config2$variables[[var.target]]))
      compare.column.pair(
        data1[, i],
        var.name,
        config1$variables[[i]],
        data2[, var.target],
        var.target,
        config2$variables[[var.target]]
      )
    }
    if (is.tagged | is.unknown) {
      ## need to conduct all pairs scan
      compare.columns(
        data1[, i],
        var.name,
        data2
      )
    }
    if (!is.tagged & !is.unknown) {
      cat("comparison skipped due to identity \"", var.identity, "\"", "\n",
        sep = ""
      )
    }
    cat("\n\n***\n<br>\n\n")
  }
}

#' Compare a column to another data frame for approximate identity
#'
#' @details
#' a single column is compared against all columns of a data frame,
#' to try to find the most likely identical column in the target.
#' this is likely to completely fail until at least minimal
#' initial processing with the main methods of this
#' package.
#'
#' @description
#' TBD
#'
#' @param query.column vector, data to be queried against target columns
#' @param query.colname character vector, name of query column
#' in its dataset
#' @param target.df data frame, target dataset to scan against query
#' column
#'
compare.columns <- function(query.column, query.colname,
                            target.df) {
  all.comparisons <- c()
  all.n <- c()
  all.names <- c()
  if (length(which(!is.na(query.column))) == 0) {
    warning("query column ", query.colname, " has entirely ",
      "NA entries, and thus the all dataset comparison ",
      "scan is suppressed",
      sep = ""
    )
    return()
  }
  for (i in seq_len(ncol(target.df))) {
    if (length(which(!is.na(target.df[, i]))) == 0) next
    all.comparisons <- c(
      all.comparisons,
      length(which(as.character(query.column) == as.character(target.df[, i]) &
        !is.na(query.column) &
        !is.na(target.df[, i]))) /
        length(which(!is.na(query.column) &
          !is.na(target.df[, i])))
    )
    all.n <- c(
      all.n,
      length(which(!is.na(query.column) &
        !is.na(target.df[, i])))
    )
    all.names <- c(
      all.names,
      colnames(target.df)[i]
    )
  }
  res <- data.frame(
    all.names,
    all.comparisons * 100,
    all.n
  )
  colnames(res) <- c("Variable Name", "Percent Identity", "N Comparisons")
  res <- res[!is.nan(res[, 2]), ]
  res <- res[res[, 2] > 1 / 30000, ]
  res <- res[order(res[, 2], decreasing = TRUE), ]
  print(knitr::kable(res,
    caption = paste("Top results for all variable scan versus ",
      query.colname,
      sep = ""
    ),
    table.attr = "style=\"width: 90%\""
  ))
}

#' Compare a column to another column for full tabular report
#'
#' @details
#' this function is designed to be used if two approximately
#' (or ideally) identical columns have been found, and the overlap
#' between their results needs to be determined
#'
#' @description
#' TBD
#'
#' @param col1 vector, first data vector to be compared
#' @param col1.name character vector, name of first data vector
#' in its dataset
#' @param col1.config list, configuration data for first vector
#' @param col2 vector, second data vector to be compared
#' @param col2.name character vector, name of second data vector
#' @param col2.config list, configuration data for second vector
#' in its dataset
compare.column.pair <- function(col1, col1.name, col1.config,
                                col2, col2.name, col2.config) {
  if (length(which(!is.na(col1))) == 0 |
    length(which(!is.na(col2))) == 0) {
    warning("Fixed pair comparison for ", col1.name, " and ",
      col2.name, " is suppressed due to lack of non-NA entries",
      sep = ""
    )
    return()
  }
  if (grepl("^binary$|^categorical$|^ordinal$", col1.config$params$type) &
    grepl("^binary$|^categorical$|^ordinal$", col2.config$params$type)) {
    tab.df <- table(col1, col2, useNA = "ifany")
    print(knitr::kable(tab.df,
      caption = paste("Tabular comparison for fixed pair scan ",
        "between ", col1.name, " and ", col2.name,
        " (", col2.config$params$name, ")",
        sep = ""
      ),
      table.attr = "style=\"width: 90%\""
    ))
  } else {
    cat("Percent identity comparison for fixed pair scan between ",
      col1.name, " and ", col2.name, " (", col2.config$params$name, "): ",
      length(which(as.character(col1) == as.character(col2) &
        !is.na(col1) &
        !is.na(col2))) /
        length(which(!is.na(col1) & !is.na(col2))),
      " (from ",
      length(which(!is.na(col1) & !is.na(col2))),
      " comparisons)\n\n",
      sep = ""
    )
  }
  if (col1.config$params$type != col2.config$params$type) {
    warning("inconsistent configured type detected for ",
      col1.name, " and ", col2.name, " (", col2.config$params$name, ")",
      sep = ""
    )
  }
}
