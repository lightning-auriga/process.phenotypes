#' @title
#' Restructure the input dataset configuration to prepare for modification,
#' and sanity check variable name linking to data.
#'
#' @description
#' In order for the package to conduct cleaning, it needs a modified
#' version of the input dataset configuration that partitions the input
#' configuration from new values that are injected during processing.
#' In the midst of creating the relevant data structure, the correspondence
#' between input phenotype column headers and `name` entries of variable
#' configuration blocks is tested, to provide a sanity check that configuration
#' has been conducted appropriately.
#'
#' @details
#' This is one of the oldest functions in the package, and most of its original
#' functionality has been removed entirely or vastly restructured. This still
#' provides a very important sanity check that the input configuration variables
#' structure matches the expectation in the input data. The parameter `force.header.mapping`
#' is present in the situation that the caller wishes to override the sanity check.
#' While potentially appealing in some circumstances, this parameter is ultimately
#' not exposed to the user of `create.phenotype.report`, and as such input configuration
#' must exactly match the column headers of the phenotype data. We do not plan
#' on changing this behavior.
#'
#' One very particular case worth noting here is the instance in which the input
#' phenotype data have duplicated column headers, and as such the user must
#' use the process.phenotypes configuration structure to unambiguously map
#' those duplicates to unique aliases. In that very particular situation, the
#' column ordering sanity check is particularly essential, and should not be overridden.
#'
#' Mapping the input configuration data into a `variables[[varname]]$params` list
#' is designed to protect the user configuration settings from accidental modification
#' during package processing. It does somewhat complicate the internal calls
#' to the variable summary object.
#'
#' This function is so old and strange that it should never be called by anything
#' outside of the standard `create.phenotype.report` processing chain.
#'
#' @param df Data frame of input phenotype data with original column headers.
#' @param config.data List of loaded input dataset yaml configuration.
#' @param dataset.tag Character vector of unique string tag for this dataset.
#' In this case, this is only used for error reporting.
#' @param force.header.mapping Logical indicating whether you want to be somewhat foolish
#' and allow desync between config variable names and dataset header data.
#' This safety check was implemented in part due to the possible presence of duplicate
#' column header names, and turning it off can potentially have catastrophically
#' bad effects if your configuration file is malformed.
#' @return List containing a modified version of the input dataset configuration,
#' with input configuration data per variable partitioned into params blocks.
#' @seealso sanitize.header
#' @usage NULL
map.header <- function(df, dataset.tag, config.data,
                       force.header.mapping = FALSE) {
  ## new: pull names from input config variable specification
  ## TODO: add yaml checker that makes sure these names
  ## are alphanumeric only
  new.names <- names(config.data$variables)
  colnames(df) <- apply.replacements(colnames(df))
  res <- list(
    tag = config.data$tag,
    globals = config.data$globals,
    variables = lapply(colnames(df), function(i) {
      list(original.name = i)
    })
  )
  names(res$variables) <- new.names
  ## for now, to handle duplicate identical header descriptors
  ## in raw input files, enforce identical ordering of variables
  ## in phenotype data and in configuration yaml file
  config.names <- lapply(config.data$variables, function(i) {
    i[["name"]]
  })

  if (ncol(df) != length(config.names)) {
    stop(
      "for dataset tag ", dataset.tag, ", variable count ",
      "in phenotypes does not match count in yaml config (",
      "found ", ncol(df), " in phenotypes but ",
      length(config.names), " in config)"
    )
  }
  if (!identical(colnames(df), unname(unlist(config.names))) &&
    !force.header.mapping) {
    error.data <- cbind(
      colnames(df),
      unname(unlist(config.names))
    )
    print(error.data[error.data[, 1] != error.data[, 2], ])
    stop(
      "for dataset tag ", dataset.tag, ", column names ",
      "in phenotypes do not match yaml config values"
    )
  }

  for (i in seq_len(length(config.data$variables))) {
    res$variables[[i]]$params <- config.data$variables[[i]]
  }
  res$derived <- config.data$derived
  res
}


#' @title
#' Apply transformed variable names to data frame
#'
#' @details
#' This function takes a data frame and the output of `map.header`, and applies
#' the variable name transformation to the data frame. Note that this must be run
#' after `map.header`, and without any column ordering
#' changes on the input phenotype data. If the order is changed, this sanitization
#' will fail silently.
#'
#' @description
#' Header remapping is designed to clean up input format discrepancies such as
#' non-standard characters, repeat values, etc. The worst we have seen is header emoji;
#' every time you don't have header emoji, take some time to feel grateful for that.
#'
#' As with `map.header`, this functionality is some of the oldest in the package.
#' It should not be called outside of the `create.phenotype.report` processing chain.
#'
#' @param df Data frame containing phenotype data with untransformed headers.
#' @param var.map List output of `map.header`, containing input dataset configuration
#' but with user settings per variable in params sublists.
#' @return Data frame containing input phenotype data with variable name transformation applied.
#' @seealso map.header
#' @usage NULL
sanitize.header <- function(df, var.map) {
  name.lookup <- names(var.map$variables)
  res <- df
  colnames(res) <- name.lookup
  res
}
