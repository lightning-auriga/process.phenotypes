#' Create and map structured variable names to replace arbitrary input variable names
#'
#' @details
#' Input data frame may have any format variable names stored as column
#' headers; this function maps those to standardized format "TAG#####" names.
#' The new names are reported as names(result), and the old variable names
#' corresponding to those names are reported as result${newname}$original.name.
#'
#' @description
#' Since the addition of yaml configuration, this function additionally grabs
#' any available configuration information for each variable and stores it as
#' a "params" list entry per variable.
#'
#' @param df data.frame, phenotype dataframe with untransformed headers
#' @param dataset.tag character vector, unique string tag for this dataset
#' @param config.data list, yaml configuration data per variable
#' @param force.header.mapping logical, whether you want to be kinda foolish
#' and allow desync between config variable names and dataset header data.
#' this safety check was implemented in part due to the possible presence of duplicate
#' column header names, and turning it off can potentially have catastrophically
#' bad effects if your configuration file is malformed.
#' @return list, mapping new variable names to lists; eventually meant to have
#' more entries but will for now just have a original.name entry for mapping back
#' to the raw variable names
#' @seealso sanitize.header
#' @keywords phenotypes
#' @examples
#' df <- data.frame(rnorm(100), runif(100))
#' colnames(df) <- c("Human height lol", "y")
#' map.header(df, "mytag")
map.header <- function(df, dataset.tag, config.data,
                       force.header.mapping = FALSE) {
  ## new: pull names from input config variable specification
  ## TODO: add yaml checker that makes sure these names
  ## are alphanumeric only
  new.names <- names(config.data$variables)
  res <- list(variables = lapply(colnames(df), function(i) {
    list(original.name = i)
  }))
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
  if (!identical(colnames(df), unname(unlist(config.names))) &
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
  res$globals <- config.data$globals
  res$derived <- config.data$derived
  res
}


#' Apply transformed variable names to data frame
#'
#' @details
#' This function takes a data frame and the output of `map.header`, and applies
#' the variable name transformation to the data frame.
#'
#' Note that this must be run after `map.header`, and without any column ordering
#' changes on the input phenotype data. If the order is changed, this sanitization
#' will fail silently.
#'
#' @description
#'
#' @param df data frame, phenotype dataframe with untransformed headers
#' @param var.map output of `map.header` for df
#' @return input data frame with variable name transformation applied
#' @seealso map.header
#' @keywords phenotypes
#' @examples
#' df <- data.frame(rnorm(100), runif(100))
#' colnames(df) <- c("Human height lol", "y")
#' mapped.values <- map.header(df, "mytag")
#' df.transformed <- sanitize.header(df, mapped.values)
sanitize.header <- function(df, var.map) {
  name.lookup <- names(var.map$variables)
  res <- df
  colnames(res) <- name.lookup
  res
}
