#' Create and map structured variable names to replace arbitrary input variable names
#'
#' @details
#' Input data frame may have any format variable names stored as column
#' headers; this function maps those to standardized format "TAG#####" names.
#' The new names are reported as names(result), and the old variable names
#' corresponding to those names are reported as result${newname}$original.name.
#'
#' @description
#'
#'
#' @param df data.frame, phenotype dataframe with untransformed headers
#' @param dataset.tag character vector, unique string tag for this dataset
#' @return list, mapping new variable names to lists; eventually meant to have
#' more entries but will for now just have a original.name entry for mapping back
#' to the raw variable names
#' @seealso sanitize.header
#' @keywords phenotypes
#' @export map.header
#' @examples
#' df <- data.frame(rnorm(100), runif(100))
#' colnames(df) <- c("Human height lol", "y")
#' map.header(df, "mytag")
map.header <- function(df, dataset.tag) {
  new.names <- sprintf("%s%05d", dataset.tag, seq_len(ncol(df)))
  res <- lapply(colnames(df), function(i) {
    list(original.name = i)
  })
  names(res) <- new.names
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
#' @export sanitize.header
#' @examples
#' df <- data.frame(rnorm(100), runif(100))
#' colnames(df) <- c("Human height lol", "y")
#' mapped.values <- map.header(df, "mytag")
#' df.transformed <- sanitize.header(df, mapped.values)
sanitize.header <- function(df, var.map) {
  name.lookup <- names(var.map)
  res <- df
  colnames(res) <- name.lookup
  res
}
