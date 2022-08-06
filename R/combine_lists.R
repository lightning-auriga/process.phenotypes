#' @title
#' Combine lists (or other named structure)
#' by overwriting specific entries in substructures.
#'
#' @description
#' Configuration lists from yaml format should ideally
#' be merged by overwriting specific entries if present,
#' while leaving any other existing entries intact. Conflicting
#' entries are prioritized from the second list provided.
#'
#' @details
#' The intention of this merge is to allow shared model data
#' to be seamlessly applied on top of whatever has been specified
#' for a variable-specific configuration.
#' This functionality doesn't appear to be available by default, so
#' this utility function does the required DFS.
#'
#' The exact behavior of this inheritance between lists is somewhat
#' underexplored: certain types of values still do seem to
#' be intermittently overwritten based on what is not specified
#' in the shared model.
#'
#' @param list1 List or vector that will have
#' entries added into it; intended to be a dataset-specific
#' variable configuration block.
#' @param list2 List or vector that will have
#' its entries added to the other; intended to be a shared
#' model specification.
#' @return List or vector representing combined version of
#' input objects, with list1 default entries present unless they
#' are overridden by identically named entries
#' in list2, and any unique entries in list2
#' added on top.
#' @examples
#' var.specific <- list(
#'   name = "myname",
#'   shared_model = "model1",
#'   canonical_name = "variable description"
#' )
#' shared.model <- list(
#'   type = "categorical",
#'   levels = list(
#'     "1" = list(name = "yes"),
#'     "2" = list(name = "no")
#'   )
#' )
#' resolved.config <- process.phenotypes:::combine.lists(var.specific, shared.model)
combine.lists <- function(list1, list2) {
  ## note that this will need to be recursively called
  if (is.null(names(list1)) ||
    is.null(names(list2))) {
    return(list2)
  }
  for (i in seq_len(length(list2))) {
    if (names(list2)[i] == "") {
      ## unnamed entries are appended
      list1[[length(list1) + 1]] <- list2[[i]]
    } else {
      ## named entries are updated in place if present,
      ## and otherwise appended
      name <- names(list2)[i]
      if (!is.null(list1[[name]])) {
        if (length(list1[[name]]) > 1 ||
          length(list2[[name]]) > 1) {
          list1[[name]] <- combine.lists(
            list1[[name]],
            list2[[name]]
          )
        } else {
          list1[[name]] <- list2[[name]]
        }
      } else {
        list1[[name]] <- list2[[name]]
      }
    }
  }
  list1
}
