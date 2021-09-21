#' Combine lists (or other named structure)
#' by overwriting specific entries in substructures
#'
#' @details
#' Configuration lists from yaml format should ideally
#' be merged by overwriting specific entries if present,
#' while leaving any other existing entries intact. This
#' doesn't appear to be available by default in R, so
#' this utility function does the required DFS to
#' enable this functionality.
#'
#' @param list1 list or vector, existing list/vector that will have
#' entries added into it
#' @param list2 list or vector, derived list/vector that will have
#' its entries added to the other
#' @return list or vector, combined version of input objects
#' with list1 default entries present unless they
#' are overridden by identically named entries
#' in list2, and any unique entries in list2
#' added on top.
combine.lists <- function(list1, list2) {
  ## note that this will need to be recursively called
  if (is.null(names(list1)) |
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
        if (length(list1[[name]]) > 1 |
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
