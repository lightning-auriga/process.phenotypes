#' Load yaml configuration from file, integrating global models
#'
#' @description
#' Load cleaning configurations from yaml file and apply
#' shared models.
#'
#' @param dataset.parameter.filename character vector, filename
#' of dataset-specific variable parameters
#' @param global.parameter.filename character vector, filename
#' of global shared model specifications
#' @return list of lists, containing parsed yaml configuration
#' information
#' @keywords yaml
load.configuration <- function(dataset.parameter.filename,
                               global.parameter.filename) {
  ## input parameter testing
  stopifnot(
    is.vector(dataset.parameter.filename, mode = "character"),
    length(dataset.parameter.filename) == 1
  )
  stopifnot(
    is.vector(global.parameter.filename, mode = "character"),
    length(global.parameter.filename) == 1
  )
  ## read global shared model information
  global.parameters <- yaml::read_yaml(global.parameter.filename,
    handlers = list(seq = function(x) x)
  )
  ## input format testing for global model parameters
  stopifnot(!is.null(global.parameters$models))
  ## read project-specific variable parameter information
  dataset.parameters <- yaml::read_yaml(dataset.parameter.filename,
    handlers = list(seq = function(x) x)
  )
  ## input format testing for project-specific variable parameters
  stopifnot(
    !is.null(dataset.parameters$tag),
    !is.null(dataset.parameters$globals),
    !is.null(dataset.parameters$variables)
  )
  ## apply shared model data when requested
  for (var.name in names(dataset.parameters$variables)) {
    requested.model <- dataset.parameters$variables[[var.name]][["shared_model"]]
    if (!is.null(requested.model)) {
      out.model <- global.parameters$models[[requested.model]]
      if (is.null(out.model)) {
        stop(
          "For variable ", var.name, ": requested shared model \"",
          requested.model, "\" not found in shared model configuration"
        )
      }
      ## apply dataset-specific values on top of any colliding variables in the
      ## global specification
      dataset.parameters$variables[[var.name]] <- combine.lists(
        out.model,
        dataset.parameters$variables[[var.name]]
      )
    }
  }
  for (var.name in names(dataset.parameters$derived)) {
    requested.model <- dataset.parameters$derived[[var.name]][["shared_model"]]
    if (!is.null(requested.model)) {
      out.model <- global.parameters$models[[requested.model]]
      if (is.null(out.model)) {
        stop(
          "For variable ", var.name, ": requested shared model \"",
          requested.model, "\" not found in shared model configuration"
        )
      }
      ## apply dataset-specific values on top of any colliding variables in the
      ## global specification
      dataset.parameters$derived[[var.name]] <- combine.lists(
        out.model,
        dataset.parameters$derived[[var.name]]
      )
    }
  }
  ## return synthesized variable configuration data
  dataset.parameters
}

#' Report final configuration data to file
#'
#' @description
#' Configuration data from input config.yaml are modified on load,
#' and some information is lost. Before reporting, the variable
#' summary information from the run needs to be somewhat reformatted
#' into a version that is compatible with the input format.
#'
#' @param variable.summary list, configuration data from
#' create.phenotype.report run
#' @param out.filename character vector, name of file
#' to which to write yaml-format configuration data
write.configuration <- function(variable.summary, out.filename) {
  stopifnot(is.list(variable.summary))
  stopifnot(is.vector(out.filename, mode = "character"))
  res <- list()
  for (name in names(variable.summary)) {
    if (name == "derived") {
      ## seemingly... just skip it?
      next
    } else if (name == "variables") {
      ## input configuration data have been placed in the
      ## params sublist; this needs to be rescued
      var.list <- list()
      for (var.name in names(variable.summary$variables)) {
        var.list[[var.name]] <- variable.summary$variables[[var.name]]$params
        ## have to update the variable name to reflect the standardized
        ## name injected by the program. but also have to save the original
        ## column name, because it tends to be rather informative
        if (is.null(var.list[[var.name]]$original.name)) {
          var.list[[var.name]]$original.name <- var.list[[var.name]]$name
        }
        var.list[[var.name]]$name <- var.name
      }
      res[["variables"]] <- var.list
    } else if (name %in% c("globals", "tag")) {
      res[[name]] <- variable.summary[[name]]
    }
  }
  yaml::write_yaml(res, out.filename,
    handlers = list(seq = function(x) x)
  )
}
