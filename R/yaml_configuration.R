#' @title
#' Load yaml configuration from file, integrating global models
#'
#' @description
#' This function loads user-specified yaml-format cleaning configurations
#' and resolves shared models for each variable. The resolution process
#' is somewhat finicky, but tries to provide the shared model as the basic
#' infrastructure with any variable-specific idiosyncrasies applied on top.
#'
#' @details
#' The shared model system is potentially very powerful, but at the
#' moment has not been particularly well used due to the manner in which
#' upstream variable definitions are shifting. Ultimately, some of the
#' cross-dataset harmony that was envisioned for shared models has moved
#' down into derived variable generation, where the discrepancies of
#' upstream modeling can be quite flexibly ironed out.
#'
#' @param dataset.parameter.filename Character vector filename
#' of dataset-specific variable parameters.
#' @param global.parameter.filename Character vector filename
#' of global shared model specifications.
#' @return List representation of input dataset yaml configuration,
#' with shared models resolved from the separate configuration file.
#' @keywords yaml
#' @examples
#' dataset.yaml <- system.file("examples", "example.dataset.yaml",
#'   package = "process.phenotypes"
#' )
#' shared.models <- system.file("examples", "example.shared_models.yaml",
#'   package = "process.phenotypes"
#' )
#' result <- process.phenotypes:::load.configuration(dataset.yaml, shared.models)
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

#' @title
#' Report final configuration data to file
#'
#' @description
#' Configuration data from the input configuratin files are modified on load,
#' and some information is lost. Before reporting, the variable
#' summary information from the run needs to be somewhat reformatted
#' into a version that is compatible with the input format. Additionally,
#' since derived variables are computed and added to the phenotype dataset,
#' the resolved (and appropriately ordered) entries from the derived variable
#' block need to be migrated into the primary variable configuration content.
#'
#' @details
#' At one time, this output file was intended to provide a way of reloading
#' the output phenotype data into process.phenotypes. That has proven somewhat
#' inconvenient, and we have moved in favor of rerunning `create.phenotype.report`
#' from scratch when any changes to the input configuration are required.
#' Nevertheless, there are potential applications for this file, and furthermore
#' it is appropriate to provide as detailed records of the processing chain
#' as possible, so this write operation remains available with the
#' `create.phenotype.report` `write.yaml` parameter.
#'
#' @param variable.summary List of configuration data from
#' create.phenotype.report run.
#' @param out.filename Character vector name of file
#' to which to write yaml-format configuration data.
#' @return NULL
#' @examples
#' variable.summary <- list(
#'   tag = "HW",
#'   globals = list(
#'     min_age_for_inclusion = 18,
#'     max_invalid_datatypes_per_subject = 10,
#'     consent_inclusion_file = NULL,
#'     consent_exclusion_file = NULL
#'   ),
#'   variables = list(
#'     HW00001 = list(params = list(
#'       name = "subjid",
#'       type = "string",
#'       subject_id = TRUE
#'     )),
#'     HW00002 = list(params = list(
#'       name = "subjage",
#'       type = "numeric",
#'       subject_age = TRUE
#'     ))
#'   )
#' )
#' out.filename <- tempfile("wc_output", fileext = ".yaml")
#' process.phenotypes:::write.configuration(variable.summary, out.filename)
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
