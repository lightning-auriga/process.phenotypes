#' Load yaml configuration from file, integrating global models
#'
#' @description
#'
#' @details
#'
#' @param dataset.parameter.filename character vector, filename
#' of dataset-specific variable parameters
#' @param global.parameter.filename character vector, filename
#' of global shared model specifications
#' @return list of lists, containing parsed yaml configuration
#' information
#' @keywords yaml
#' @export load.configuration
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
  global.parameters <- yaml::read_yaml(global.parameter.filename)
  ## input format testing for global model parameters
  stopifnot(!is.null(global.parameters$models))
  ## read project-specific variable parameter information
  dataset.parameters <- yaml::read_yaml(dataset.parameter.filename)
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
      stopifnot(!is.null(out.model))
      ## apply dataset-specific values on top of any colliding variables in the
      ## global specification
      for (parameter.name in names(dataset.parameters$variables[[var.name]])) {
        out.model[[parameter.name]] <- dataset.parameters$variables[[var.name]][[parameter.name]]
      }
      dataset.parameters$variables[[var.name]] <- out.model
    }
  }
  ## return synthesized variable configuration data
  dataset.parameters
}
