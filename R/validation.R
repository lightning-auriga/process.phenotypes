#' @title
#' Run schema validation for process.phenotypes configuration files
#'
#' @description
#' This function takes json schema files specifying the permitted formats
#' of process.phenotypes dataset and shared model configuration files,
#' and uses them to validate a provided pair of user configuration files.
#' If the schema validation is successful, an acknowledgment message
#' will be printed for each input; if not, the embedded validator will
#' report a somewhat helpful summary of the issue to guide the process
#' of fixing the configuration.
#'
#' @details
#' This function replaces the old python validator that was inherited
#' from a very old version of this package, and that was removed some
#' time ago.
#'
#' The function returns NULL, and is run primarily for its side effect
#' of emitting verbose logging data about the manner of validation
#' error detected by the json_validate call.
#'
#' @param dataset.yaml character; name of input dataset yaml file
#' @param shared.models.yaml character; name of input shared models yaml file
#' @param dataset.schema character; name of schema specification for dataset yaml
#' @param shared.models.schema character; name of schema specification for shared models yaml
#' @return NULL
#' @export
#' @examples
#' dataset.yaml <- system.file("examples", "example.dataset.yaml",
#'   package = "process.phenotypes"
#' )
#' shared.models.yaml <- system.file("examples", "example.shared_models.yaml",
#'   package = "process.phenotypes"
#' )
#' dataset.schema <- system.file("validator", "schema.datasets.yaml",
#'   package = "process.phenotypes"
#' )
#' shared.models.schema <- system.file("validator", "schema.shared-models.yaml",
#'   package = "process.phenotypes"
#' )
#' process.phenotypes::config.validation(
#'   dataset.yaml,
#'   shared.models.yaml,
#'   dataset.schema,
#'   shared.models.schema
#' )
config.validation <- function(dataset.yaml,
                              shared.models.yaml,
                              dataset.schema,
                              shared.models.schema) {
  stopifnot(
    "dataset yaml does not exist" = file.exists(dataset.yaml),
    "shared models yaml does not exist" = file.exists(shared.models.yaml),
    "dataset schema does not exist" = file.exists(dataset.schema),
    "shared models schema does not exist" = file.exists(shared.models.schema)
  )
  ## validate shared models file
  shared.models.yaml.contents <- rjson::toJSON(yaml::yaml.load_file(shared.models.yaml,
    handlers = list(seq = function(x) x)
  ))
  shared.models.schema.contents <- rjson::toJSON(yaml::yaml.load_file(shared.models.schema,
    handlers = list(seq = function(x) x)
  ))
  shared.models.res <- jsonvalidate::json_validate(shared.models.yaml.contents,
    shared.models.schema.contents,
    error = TRUE,
    verbose = TRUE,
    engine = "ajv"
  )
  cat("File", shared.models.yaml, "passes schema validation!\n")

  ## validate dataset yaml
  dataset.yaml.contents <- rjson::toJSON(yaml::yaml.load_file(dataset.yaml,
    handlers = list(seq = function(x) x)
  ))
  dataset.schema.contents <- rjson::toJSON(yaml::yaml.load_file(dataset.schema,
    handlers = list(seq = function(x) x)
  ))
  dataset.res <- jsonvalidate::json_validate(dataset.yaml.contents,
    dataset.schema.contents,
    error = TRUE,
    verbose = TRUE,
    engine = "ajv"
  )
  cat("File", dataset.yaml, "passes schema validation!\n")
}
