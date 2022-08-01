#' Run schema validation for process.phenotypes configuration files
#'
#' The function returns NULL, and is run primarily for its side effect
#' of emitting verbose logging data about the manner of validation
#' error detected by the json_validate call.
#'
#' @param dataset.yaml character; name of input dataset yaml file
#' @param shared.models.yaml character; name of input shared models yaml file
#' @param dataset.schema character; name of schema specification for dataset yaml
#' @param shared.models.schema character; name of schema specification for shared models yaml
#' @export
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
