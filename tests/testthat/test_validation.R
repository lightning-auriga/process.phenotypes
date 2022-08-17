dataset.yaml.valid <- "files/validation/dataset-valid.yaml"
dataset.yaml.invalid <- "files/validation/dataset-invalid.yaml"
shared.models.yaml.valid <- "files/validation/shared-models-valid.yaml"
shared.models.yaml.invalid <- "files/validation/shared-models-invalid.yaml"
dataset.schema <- system.file("validator/schema.datasets.yaml",
  package = "process.phenotypes"
)
shared.models.schema <- system.file("validator/schema.shared-models.yaml",
  package = "process.phenotypes"
)

test_that("validator recognizes acceptable dataset and shared models yaml", {
  skip_if(dataset.schema == "")
  expect_output(output <- config.validation(
    dataset.yaml.valid,
    shared.models.yaml.valid,
    dataset.schema,
    shared.models.schema
  ),
  regexp = paste("File ", shared.models.yaml.valid, " passes schema validation!\n",
    "File ", dataset.yaml.valid, " passes schema validation!",
    sep = ""
  )
  )
  expect_null(output)
})

test_that("validator recognizes unacceptable dataset yaml", {
  skip_if(dataset.schema == "")
  expect_snapshot(config.validation(
    dataset.yaml.invalid,
    shared.models.yaml.valid,
    dataset.schema,
    shared.models.schema
  ),
  error = TRUE
  )
})


test_that("validator recognizes unacceptable shared models yaml", {
  skip_if(shared.models.schema == "")
  expect_snapshot(config.validation(
    dataset.yaml.valid,
    shared.models.yaml.invalid,
    dataset.schema,
    shared.models.schema
  ),
  error = TRUE
  )
})
