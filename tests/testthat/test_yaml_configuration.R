test_that("dataset parameters loaded from yaml and updated with shared models", {
  in.dataset.filename <- "files/yaml_configuration/dataset-parameters.yaml"
  in.shared.models <- "files/yaml_configuration/shared-models.yaml"

  out.list <- list(
    tag = "testset",
    globals = list(min_age_for_inclusion = as.integer(16)),
    variables = list(
      var001 = list(
        name = "PID",
        type = "string",
        subject_id = TRUE
      ),
      var013 = list(
        type = "binary",
        reference = "no",
        comparison = "yes",
        name = "Any associated goitre (neck swelling):",
        shared_model = "yes_no"
      )
    )
  )
  expect_identical(
    load.configuration(
      in.dataset.filename,
      in.shared.models
    ),
    out.list
  )
})
