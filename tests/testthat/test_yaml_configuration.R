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
    ),
    derived = list(
      var013_derived = list(
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


test_that("load.configuration correctly errors when variable shared model missing from shared models", {
  in.dataset.filename <- "files/yaml_configuration/dataset-parameters.yaml"
  in.shared.models <- "files/yaml_configuration/shared-models.yaml"
  ## primary and derived variables share same model in simple example; need
  ## to modify this to test error handling
  replacement.dataset.yaml <- tempfile("replacement_dataset_yaml")
  data <- yaml::read_yaml(in.dataset.filename)
  data$variables$var013$shared_model <- "missing_no"
  yaml::write_yaml(data, replacement.dataset.yaml)
  expect_error(load.configuration(
    replacement.dataset.yaml,
    in.shared.models
  ))
})

test_that("load.configuration correctly errors when derived shared model missing from shared models", {
  in.dataset.filename <- "files/yaml_configuration/dataset-parameters.yaml"
  in.shared.models <- "files/yaml_configuration/shared-models.yaml"
  ## primary and derived variables share same model in simple example; need
  ## to modify this to test error handling
  replacement.dataset.yaml <- tempfile("replacement_dataset_yaml")
  data <- yaml::read_yaml(in.dataset.filename)
  data$derived$var013_derived$shared_model <- "missing_no"
  yaml::write_yaml(data, replacement.dataset.yaml)
  expect_error(load.configuration(
    replacement.dataset.yaml,
    in.shared.models
  ))
})

test_that("write.configuration emits a final config yaml with derived variables appended", {
  in.list <- list(
    tag = "testset",
    globals = list(min_age_for_inclusion = as.integer(16)),
    variables = list(
      var001 = list(
        name = "PID",
        type = "string",
        subject_id = TRUE,
        params = list(
          name = "PID",
          type = "string",
          subject_id = TRUE
        )
      ),
      var013 = list(
        type = "binary",
        reference = "no",
        comparison = "yes",
        name = "Any associated goitre (neck swelling):",
        shared_model = "yes_no",
        params = list(
          type = "binary",
          reference = "no",
          comparison = "yes",
          name = "Any associated goitre (neck swelling):",
          shared_model = "yes_no"
        )
      ),
      var013_derived = list(
        type = "binary",
        reference = "no",
        comparison = "yes",
        name = "Any associated goitre (neck swelling):",
        shared_model = "yes_no",
        params = list(
          type = "binary",
          reference = "no",
          comparison = "yes",
          name = "Any associated goitre (neck swelling):",
          shared_model = "yes_no"
        )
      )
    ),
    derived = list(
      var013_derived = list(
        type = "binary",
        reference = "no",
        comparison = "yes",
        name = "Any associated goitre (neck swelling):",
        shared_model = "yes_no",
        params = list(
          type = "binary",
          reference = "no",
          comparison = "yes",
          name = "Any associated goitre (neck swelling):",
          shared_model = "yes_no"
        )
      )
    )
  )
  out.list <- list(
    tag = "testset",
    globals = list(min_age_for_inclusion = as.integer(16)),
    variables = list(
      var001 = list(
        name = "var001",
        type = "string",
        subject_id = TRUE,
        original.name = "PID"
      ),
      var013 = list(
        type = "binary",
        reference = "no",
        comparison = "yes",
        name = "var013",
        shared_model = "yes_no",
        original.name = "Any associated goitre (neck swelling):"
      ),
      var013_derived = list(
        type = "binary",
        reference = "no",
        comparison = "yes",
        name = "var013_derived",
        shared_model = "yes_no",
        original.name = "Any associated goitre (neck swelling):"
      )
    )
  )
  out.filename <- tempfile("write_configuration")
  write.configuration(in.list, out.filename)
  expect_true(file.exists(out.filename))
  output <- yaml::read_yaml(out.filename)
  expect_equal(output, out.list)
})
