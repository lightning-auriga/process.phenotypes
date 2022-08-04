input.yaml.data <- list(
  globals = list("thing" = "otherthing"),
  variables = list(
    "HW00001" = list(
      "name" = "var1",
      "type" = "string",
      "canonical_name" = "name1"
    ),
    "HW00002" = list(
      "name" = "var2",
      "type" = "string",
      "canonical_name" = "name2"
    ),
    "HW00003_1" = list(
      "name" = "var3_1",
      "type" = "string",
      "canonical_name" = "name3, repeat observation 1"
    ),
    "HW00004_1" = list(
      "name" = "var4_1",
      "type" = "string",
      "canonical_name" = "name4, repeat observation 1"
    ),
    "HW00003_2" = list(
      "name" = "var3_2",
      "type" = "string",
      "canonical_name" = "name3, repeat observation 2",
      "suppress_reporting" = TRUE
    ),
    "HW00004_2" = list(
      "name" = "var4_2",
      "type" = "string",
      "canonical_name" = "name4, repeat observation 2",
      "suppress_reporting" = TRUE,
      "bounds" = list("max" = 5)
    ),
    "HW00005" = list(
      "name" = "var5",
      "type" = "string",
      "canonical_name" = "name5"
    )
  ),
  derived = list("myvar" = list(
    "name" = "myname",
    "type" = "string",
    "code" = "\"hello world\""
  ))
)

input.csv.data <- data.frame()
regenerated.yaml.data <- list(
  globals = list("thing" = "otherthing"),
  variables = list(
    "HW00001" = list(
      "name" = "var1",
      "type" = "string",
      "canonical_name" = "name1"
    ),
    "HW00002" = list(
      "name" = "var2",
      "type" = "string",
      "canonical_name" = "name2"
    ),
    "HW00003_1" = list(
      "name" = "var3_1",
      "type" = "string",
      "canonical_name" = "name3, repeat observation 1"
    ),
    "HW00004_1" = list(
      "name" = "var4_1",
      "type" = "string",
      "canonical_name" = "name4, repeat observation 1"
    ),
    "HW00003_2" = list(
      "name" = "var3_2",
      "type" = "string",
      "canonical_name" = "name3, repeat observation 2"
    ),
    "HW00004_2" = list(
      "name" = "var4_2",
      "type" = "string",
      "canonical_name" = "name4, repeat observation 2"
    ),
    "HW00003_3" = list(
      "name" = "var3_3",
      "type" = "string",
      "canonical_name" = "name3, repeat observation 3"
    ),
    "HW00004_3" = list(
      "name" = "var4_3",
      "type" = "string",
      "canonical_name" = "name4, repeat observation 3"
    ),
    "HW00005" = list(
      "name" = "var5",
      "type" = "string",
      "canonical_name" = "name5"
    ),
    "HW00006_1" = list(
      "name" = "var6_1",
      "type" = "string",
      "canonical_name" = "name6, repeat observation 1"
    ),
    "HW00007_1" = list(
      "name" = "var7_1",
      "type" = "string",
      "canonical_name"
    )
  )
)


test_that(paste("handle.missing.block successfully imports pre-constructed entry ",
  "from parse.surveycto output, in middle of existing config",
  sep = ""
), {
  varname <- "HW00001"
  max.existing.number <- as.integer(5)
  res.variables <- regenerated.yaml.data$variables
  res.variables[["HW00003_1"]] <- NULL
  res.variables[["HW00004_1"]] <- NULL
  res.variables[["HW00003_2"]] <- NULL
  res.variables[["HW00004_2"]] <- NULL
  res.variables[["HW00005"]] <- NULL
  res.variables[["HW00006_1"]] <- NULL
  res.variables[["HW00007_1"]] <- NULL
  missing.colname <- "var3_1"
  predicted.colname <- "var5"
  output <- handle.missing.block(
    varname, regenerated.yaml.data, max.existing.number,
    res.variables, missing.colname
  )
  expected.res.variables <- res.variables
  expected.res.variables[["HW00006_1"]] <- regenerated.yaml.data$variables[["HW00003_1"]]
  expected.max.existing.number <- 6
  expected <- list(
    res.variables = expected.res.variables,
    max.existing.number = expected.max.existing.number
  )
  expect_identical(output, expected)
})


test_that(paste("handle.missing.block successfully imports pre-constructed entry ",
  "from parse.surveycto output, at end of existing config",
  sep = ""
), {
  varname <- "HW00001"
  max.existing.number <- as.integer(5)
  res.variables <- regenerated.yaml.data$variables
  res.variables[["HW00003_2"]][["suppress_reporting"]] <- TRUE
  res.variables[["HW00004_2"]][["suppress_reporting"]] <- TRUE
  res.variables[["HW00006_1"]] <- NULL
  res.variables[["HW00007_1"]] <- NULL
  missing.colname <- "var6_1"
  predicted.colname <- "(end of existing input configuration yaml)"
  output <- handle.missing.block(
    varname, regenerated.yaml.data, max.existing.number,
    res.variables, missing.colname
  )
  expected.res.variables <- res.variables
  expected.res.variables[["HW00006_1"]] <- regenerated.yaml.data$variables[["HW00006_1"]]
  expected.max.existing.number <- 6
  expected <- list(
    res.variables = expected.res.variables,
    max.existing.number = expected.max.existing.number
  )
  expect_identical(output, expected)
})

test_that("handle.missing.block correctly errors when it finds a variable it cannot handle", {
  varname <- "HW00001"
  max.existing.number <- as.integer(5)
  res.variables <- regenerated.yaml.data$variables
  res.variables[["HW00003_2"]][["suppress_reporting"]] <- TRUE
  res.variables[["HW00004_2"]][["suppress_reporting"]] <- TRUE
  res.variables[["HW00006_1"]] <- NULL
  res.variables[["HW00007_1"]] <- NULL
  missing.colname <- "var6"
  predicted.colname <- "(end of existing input configuration yaml)"
  expect_error(handle.missing.block(
    regenerated.yaml.data, max.existing.number,
    res.variables, missing.colname
  ))
})

test_that("handle.existing.block extends existing repeat block", {
  max.existing.number <- as.integer(5)
  res.variables <- regenerated.yaml.data$variables
  res.variables[["HW00003_2"]][["suppress_reporting"]] <- TRUE
  res.variables[["HW00004_2"]][["suppress_reporting"]] <- TRUE
  res.variables[["HW00003_3"]] <- NULL
  res.variables[["HW00004_3"]] <- NULL
  res.variables[["HW00005"]] <- NULL
  res.variables[["HW00006_1"]] <- NULL
  res.variables[["HW00007_1"]] <- NULL
  missing.colname <- "var3_3"
  predicted.colname <- "var5"
  output <- handle.existing.block("var3_", 3, res.variables, missing.colname)
  expected <- c(
    res.variables[1:6],
    list("HW00003_3" = regenerated.yaml.data$variables[["HW00003_3"]])
  )
  expect_identical(output, expected)
})

test_that("handle.existing.block correctly errors when it finds a variable it cannot handle", {
  max.existing.number <- as.integer(5)
  res.variables <- regenerated.yaml.data$variables
  res.variables[["HW00003_2"]][["suppress_reporting"]] <- TRUE
  res.variables[["HW00004_2"]][["suppress_reporting"]] <- TRUE
  res.variables[["HW00003_3"]] <- NULL
  res.variables[["HW00004_3"]] <- NULL
  res.variables[["HW00005"]] <- NULL
  res.variables[["HW00006_1"]] <- NULL
  res.variables[["HW00007_1"]] <- NULL
  missing.colname <- "var20_1"
  predicted.colname <- "var5"
  expect_error(handle.existing.block("var20_", 3, res.variables, missing.colname))
})

test_that("generate.predicted.yaml manages to call process.phenotypes::parse.surveycto", {
  intermediate.dataset <- tempfile("example_cto_intermediate_dataset")
  intermediate.shared.models <- tempfile("example_cto_intermediate_shared_models")
  generate.predicted.yaml(
    yaml::read_yaml("files/expand_surveycto_config/example_cto_corrected_dataset.yaml"),
    "files/expand_surveycto_config/example_cto_form_repeat_appended.xlsx",
    "files/expand_surveycto_config/example_cto_problematic_dump.csv",
    intermediate.dataset,
    intermediate.shared.models
  )
  expect_true(file.exists(intermediate.dataset))
  expect_true(file.exists(intermediate.shared.models))
  output <- yaml::read_yaml(intermediate.dataset)
  expected <- yaml::read_yaml("files/expand_surveycto_config/example_cto_projected_dataset.yaml")
  expect_equal(output, expected)
})

test_that("expand.surveycto.config runs end-to-end and correctly adjusts config", {
  intermediate.dataset <- tempfile("example_cto_intermediate_dataset")
  intermediate.shared.models <- tempfile("example_cto_intermediate_shared_models")
  dataset.yaml <- tempfile("example_cto_dataset_yaml")
  current.form <- "files/expand_surveycto_config/example_cto_draft_reduced_dataset.yaml"
  current.data.pull <- "files/expand_surveycto_config/example_cto_problematic_dump.csv"
  current.data.merge <- "files/expand_surveycto_config/example_cto_problematic_merge.tsv"
  current.form.definition <- "files/expand_surveycto_config/example_cto_form_repeat_appended.xlsx"
  expand.surveycto.config(
    current.form,
    dataset.yaml,
    current.data.pull,
    current.form.definition,
    current.data.merge,
    intermediate.dataset,
    intermediate.shared.models
  )
  expect_true(file.exists(intermediate.dataset))
  expect_true(file.exists(intermediate.shared.models))
  expect_true(file.exists(dataset.yaml))
  output <- yaml::read_yaml(dataset.yaml)
  expected <- yaml::read_yaml("files/expand_surveycto_config/example_cto_corrected_dataset.yaml")
  expect_equal(output, expected)
})

test_that("expand_surveycto_config correctly errors when it finds a variable it cannot handle, at the end", {
  intermediate.dataset <- tempfile("example_cto_intermediate_dataset")
  intermediate.shared.models <- tempfile("example_cto_intermediate_shared_models")
  dataset.yaml <- tempfile("example_cto_dataset_yaml")
  current.form <- "files/expand_surveycto_config/example_cto_draft_reduced_dataset.yaml"
  current.data.pull <- "files/expand_surveycto_config/example_cto_dump.csv"
  current.data.merge <- "files/expand_surveycto_config/example_cto_merge.tsv"
  current.form.definition <- "files/expand_surveycto_config/example_cto_form.xlsx"
  expect_snapshot(expand.surveycto.config(
    current.form,
    dataset.yaml,
    current.data.pull,
    current.form.definition,
    current.data.merge,
    intermediate.dataset,
    intermediate.shared.models
  ),
  error = TRUE
  )
})

test_that("expand_surveycto_config correctly errors when it finds a variable it cannot handle, in the middle", {
  intermediate.dataset <- tempfile("example_cto_intermediate_dataset")
  intermediate.shared.models <- tempfile("example_cto_intermediate_shared_models")
  dataset.yaml <- tempfile("example_cto_dataset_yaml")
  current.form <- "files/expand_surveycto_config/example_cto_draft_reduced_dataset.yaml"
  current.data.pull <- "files/expand_surveycto_config/example_cto_dump_middle.csv"
  current.data.merge <- "files/expand_surveycto_config/example_cto_merge_middle.tsv"
  current.form.definition <- "files/expand_surveycto_config/example_cto_form.xlsx"
  expect_snapshot(expand.surveycto.config(
    current.form,
    dataset.yaml,
    current.data.pull,
    current.form.definition,
    current.data.merge,
    intermediate.dataset,
    intermediate.shared.models
  ),
  error = TRUE
  )
})
