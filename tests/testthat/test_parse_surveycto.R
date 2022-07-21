test_that("get.last.variable.number functions correctly", {
  in.variables <- list(
    HW00001 = list(),
    HW00002 = list(),
    HW00003_1 = list(),
    HW00003_1_1 = list(),
    HW00003_2_1 = list(),
    HW00003_2 = list(),
    HW00003_1_2 = list(),
    HW00003_2_2 = list(),
    HW00004 = list()
  )
  expected <- as.integer(4)
  output <- get.last.variable.number(in.variables)
  expect_identical(output, expected)
})

test_that("populate.choices functions correctly on mock surveycto choices tab", {
  in.df <- data.frame(
    "list_name" = c(
      rep("model1", 3),
      NA,
      rep("model2", 2),
      NA,
      rep("model3", 4),
      rep("model4", 3)
    ),
    "value" = c(
      "1", "2", "3",
      NA,
      "a", "b",
      NA,
      "lvl1", "lvl2", "lvl3", "lvl4",
      "lvl1", "lvl2", "lvl1"
    ),
    "label" = c(
      "val1", "val2", "val3",
      NA,
      "val4", "val5",
      NA,
      "val6", "val7", "val8", "val9",
      "val10", "val11", "val12"
    )
  )
  in.survey.type <- c(
    "begin group",
    "begin repeat",
    "select_one model1",
    "select_multiple model2",
    "select_one model3",
    "select_one model4",
    "end repeat",
    "end group"
  )
  in.na.values <- c("val8")
  expected <- list("models" = list(
    "model1" = list(
      "type" = "categorical",
      "levels" = list(
        "lvl1" = list(
          "name" = "val1",
          "alternate_patterns" = c("1", "1")
        ),
        "lvl2" = list(
          "name" = "val2",
          "alternate_patterns" = c("2", "2")
        ),
        "lvl3" = list(
          "name" = "val3",
          "alternate_patterns" = c("3", "3")
        )
      )
    ),
    "model2" = list(
      "type" = "categorical",
      "levels" = list(
        "lvl1" = list(
          "name" = "val4",
          "alternate_patterns" = c("a", "a")
        ),
        "lvl2" = list(
          "name" = "val5",
          "alternate_patterns" = c("b", "b")
        )
      )
    ),
    "model3" = list(
      "type" = "categorical",
      "levels" = list(
        "lvl1" = list(
          "name" = "val6",
          "alternate_patterns" = c("lvl1", "lvl1")
        ),
        "lvl2" = list(
          "name" = "val7",
          "alternate_patterns" = c("lvl2", "lvl2")
        ),
        "lvl3" = list(
          "name" = "val9",
          "alternate_patterns" = c("lvl4", "lvl4")
        )
      ),
      "na-values" = c("lvl3", "lvl3")
    ),
    "model4" = list(
      "type" = "categorical",
      "levels" = list(
        "lvl1" = list(
          "name" = "val10",
          "alternate_patterns" = c("lvl1", "lvl1")
        ),
        "lvl2" = list(
          "name" = "val11",
          "alternate_patterns" = c("lvl2", "lvl2")
        )
      )
    )
  ))
  output <- populate.choices(in.df, in.survey.type, in.na.values)
  expect_identical(output, expected)
})


test_that("populate.choices warns when impossible multiple choice configurations are present", {
  in.df <- data.frame(
    "list_name" = c(
      rep("model1", 3)
    ),
    "value" = c(
      "1", "2", "3"
    ),
    "label" = c(
      "val1", "val2", "val1"
    )
  )
  in.survey.type <- c(
    "select_multiple model1",
    "select_one model1"
  )
  in.na.values <- c()
  expected <- list("models" = list(
    "model1" = list(
      "type" = "categorical",
      "levels" = list(
        "lvl1" = list(
          "name" = "val1",
          "alternate_patterns" = c("1", "1")
        ),
        "lvl2" = list(
          "name" = "val2",
          "alternate_patterns" = c("2", "2")
        ),
        "lvl3" = list(
          "name" = "val1",
          "alternate_patterns" = c("3", "3")
        )
      )
    )
  ))
  expect_warning(expect_warning(output <- populate.choices(in.df, in.survey.type, in.na.values)))
  expect_identical(output, expected)
})

test_that("create.config creates correctly formatted initial placeholder yaml config", {
  expected.filename <- "files/parse_surveycto/initial_placeholder.yaml"
  expected <- yaml::read_yaml(expected.filename)
  output <- create.config("HW")
  expect_equal(output, expected)
})

test_that("handle.multiple.levels correctly expands existing yaml config with onehots", {
  choice.list <- list(models = list(
    "model1" = list(
      "type" = "categorical",
      "levels" = list(
        "1" = list(
          "name" = "lvl1",
          "alternate_patterns" = c("1", "1")
        ),
        "2" = list(
          "name" = "lvl2",
          "alternate_patterns" = c("2", "2")
        )
      )
    ),
    "model2" = list(
      "type" = "categorical",
      "levels" = list(
        "1" = list(
          "name" = "lvl3",
          "alternate_patterns" = c("1", "1")
        ),
        "2" = list(
          "name" = "lvl4",
          "alternate_patterns" = c("2", "2")
        ),
        "3" = list(
          "name" = "lvl5",
          "alternate_patterns" = c("3", "3")
        )
      )
    )
  ))
  shared.model <- "model2"
  varname <- "HW00003"
  name.value <- "my name"
  label.value <- "my value"
  res <- list("variables" = list(
    "HW00001" = list(
      "name" = "name1",
      "type" = "string"
    ),
    "HW00002" = list(
      "name" = "name2",
      "type" = "string"
    )
  ))
  output <- handle.multiple.levels(
    choice.list, shared.model, varname,
    name.value, label.value, res
  )
  expected <- res
  expected$variables[["HW00003_1"]] <- list(
    "name" = "my name_1",
    "shared_model" = "yesno",
    "canonical_name" = "my value, indicator response for level lvl3"
  )
  expected$variables[["HW00003_2"]] <- list(
    "name" = "my name_2",
    "shared_model" = "yesno",
    "canonical_name" = "my value, indicator response for level lvl4"
  )
  expected$variables[["HW00003_3"]] <- list(
    "name" = "my name_3",
    "shared_model" = "yesno",
    "canonical_name" = "my value, indicator response for level lvl5"
  )
  expect_equal(output, expected)
})

test_that("add.trailing.metadata appends SurveyCTO special metadata columns", {
  in.filename <- "files/parse_surveycto/add_trailing_metadata_input.yaml"
  out.filename <- "files/parse_surveycto/add_trailing_metadata_output.yaml"
  in.list <- yaml::read_yaml(in.filename)
  expected <- yaml::read_yaml(out.filename)
  output <- add.trailing.metadata(
    in.list,
    "HW",
    c(
      "instanceID",
      "instanceName",
      "formdef_version",
      "KEY",
      "review_quality",
      "review_comments",
      "review_status",
      "review_corrections"
    )
  )
  expect_equal(output, expected)
})

test_that("add.trailing.metadata respects missing trailing metadata columns", {
  in.filename <- "files/parse_surveycto/add_trailing_metadata_input.yaml"
  out.filename <- "files/parse_surveycto/add_trailing_metadata_output_reduced.yaml"
  in.list <- yaml::read_yaml(in.filename)
  expected <- yaml::read_yaml(out.filename)
  output <- add.trailing.metadata(
    in.list,
    "HW",
    c(
      "instanceID",
      "instanceName",
      "formdef_version",
      "KEY",
      "review_comments",
      "review_status",
      "review_corrections"
    )
  )
  expect_equal(output, expected)
})

bvd.string.example.gen <- function(stringtype) {
  type.value <- stringtype
  name.value <- paste("my_example_", stringtype, sep = "")
  label.value <- paste("my example ", stringtype, sep = "")
  choice.list <- list(models = list())
  varname <- "HW00002"
  output <- build.variable.data(
    type.value,
    name.value,
    label.value,
    choice.list,
    varname
  )
  expected <- list(variables = list("HW00002" = list(
    "name" = paste("my_example_", stringtype, sep = ""),
    "type" = "string",
    "suppress_reporting" = TRUE,
    "canonical_name" = paste("my example ", stringtype, sep = "")
  )))
  list(
    output = output,
    expected = expected
  )
}

test_that("build.variable.data primary functionality: start", {
  res <- bvd.string.example.gen("start")
  expect_equal(res$output, res$expected)
})

test_that("build.variable.data primary functionality: end", {
  res <- bvd.string.example.gen("end")
  expect_equal(res$output, res$expected)
})

test_that("build.variable.data primary functionality: deviceid", {
  res <- bvd.string.example.gen("deviceid")
  expect_equal(res$output, res$expected)
})

test_that("build.variable.data primary functionality: subscriberid", {
  res <- bvd.string.example.gen("subscriberid")
  expect_equal(res$output, res$expected)
})

test_that("build.variable.data primary functionality: simserial", {
  res <- bvd.string.example.gen("simserial")
  expect_equal(res$output, res$expected)
})

test_that("build.variable.data primary functionality: phonenumber", {
  res <- bvd.string.example.gen("phonenumber")
  expect_equal(res$output, res$expected)
})

test_that("build.variable.data primary functionality: username", {
  res <- bvd.string.example.gen("username")
  expect_equal(res$output, res$expected)
})

test_that("build.variable.data primary functionality: caseid", {
  res <- bvd.string.example.gen("caseid")
  expect_equal(res$output, res$expected)
})

test_that("build.variable.data primary functionality: image", {
  res <- bvd.string.example.gen("image")
  expect_equal(res$output, res$expected)
})

test_that("build.variable.data primary functionality: text", {
  res <- bvd.string.example.gen("text")
  expect_equal(res$output, res$expected)
})

test_that("build.variable.data primary functionality: datetime", {
  res <- bvd.string.example.gen("datetime")
  expect_equal(res$output, res$expected)
})

test_that("build.variable.data primary functionality: group boundaries", {
  expect_null(build.variable.data(
    "begin group", "placeholder_name",
    "placeholder_label", list(models = list()), "HW00001"
  ))
  expect_null(build.variable.data(
    "end group", "placeholder_name",
    "placeholder_label", list(models = list()), "HW00001"
  ))
})

test_that("build.variable.data primary functionality: NA type", {
  expect_null(build.variable.data(
    NA, "placeholder_name",
    "placeholder_label", list(models = list()), "HW00001"
  ))
})

test_that("build.variable.data primary functionality: empty type vector", {
  expect_null(build.variable.data(
    c(), "placeholder_name",
    "placeholder_label", list(models = list()), "HW00001"
  ))
})

test_that("build.variable.data primary functionality: note", {
  expect_null(build.variable.data(
    "note", "placeholder_name",
    "placeholder_label", list(models = list()), "HW00001"
  ))
})

test_that("build.variable.data primary functionality: calculate", {
  ## override simple generator function
  res <- bvd.string.example.gen("calculate")
  res$expected$variables$HW00002$suppress_reporting <- NULL
  expect_equal(res$output, res$expected)
})

test_that("build.variable.data primary functionality: date", {
  ## override simple generator function
  res <- bvd.string.example.gen("date")
  res$expected$variables$HW00002$type <- "date"
  res$expected$variables$HW00002$suppress_reporting <- NULL
  expect_equal(res$output, res$expected)
})

test_that("build.variable.data primary functionality: integer", {
  ## override simple generator function
  res <- bvd.string.example.gen("integer")
  res$expected$variables$HW00002$type <- "numeric"
  res$expected$variables$HW00002$suppress_reporting <- NULL
  expect_equal(res$output, res$expected)
})

test_that("build.variable.data primary functionality: decimal", {
  ## override simple generator function
  res <- bvd.string.example.gen("decimal")
  res$expected$variables$HW00002$type <- "numeric"
  res$expected$variables$HW00002$suppress_reporting <- NULL
  expect_equal(res$output, res$expected)
})

test_that("build.variable.data primary functionality: unrecognized type flag", {
  expect_warning(output <- build.variable.data(
    "FAKENAME", "placeholder_name",
    "placeholder_label", list(models = list()), "HW00001"
  ))
  expect_true(is.null(output))
})

test_that("build.variable.data primary functionality: select_one", {
  type.value <- "select_one mymodel"
  name.value <- "my_example_select_one"
  label.value <- "my example select_one"
  choice.list <- list(models = list())
  varname <- "HW00002"
  output <- build.variable.data(
    type.value,
    name.value,
    label.value,
    choice.list,
    varname
  )
  expected <- list(variables = list("HW00002" = list(
    "name" = "my_example_select_one",
    "shared_model" = "mymodel",
    "canonical_name" = "my example select_one"
  )))
  expect_equal(output, expected)
})

test_that("build.variable.data primary functionality: select_multiple", {
  type.value <- "select_multiple mymodel"
  name.value <- "my_example_select_multiple"
  label.value <- "my example select_multiple"
  choice.list <- list(models = list("mymodel" = list(
    "type" = "categorical",
    "levels" = list(
      "1" = list(
        "name" = "lvl1",
        "alternate_patterns" = c("1", "1")
      ),
      "2" = list(
        "name" = "lvl2",
        "alternate_patterns" = c("2", "2")
      )
    )
  )))
  varname <- "HW00002"
  output <- build.variable.data(
    type.value,
    name.value,
    label.value,
    choice.list,
    varname
  )
  expected <- list(variables = list(
    "HW00002" = list(
      "name" = "my_example_select_multiple",
      "type" = "string",
      "suppress_reporting" = TRUE,
      "canonical_name" = "my example select_multiple"
    ),
    "HW00002_1" = list(
      "name" = "my_example_select_multiple_1",
      "shared_model" = "yesno",
      "canonical_name" = "my example select_multiple, indicator response for level lvl1"
    ),
    "HW00002_2" = list(
      "name" = "my_example_select_multiple_2",
      "shared_model" = "yesno",
      "canonical_name" = "my example select_multiple, indicator response for level lvl2"
    )
  ))
  expect_equal(output, expected)
})
