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
