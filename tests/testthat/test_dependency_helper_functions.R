test_that("variables that require a yes in a related variable are correctly flagged", {
  in.phenotype.data <- data.frame(
    "subject" = c("A", "B", "C", "D", "E", "F"),
    "PPB001" = c(NA, "yes", "no", "no", "yes", "yes"),
    "PPB002" = c(NA, NA, NA, "yes", "yes", "no")
  )
  in.variable.summary <- list(
    variables = list(
      "subject" = list(params = list(
        name = "subject",
        type = "string",
        subject_id = TRUE
      )),
      "PPB001" = list(params = list(
        name = "thing1",
        type = "string"
      )),
      "PPB002" = list(params = list(
        name = "thing2",
        type = "string",
        dependencies = list(
          "1" = list(
            name = "test1",
            condition = "response.depends.on.yes(PPB001, PPB002)"
          )
        )
      ))
    )
  )
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables[["PPB002"]]$dependency.results[["1"]] <- c("B", "C", "F")
  expect_identical(
    check.variable.dependencies(in.phenotype.data, in.variable.summary),
    out.variable.summary
  )
})

test_that("variables that require a yes in a related variable are correctly flagged; 'no' answers are treated as NA", {
  in.phenotype.data <- data.frame(
    "subject" = c("A", "B", "C", "D", "E", "F"),
    "PPB001" = c(NA, "yes", "no", "no", "yes", "yes"),
    "PPB002" = c(NA, NA, NA, "yes", "yes", "no")
  )
  in.variable.summary <- list(
    variables = list(
      "subject" = list(params = list(
        name = "subject",
        type = "string",
        subject_id = TRUE
      )),
      "PPB001" = list(params = list(
        name = "thing1",
        type = "string"
      )),
      "PPB002" = list(params = list(
        name = "thing2",
        type = "string",
        dependencies = list(
          "1" = list(
            name = "test1",
            condition = "response.depends.on.yes(PPB001, PPB002, allow.no = TRUE)"
          )
        )
      ))
    )
  )
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables[["PPB002"]]$dependency.results[["1"]] <- c("B", "F")
  expect_identical(
    check.variable.dependencies(in.phenotype.data, in.variable.summary),
    out.variable.summary
  )
})

test_that("variables that require a non-NA response in a related variable are correctly flagged", {
  in.phenotype.data <- data.frame(
    "subject" = c("A", "B", "C", "D", "E", "F"),
    "PPB001" = c(NA, "yes", "no", "no", "yes", "yes"),
    "PPB002" = c(NA, NA, NA, "yes", "yes", "no")
  )
  in.variable.summary <- list(
    variables = list(
      "subject" = list(params = list(
        name = "subject",
        type = "string",
        subject_id = TRUE
      )),
      "PPB001" = list(params = list(
        name = "thing1",
        type = "string"
      )),
      "PPB002" = list(params = list(
        name = "thing2",
        type = "string",
        dependencies = list(
          "1" = list(
            name = "test1",
            condition = "response.depends.on.not.na(PPB001, PPB002)"
          )
        )
      ))
    )
  )
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables[["PPB002"]]$dependency.results[["1"]] <- c("B", "C")
  expect_identical(
    check.variable.dependencies(in.phenotype.data, in.variable.summary),
    out.variable.summary
  )
})

test_that("variables that require a non-NA response in a related variable are correctly flagged;
'no' answers are treated as NA", {
  in.phenotype.data <- data.frame(
    "subject" = c("A", "B", "C", "D", "E", "F"),
    "PPB001" = c(NA, "yes", "no", "no", "yes", "yes"),
    "PPB002" = c(NA, NA, NA, "yes", "yes", "no")
  )
  in.variable.summary <- list(
    variables = list(
      "subject" = list(params = list(
        name = "subject",
        type = "string",
        subject_id = TRUE
      )),
      "PPB001" = list(params = list(
        name = "thing1",
        type = "string"
      )),
      "PPB002" = list(params = list(
        name = "thing2",
        type = "string",
        dependencies = list(
          "1" = list(
            name = "test1",
            condition = "response.depends.on.not.na(PPB001, PPB002, allow.no = TRUE)"
          )
        )
      ))
    )
  )
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables[["PPB002"]]$dependency.results[["1"]] <- c("B")
  expect_identical(
    check.variable.dependencies(in.phenotype.data, in.variable.summary),
    out.variable.summary
  )
})

test_that("variables that require a non-NA response in a related variable are correctly flagged;
specified answers are treated as NA", {
  in.phenotype.data <- data.frame(
    "subject" = c("A", "B", "C", "D", "E", "F"),
    "PPB001" = c(NA, "yes", "no", "no", "yes", "yes"),
    "PPB002" = c(NA, NA, NA, "yes", "0 times", "no")
  )
  in.variable.summary <- list(
    variables = list(
      "subject" = list(params = list(
        name = "subject",
        type = "string",
        subject_id = TRUE
      )),
      "PPB001" = list(params = list(
        name = "thing1",
        type = "string"
      )),
      "PPB002" = list(params = list(
        name = "thing2",
        type = "string",
        dependencies = list(
          "1" = list(
            name = "test1",
            condition = "response.depends.on.not.na(PPB001, PPB002, additional.na.levels = '0 times')"
          )
        )
      ))
    )
  )
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables[["PPB002"]]$dependency.results[["1"]] <- c("B", "C", "E")
  expect_identical(
    check.variable.dependencies(in.phenotype.data, in.variable.summary),
    out.variable.summary
  )
})

test_that("variables that are required to be less than a related variable are correctly flagged", {
  in.phenotype.data <- data.frame(
    "subject" = c("A", "B", "C", "D", "E"),
    "PPB001" = c(1, 2, 3, 4, 5),
    "PPB002" = c(1, 4, 1, 2, 6)
  )
  in.variable.summary <- list(
    variables = list(
      "subject" = list(params = list(
        name = "subject",
        type = "string",
        subject_id = TRUE
      )),
      "PPB001" = list(params = list(
        name = "thing1",
        type = "string"
      )),
      "PPB002" = list(params = list(
        name = "thing2",
        type = "string",
        dependencies = list(
          "1" = list(
            name = "test1",
            condition = "response.is.less.than(PPB001, PPB002)"
          )
        )
      ))
    )
  )
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables[["PPB002"]]$dependency.results[["1"]] <- c("A", "C", "D")
  expect_identical(
    check.variable.dependencies(in.phenotype.data, in.variable.summary),
    out.variable.summary
  )
})
