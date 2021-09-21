test_that("unit test that dependency on yes is assessed as expected", {
  dependent.variable <- c("yes", NA, "no", "yes")
  independent.variable <- c("yes", "no", NA, "no")
  result <- c(TRUE, TRUE, FALSE, FALSE)
  expect_identical(
    response.depends.on.yes(dependent.variable, independent.variable),
    result
  )
})

test_that("unit test that dependency on yes is assessed as expected; 'no' answers treated as NA", {
  dependent.variable <- c("yes", NA, "no", "yes")
  independent.variable <- c("yes", "no", NA, "no")
  result <- c(TRUE, TRUE, TRUE, FALSE)
  expect_identical(
    response.depends.on.yes(dependent.variable, independent.variable, allow.no = TRUE),
    result
  )
})

test_that("unit test that dependency on yes is assessed as expected; specified answers treated as NA", {
  dependent.variable <- c("yes", NA, "no", "yes")
  independent.variable <- c("yes", "no", "zero", "no")
  result <- c(TRUE, TRUE, TRUE, FALSE)
  expect_identical(
    response.depends.on.yes(dependent.variable, independent.variable, additional.na.levels = "zero"),
    result
  )
})

test_that("unit test that dependency on yes is assessed as expected, xfail unequal vector lengths", {
  dependent.variable <- c("yes", NA, "no", "yes")
  independent.variable <- c("yes", "no", NA)
  expect_error(
    response.depends.on.yes(dependent.variable, independent.variable),
  )
})

test_that("unit test that dependency on yes is assessed as expected, xfail vector length < 1", {
  dependent.variable <- c()
  independent.variable <- c()
  expect_error(
    response.depends.on.yes(dependent.variable, independent.variable),
  )
})

test_that("unit test that dependency on yes is assessed as expected, xfail allow.no not a logical", {
  dependent.variable <- c("yes", NA, "no", "yes")
  independent.variable <- c("yes", "no", NA, "no")
  expect_error(
    response.depends.on.yes(dependent.variable, independent.variable, allow.no = "HAHA"),
  )
})

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

test_that("variables that require a yes in a related variable are correctly flagged;
specified answers are treated as NA", {
  in.phenotype.data <- data.frame(
    "subject" = c("A", "B", "C", "D", "E", "F"),
    "PPB001" = c(NA, "yes", "no", "no", "yes", "yes"),
    "PPB002" = c(NA, NA, "0 times", "yes", "yes", "no")
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
            condition = "response.depends.on.yes(PPB001, PPB002, additional.na.levels = '0 times')"
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

test_that("unit test that dependency on not-NA is assessed as expected", {
  dependent.variable <- c("yes", NA, "no", "yes")
  independent.variable <- c("yes", "no", NA, "no")
  result <- c(TRUE, TRUE, FALSE, TRUE)
  expect_identical(
    response.depends.on.not.na(dependent.variable, independent.variable),
    result
  )
})

test_that("unit test that dependency on not-NA is assessed as expected; 'no' answers treated as NA", {
  dependent.variable <- c("yes", NA, "no", "yes")
  independent.variable <- c("yes", "no", NA, "no")
  result <- c(TRUE, TRUE, TRUE, TRUE)
  expect_identical(
    response.depends.on.not.na(dependent.variable, independent.variable, allow.no = TRUE),
    result
  )
})

test_that("unit test that dependency on not-NA is assessed as expected; specified answers treated as NA", {
  dependent.variable <- c("yes", NA, "no", "yes")
  independent.variable <- c("yes", "no", "zero", "no")
  result <- c(TRUE, TRUE, FALSE, TRUE)
  expect_identical(
    response.depends.on.not.na(dependent.variable, independent.variable, additional.na.levels = "zero"),
    result
  )
})

test_that("unit test that dependency on not-NA is assessed as expected, xfail unequal vector lengths", {
  dependent.variable <- c("yes", NA, "no", "yes")
  independent.variable <- c("yes", "no", NA)
  expect_error(
    response.depends.on.not.na(dependent.variable, independent.variable),
  )
})

test_that("unit test that dependency on not-NA is assessed as expected, xfail vector length < 1", {
  dependent.variable <- c()
  independent.variable <- c()
  expect_error(
    response.depends.on.not.na(dependent.variable, independent.variable),
  )
})

test_that("unit test that dependency on not-NA is assessed as expected, xfail allow.no not a logical", {
  dependent.variable <- c("yes", NA, "no", "yes")
  independent.variable <- c("yes", "no", NA, "no")
  expect_error(
    response.depends.on.not.na(dependent.variable, independent.variable, allow.no = "HAHA"),
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

test_that("unit test that dependency on less-than is assessed as expected", {
  dependent.variable <- c(1, 2, 3, 4)
  independent.variable <- c(2, 1, 3, 5)
  result <- c(TRUE, FALSE, FALSE, TRUE)
  expect_identical(
    response.is.less.than(dependent.variable, independent.variable),
    result
  )
})

test_that("unit test that dependency on less-than is assessed as expected, xfail unequal vector lengths", {
  dependent.variable <- c(1, 2, 3)
  independent.variable <- c(1, 2)
  expect_error(
    response.is.less.than(dependent.variable, independent.variable),
  )
})

test_that("unit test that dependency on less-than is assessed as expected, xfail vector length < 1", {
  dependent.variable <- c()
  independent.variable <- c()
  expect_error(
    response.is.less.than(dependent.variable, independent.variable),
  )
})

test_that("unit test that dependency on less-than is assessed as expected, xfail vector length == 1 and is NA", {
  dependent.variable <- c(NA)
  independent.variable <- c(NA)
  expect_error(
    response.is.less.than(dependent.variable, independent.variable),
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

test_that("unit test that duplicate concordance is assessed as expected", {
  dependent.variable <- c("a", "b", "c", NA)
  independent.variable <- c("z", "b", NA, "d")
  result <- c(FALSE, TRUE, TRUE, TRUE)
  expect_identical(
    response.is.duplicate.of(dependent.variable, independent.variable),
    result
  )
})

test_that("unit test that duplicate concordance is assessed as expected, xfail unequal length vectors", {
  dependent.variable <- c("a", "b", "c")
  independent.variable <- c("z", "b", NA, "d")
  expect_error(
    response.is.duplicate.of(dependent.variable, independent.variable),
  )
})

test_that("unit test that duplicate concordance is assessed as expected, xfail unequal classes", {
  dependent.variable <- c("a", "b", "c")
  independent.variable <- c(1, 2, 3)
  expect_error(
    response.is.duplicate.of(dependent.variable, independent.variable),
  )
})

test_that("duplicate variable concordance is correctly flagged", {
  in.phenotype.data <- data.frame(
    "subject" = c("A", "B", "C", "D", "E"),
    "PPB001" = c("this", "is", "a", "duplicated", "variable"),
    "PPB002" = c("this", "is not", "a", "nope", "variable")
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
            condition = "response.is.duplicate.of(PPB001, PPB002)"
          )
        )
      ))
    )
  )
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables[["PPB002"]]$dependency.results[["1"]] <- c("B", "D")
  expect_identical(
    check.variable.dependencies(in.phenotype.data, in.variable.summary),
    out.variable.summary
  )
})

test_that("unit test that age and year of birth consistency is assessed as expected", {
  dependent.variable <- c(1990, 1982, 1950, 2000, NA)
  independent.variable <- c(32, 34, 22, NA, 18)
  result <- c(TRUE, TRUE, FALSE, TRUE, TRUE)
  expect_identical(
    year.is.consistent.with.age(dependent.variable, independent.variable, 2020, 5),
    result
  )
})

test_that("unit test that age and year of birth consistency is assessed as expected, xfail non-numeric", {
  dependent.variable <- c("a", "b", "c")
  independent.variable <- c("z", "b", NA)
  expect_error(
    year.is.consistent.with.age(dependent.variable, independent.variable, 2020, 5),
  )
})

test_that("unit test that age and year of birth consistency is assessed as expected,
          xfail non-numeric reference year", {
  dependent.variable <- c("a", "b", "c")
  independent.variable <- c("z", "b", NA)
  expect_error(
    year.is.consistent.with.age(dependent.variable, independent.variable, NA, 5),
  )
})

test_that("unit test that age and year of birth consistency is assessed as expected,
          xfail non-numeric tolerance", {
  dependent.variable <- c("a", "b", "c")
  independent.variable <- c("z", "b", NA)
  expect_error(
    year.is.consistent.with.age(dependent.variable, independent.variable, 2020, "tol"),
  )
})

test_that("unit test that age and year of birth consistency is assessed as expected, xfail non-numeric NA", {
  dependent.variable <- c(4, 5, 6)
  independent.variable <- c(1, 2, 3)
  expect_error(
    year.is.consistent.with.age(dependent.variable, independent.variable, as.numeric(NA), 5),
  )
})

test_that("unit test that age and year of birth consistency is assessed as expected, xfail unequal length vectors", {
  dependent.variable <- c(1, 2, 3)
  independent.variable <- c(1, 2)
  expect_error(
    year.is.consistent.with.age(dependent.variable, independent.variable, 2020, 5),
  )
})

test_that("unit test that age and year of birth consistency is assessed as expected, xfail unequal classes", {
  dependent.variable <- c("a", "b", "c")
  independent.variable <- c(1, 2, 3)
  expect_error(
    year.is.consistent.with.age(dependent.variable, independent.variable, 2020, 5),
  )
})

test_that("consistency of age and year of birth is correctly flagged", {
  in.phenotype.data <- data.frame(
    "subject" = c("A", "B", "C", "D", "E"),
    "PPB001" = c(1990, 1982, 1950, 2000, NA),
    "PPB002" = c(32, 34, 22, NA, 18)
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
            condition = "year.is.consistent.with.age(PPB001, PPB002, 2020, 5)"
          )
        )
      ))
    )
  )
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables[["PPB002"]]$dependency.results[["1"]] <- c("C")
  expect_identical(
    check.variable.dependencies(in.phenotype.data, in.variable.summary),
    out.variable.summary
  )
})

test_that("unit test that age and year of birth consistency is assessed as expected,
          using a vector for reference.year", {
  dependent.variable <- c(1990, 1982, 1950, 2000, NA)
  independent.variable <- c(32, 34, 22, NA, 18)
  reference.year <- c(2020, 2020, 2020, 2020, NA)
  result <- c(TRUE, TRUE, FALSE, TRUE, TRUE)
  expect_identical(
    year.is.consistent.with.age(dependent.variable, independent.variable, reference.year, 5),
    result
  )
})

test_that("consistency of age and year of birth is correctly flagged, using a vector for reference.year", {
  in.phenotype.data <- data.frame(
    "subject" = c("A", "B", "C", "D", "E"),
    "PPB001" = c(1990, 1982, 1950, 2000, NA),
    "PPB002" = c(32, 34, 22, NA, 18),
    "PPB003" = c(2020, 2020, 2020, 2020, 2020)
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
            condition = "year.is.consistent.with.age(PPB001, PPB002, PPB003, 5)"
          )
        )
      )),
      "PPB003" = list(params = list(
        name = "thing3",
        type = "numeric"
      ))
    )
  )
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables[["PPB002"]]$dependency.results[["1"]] <- c("C")
  expect_identical(
    check.variable.dependencies(in.phenotype.data, in.variable.summary),
    out.variable.summary
  )
})

test_that("consistency of BMI and weight/height is correctly flagged", {
  in.phenotype.data <- data.frame(
    "subject" = c("A", "B", "C", "D", "E"),
    "PPB001" = c(40, 45, NA, 20, 20),
    "PPB002" = c(1.25, 1.15, 1.1, NA, 2),
    "PPB003" = c(25.6, 34.6, 32, 30, NA)
  )
  in.variable.summary <- list(
    variables = list(
      "subject" = list(params = list(
        name = "subject",
        type = "string",
        subject_id = TRUE
      )),
      "PPB001" = list(params = list(
        name = "weight",
        type = "numeric"
      )),
      "PPB002" = list(params = list(
        name = "height",
        type = "numeric"
      )),
      "PPB003" = list(params = list(
        name = "BMI",
        type = "numeric",
        dependencies = list(
          "1" = list(
            name = "test1",
            condition = "response.is.computed.bmi(PPB003, PPB001, PPB002, 0.05)"
          )
        )
      ))
    )
  )
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables[["PPB003"]]$dependency.results[["1"]] <- c("B")
  expect_identical(
    check.variable.dependencies(in.phenotype.data, in.variable.summary),
    out.variable.summary
  )
})

test_that("variable greater than comparison works", {
  x1 <- c(1:11, NA, 6)
  x2 <- c(11:1, 5, NA)
  expect_identical(
    response.greater.than(x1, x2),
    c(
      rep(FALSE, 5),
      rep(TRUE, 8)
    )
  )
})

test_that("variable greater than or equal to comparison works", {
  x1 <- c(1:11, NA, 6)
  x2 <- c(11:1, 5, NA)
  expect_identical(
    response.greater.than(x1, x2, FALSE),
    c(
      rep(FALSE, 6),
      rep(TRUE, 7)
    )
  )
})
