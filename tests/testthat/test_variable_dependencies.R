test_that("find.subject.id.index locates variable flagged as subject_id", {
  in.var.summary <- list(variables = list(
    TN001 = list(name = "placeholder"),
    TN002 = list(
      name = "puppies",
      subject_id = TRUE
    )
  ))
  expect_equal(find.subject.id.index(in.var.summary), 2)
})

test_that("find.subject.id.index throws error when no subject_id variable is present", {
  in.var.summary <- list(variables = list(
    TN001 = list(name = "placeholder"),
    TN002 = list(name = "cats")
  ))
  expect_error(find.subject.id.index(in.var.summary))
})

test_that("check.variable.dependencies evaluates simple dependencies (1)", {
  in.phenotype.data <- data.frame(
    TN001 = c("A", "B", "C"),
    TN002 = 1:3,
    TN003 = 4:6
  )
  in.variable.summary <- list(variables = list(
    TN001 = list(
      name = "subject ID",
      subject_id = TRUE
    ),
    TN002 = list(name = "first number"),
    TN003 = list(
      name = "second number",
      dependencies = list("1" = list(
        name = "first dependency",
        condition = "TN003 == TN002 + 3"
      ))
    )
  ))
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables$TN003$dependencies[["1"]]$result <- as.character(c())
  expect_identical(
    check.variable.dependencies(in.phenotype.data, in.variable.summary),
    out.variable.summary
  )
})

test_that("check.variable.dependencies evaluates simple dependencies (2)", {
  in.phenotype.data <- data.frame(
    TN001 = c("A", "B", "C"),
    TN002 = 1:3,
    TN003 = 4:6
  )
  in.variable.summary <- list(variables = list(
    TN001 = list(
      name = "subject ID",
      subject_id = TRUE
    ),
    TN002 = list(name = "first number"),
    TN003 = list(
      name = "second number",
      dependencies = list("1" = list(
        name = "first dependency",
        condition = "TN003 == TN002 + 3 & TN003 > 4"
      ))
    )
  ))
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables$TN003$dependencies[["1"]]$result <- as.character(c("A"))
  expect_identical(
    check.variable.dependencies(in.phenotype.data, in.variable.summary),
    out.variable.summary
  )
})
