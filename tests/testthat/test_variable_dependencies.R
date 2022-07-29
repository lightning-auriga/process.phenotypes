test_that("find.subject.id.index locates variable flagged as subject_id", {
  in.var.summary <- list(variables = list(
    TN001 = list(name = "placeholder"),
    TN002 = list(
      name = "puppies",
      params = list(subject_id = TRUE)
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
      params = list(subject_id = TRUE)
    ),
    TN002 = list(name = "first number"),
    TN003 = list(
      name = "second number",
      params = list(dependencies = list("1" = list(
        name = "first dependency",
        condition = "TN003 == TN002 + 3"
      )))
    )
  ))
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables$TN003$dependency.results[["1"]] <- as.character(c())
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
      params = list(subject_id = TRUE)
    ),
    TN002 = list(name = "first number"),
    TN003 = list(
      name = "second number",
      params = list(dependencies = list("1" = list(
        name = "first dependency",
        condition = "TN003 == TN002 + 3 & TN003 > 4"
      )))
    )
  ))
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables$TN003$dependency.results[["1"]] <- as.character(c("A"))
  expect_identical(
    check.variable.dependencies(in.phenotype.data, in.variable.summary),
    out.variable.summary
  )
})

test_that("check.variable.dependencies evaluates simple dependencies (length 1 result)", {
  in.phenotype.data <- data.frame(
    TN001 = c("A", "B", "C"),
    TN002 = 1:3,
    TN003 = 4:6
  )
  in.variable.summary <- list(variables = list(
    TN001 = list(
      name = "subject ID",
      params = list(subject_id = TRUE)
    ),
    TN002 = list(name = "first number"),
    TN003 = list(
      name = "second number",
      params = list(dependencies = list("1" = list(
        name = "first dependency",
        condition = "all(TN003 == TN002 + 3)"
      )))
    )
  ))
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables$TN003$dependency.results[["1"]] <- TRUE
  expect_identical(
    check.variable.dependencies(in.phenotype.data, in.variable.summary),
    out.variable.summary
  )
})

test_that("check.variable.dependencies evaluates simple dependencies (invalid return vector length)", {
  in.phenotype.data <- data.frame(
    TN001 = c("A", "B", "C"),
    TN002 = 1:3,
    TN003 = 4:6
  )
  in.variable.summary <- list(variables = list(
    TN001 = list(
      name = "subject ID",
      params = list(subject_id = TRUE)
    ),
    TN002 = list(name = "first number"),
    TN003 = list(
      name = "second number",
      params = list(dependencies = list("1" = list(
        name = "first dependency",
        condition = "c(TRUE, FALSE)"
      )))
    )
  ))
  expect_error(
    check.variable.dependencies(in.phenotype.data, in.variable.summary)
  )
})

test_that("dependency.failure.handling successfully responds to exclusion requests", {
  in.phenotype.data <- data.frame(
    TV001 = c("A", "B", "C", "D", "E", "F"),
    TV002 = c("yes", "yes", "yes", "no", NA, "no"),
    TV003 = c(1.001, 2.002, 3.003, 4.04, 5.05, NA),
    TV004 = c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE),
    TV005 = c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
  )
  in.variable.summary <- list(variables = list(
    TV001 = list(params = list(subject_id = TRUE)),
    TV002 = list(
      params = list(dependencies = list("1" = list(
        name = "dep1",
        condition = "thing"
      ))),
      dependency.results = list("1" = c("B", "F"))
    ),
    TV003 = list(
      params = list(dependencies = list(
        "1" = list(
          name = "dep2",
          condition = "thing2",
          exclude_on_failure = c("TV003")
        ),
        "2" = list(
          name = "dep3",
          condition = "thing3",
          exclude_on_failure = c("TV002", "TV003")
        )
      )),
      dependency.results = list(
        "1" = c("A"),
        "2" = c("A", "C")
      )
    ),
    TV004 = list(
      params = list(dependencies = list(
        "1" = list(
          name = "dep4",
          condition = "thing4",
          exclude_on_failure = c("TV004")
        ),
        "2" = list(
          name = "dep5",
          condition = "thing5",
          exclude_on_failure = c("TV004")
        ),
        "3" = list(
          name = "dep6",
          condition = "thing6",
          exclude_on_failure = c("TV002", "TV003", "TV004")
        )
      )),
      dependency.results = list(
        "1" = c("B"),
        "2" = c("B", "D"),
        "3" = c()
      )
    ),
    TV005 = list(
      params = list(dependencies = list(
        "1" = list(
          name = "dep4",
          condition = "thing4",
          exclude_all_on_failure = TRUE
        )
      )),
      dependency.results = list(
        "1" = c("B")
      )
    )
  ))
  out.phenotype.data <- data.frame(
    TV001 = c("A", "B", "C", "D", "E", "F"),
    TV002 = c(NA, NA, NA, "no", NA, "no"),
    TV003 = c(NA, NA, NA, 4.04, 5.05, NA),
    TV004 = c(TRUE, NA, TRUE, NA, TRUE, FALSE),
    TV005 = c(TRUE, NA, TRUE, TRUE, TRUE, FALSE)
  )
  out.variable.summary <- in.variable.summary
  out.variable.summary$actual.nas.from.deps <- as.integer(9)
  out.variable.summary$possible.nas.from.deps <- as.integer(24)

  expect_identical(
    dependency.failure.handling(in.phenotype.data, in.variable.summary),
    list(
      phenotype.data = out.phenotype.data,
      variable.summary = out.variable.summary
    )
  )
})
