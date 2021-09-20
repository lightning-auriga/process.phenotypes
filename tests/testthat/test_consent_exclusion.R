test_that("consent exclusion works with both files specified", {
  in.phenotype.data <- data.frame(
    TV001 = 1:6,
    TV002 = c("E", "B", "C", "A", "D", "F"),
    TV003 = c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
  )
  in.variable.summary <- list(
    globals = list(
      consent_inclusion_file = "files/consent_exclusion/include.txt",
      consent_exclusion_file = "files/consent_exclusion/exclude.txt"
    ),
    variables = list(
      TV001 = list(),
      TV002 = list(params = list(subject_id = TRUE)),
      TV003 = list()
    )
  )
  out.phenotype.data <- in.phenotype.data[4:5, ]
  out.variable.summary <- in.variable.summary
  out.variable.summary$subjects.consent.no <- as.integer(c(1))
  out.variable.summary$subjects.consent.yes <- as.integer(c(2))
  out.variable.summary$subjects.ambiguous.consent <- c("E", "C", "F")
  expect_identical(
    apply.consent.exclusion(in.phenotype.data, in.variable.summary),
    list(
      phenotype.data = out.phenotype.data,
      variable.summary = out.variable.summary
    )
  )
})

test_that("consent exclusion works with only exclusion file specified", {
  in.phenotype.data <- data.frame(
    TV001 = 1:6,
    TV002 = c("E", "B", "C", "A", "D", "F"),
    TV003 = c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
  )
  in.variable.summary <- list(
    globals = list(
      consent_exclusion_file = "files/consent_exclusion/exclude.txt"
    ),
    variables = list(
      TV001 = list(),
      TV002 = list(params = list(subject_id = TRUE)),
      TV003 = list()
    )
  )
  out.phenotype.data <- in.phenotype.data[-2, ]
  out.variable.summary <- in.variable.summary
  out.variable.summary$subjects.consent.no <- as.integer(c(1))
  out.variable.summary$subjects.consent.yes <- as.integer(c(0))
  out.variable.summary$subjects.ambiguous.consent <- character()
  expect_identical(
    apply.consent.exclusion(in.phenotype.data, in.variable.summary),
    list(
      phenotype.data = out.phenotype.data,
      variable.summary = out.variable.summary
    )
  )
})

test_that("consent exclusion works with only inclusion file specified", {
  in.phenotype.data <- data.frame(
    TV001 = 1:6,
    TV002 = c("E", "B", "C", "A", "D", "F"),
    TV003 = c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
  )
  in.variable.summary <- list(
    globals = list(
      consent_inclusion_file = "files/consent_exclusion/include.txt"
    ),
    variables = list(
      TV001 = list(),
      TV002 = list(params = list(subject_id = TRUE)),
      TV003 = list()
    )
  )
  out.phenotype.data <- in.phenotype.data[4:5, ]
  out.variable.summary <- in.variable.summary
  out.variable.summary$subjects.consent.no <- as.integer(c(0))
  out.variable.summary$subjects.consent.yes <- as.integer(c(2))
  out.variable.summary$subjects.ambiguous.consent <- character()
  expect_identical(
    apply.consent.exclusion(in.phenotype.data, in.variable.summary),
    list(
      phenotype.data = out.phenotype.data,
      variable.summary = out.variable.summary
    )
  )
})

test_that("consent exclusion does nothing with no files specified", {
  in.phenotype.data <- data.frame(
    TV001 = 1:6,
    TV002 = c("E", "B", "C", "A", "D", "F"),
    TV003 = c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
  )
  in.variable.summary <- list(
    globals = list(),
    variables = list(
      TV001 = list(),
      TV002 = list(params = list(subject_id = TRUE)),
      TV003 = list()
    )
  )
  out.phenotype.data <- in.phenotype.data
  out.variable.summary <- in.variable.summary
  out.variable.summary$subjects.consent.no <- as.integer(c(0))
  out.variable.summary$subjects.consent.yes <- as.integer(c(0))
  out.variable.summary$subjects.ambiguous.consent <- character()
  expect_identical(
    apply.consent.exclusion(in.phenotype.data, in.variable.summary),
    list(
      phenotype.data = out.phenotype.data,
      variable.summary = out.variable.summary
    )
  )
})

test_that("consent exclusion gracefully handles empty files", {
  in.phenotype.data <- data.frame(
    TV001 = 1:6,
    TV002 = c("E", "B", "C", "A", "D", "F"),
    TV003 = c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
  )
  in.variable.summary <- list(
    globals = list(
      consent_inclusion_file = "files/consent_exclusion/empty.txt",
      consent_exclusion_file = "files/consent_exclusion/empty.txt"
    ),
    variables = list(
      TV001 = list(),
      TV002 = list(params = list(subject_id = TRUE)),
      TV003 = list()
    )
  )
  out.phenotype.data <- in.phenotype.data[rep(FALSE, 6), ]
  out.variable.summary <- in.variable.summary
  out.variable.summary$subjects.consent.no <- as.integer(c(0))
  out.variable.summary$subjects.consent.yes <- as.integer(c(0))
  out.variable.summary$subjects.ambiguous.consent <- c("E", "B", "C", "A", "D", "F")
  expect_identical(
    apply.consent.exclusion(in.phenotype.data, in.variable.summary),
    list(
      phenotype.data = out.phenotype.data,
      variable.summary = out.variable.summary
    )
  )
})
