test_that("aggregate.subjects.wrong.type works at all", {
  in.variable.summary <- list(variables = list(
    TV001 = list(subjects.wrong.type = c("SC001", "SC004")),
    TV002 = list(subjects.wrong.type = c("SC001", "SC003")),
    TV003 = list(),
    TV004 = list(subjects.wrong.type = c())
  ))
  output <- as.integer(c(2, 1, 1))
  names(output) <- c("SC001", "SC003", "SC004")
  expect_identical(
    aggregate.subjects.wrong.type(in.variable.summary),
    output
  )
})

test_that("aggregate.variables.wrong.type works at all", {
  in.variable.summary <- list(variables = list(
    TV001 = list(subjects.wrong.type = c("SC001", "SC004", "SC005")),
    TV002 = list(subjects.wrong.type = c("SC001", "SC003")),
    TV003 = list(),
    TV004 = list(subjects.wrong.type = c())
  ))
  output <- as.integer(c(3, 2, 0, 0))
  names(output) <- c("TV001", "TV002", "TV003", "TV004")
  expect_identical(
    aggregate.variables.wrong.type(in.variable.summary),
    output
  )
})

test_that("compute.subject.na.count works at all", {
  in.phenotype.data <- data.frame(
    TV001 = c("A", "B", "C", "D", "E"),
    TV002 = c(1, 2, 3, NA, 5),
    TV003 = c(NA, NA, NA, NA, 20),
    TV004 = c(NA, "A", NA, "B", "A")
  )
  in.variable.summary <- list(variables = list(
    TV001 = list(
      original.name = "whatever",
      params = list(
        name = "whatever",
        type = "string",
        subject_id = TRUE
      )
    ),
    TV002 = list(
      original.name = "otherthing",
      params = list(
        name = "otherthing",
        type = "numeric"
      )
    ),
    TV003 = list(
      original.name = "pandabear",
      params = list(
        name = "pandabear",
        type = "numeric"
      )
    ),
    TV004 = list(
      original.name = "alpaca",
      params = list(
        name = "alpaca",
        type = "categorical",
        levels = list(
          "1" = list(name = "A"),
          "2" = list(name = "B")
        )
      )
    )
  ))
  output <- as.integer(c(2, 1, 2, 2, 0))
  names(output) <- in.phenotype.data[, "TV001"]
  expect_identical(
    compute.subject.na.count(
      in.phenotype.data,
      in.variable.summary
    ),
    output
  )
})
