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
