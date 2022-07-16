test_that("subject exclusion by metric responds correctly to valid data", {
  df <- data.frame(
    A = c(1, 2, 3, 4, 5),
    B = rnorm(5, 0, 1),
    C = c("A", "B", "C", "D", "E")
  )
  var.summary <- list(variables = list(
    A = list(),
    B = list(),
    C = list(params = list(subject_id = TRUE))
  ))
  metric <- as.integer(c(10, 5, 6))
  names(metric) <- c("B", "C", "D")
  bound <- as.numeric(5)
  out.df <- df[c(1, 3, 5), ]
  expect_identical(
    exclude.subjects.by.metric(df, var.summary, metric, bound),
    out.df
  )
})

test_that("subject exclusion by metric tolerates null bound by not removing anything", {
  df <- data.frame(
    A = c(1, 2, 3, 4, 5),
    B = rnorm(5, 0, 1),
    C = c("A", "B", "C", "D", "E")
  )
  var.summary <- list(variables = list(
    A = list(),
    B = list(),
    C = list(params = list(subject_id = TRUE))
  ))
  metric <- as.integer(c(10, 5, 6))
  names(metric) <- c("B", "C", "D")
  bound <- NULL
  expect_identical(
    exclude.subjects.by.metric(df, var.summary, metric, bound),
    df
  )
})

test_that("subject exclusion by metric detects aberrant NAs in metric and bound", {
  df <- data.frame(
    A = rnorm(5),
    B = c("A", "B", "C", "D", "E")
  )
  var.summary <- list(variables = list(
    A = list(),
    B = list(params = list(subject_id = TRUE))
  ))
  metric <- as.integer(c(1, 2, 4, 5))
  metric.na <- as.integer(c(1, 2, 4, NA))
  bound <- as.numeric(4)
  bound.na <- as.numeric(NA)
  expect_error(exclude.subjects.by.metric(df, var.summary, metric, bound.na))
  expect_error(exclude.subjects.by.metric(df, var.summary, metric.na, bound))
})
