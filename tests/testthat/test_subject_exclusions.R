test_that("subject exclusion by metric responds correctly to valid data", {
  df <- data.frame(
    A = c(1, 2, 3, 4, 5),
    B = rnorm(5, 0, 1)
  )
  metric <- as.numeric(c(1, 10, 5, 6, -1))
  bound <- as.numeric(5)
  out.df <- df[c(1, 3, 5), ]
  expect_identical(
    exclude.subjects.by.metric(df, metric, bound),
    out.df
  )
})

test_that("subject exclusion by metric detects aberrant NAs in metric and bound", {
  df <- data.frame(A = rnorm(5))
  metric <- as.numeric(c(1, 2, 4, 5))
  metric.na <- as.numeric(c(1, 2, 4, NA))
  bound <- as.numeric(4)
  bound.na <- as.numeric(NA)
  expect_error(exclude.subjects.by.metric(df, metric, bound.na))
  expect_error(exclude.subjects.by.metric(df, metric.na, bound))
})
