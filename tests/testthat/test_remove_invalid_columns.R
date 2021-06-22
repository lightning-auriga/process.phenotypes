test_that("columns with NA headers are removed", {
  df <- data.frame(1:3, 4:6, 7:9)
  colnames(df) <- c("a", NA, "b")
  out.df <- df[, c(1, 3)]
  expect_identical(
    remove.invalid.columns(df),
    out.df
  )
})

test_that("columns with empty headers are removed", {
  df <- data.frame(1:3, 4:6, 7:9)
  colnames(df) <- c("a", "c", "")
  out.df <- df[, 1:2]
  expect_identical(
    remove.invalid.columns(df),
    out.df
  )
})
