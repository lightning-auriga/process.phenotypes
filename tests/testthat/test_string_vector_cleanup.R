df <- data.frame(
  c("WeiRd CaPs", "Trailing ", "NA", "Nil"),
  c(";.something+", "Not Applicable", "Too   muchspace", "")
)
colnames(df) <- c("A", "B")

test_that("all characters are converted to lowercase", {
  out.df <- data.frame(
    c("weird caps", "trailing ", "na", "nil"),
    c(";.something+", "not applicable", "too   muchspace", "")
  )
  colnames(out.df) <- c("A", "B")
  expect_identical(
    make.lowercase(df),
    out.df
  )
})

test_that("all leading, trailing, and multiple whitespaces are removed", {
  out.df <- data.frame(
    c("WeiRd CaPs", "Trailing", "NA", "Nil"),
    c(";.something+", "Not Applicable", "Too muchspace", "")
  )
  colnames(out.df) <- c("A", "B")
  expect_identical(
    remove.whitespace(df),
    out.df
  )
})

# note that this one is partially redundant with remove.whitespace!
test_that("all leading and trailing non-word characters are removed", {
  out.df <- data.frame(
    c("WeiRd CaPs", "Trailing", "NA", "Nil"),
    c("something", "Not Applicable", "Too   muchspace", "")
  )
  colnames(out.df) <- c("A", "B")
  expect_identical(
    remove.nonword.chars(df),
    out.df
  )
})

# this test now depends on make.lowercase - how do we feel about that?
test_that("all missing values are normalized", {
  out.df <- make.lowercase(data.frame(
    c("WeiRd CaPs", "Trailing ", NA, NA),
    c(";.something+", NA, "Too   muchspace", NA)
  ))
  colnames(out.df) <- c("A", "B")
  expect_identical(
    normalize.missing.values(make.lowercase(df)),
    out.df
  )
})
