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

test_that("whitespace surrounding dashes is removed", {
  in.df <- data.frame(
    A = c(" - ", " -", "- ", "-"),
    B = c(" -- ", " --", "-- ", "--"),
    C = c("A - B", "A --B", "A-- B", "A--B"),
    D = c(
      " - -", "- -",
      " - - - --- -- -- -- ----",
      " -- -- -- - - A--- ---   - - - "
    )
  )
  out.df <- data.frame(
    A = c("-", "-", "-", "-"),
    B = c("-", "-", "-", "-"),
    C = c("A-B", "A-B", "A-B", "A-B"),
    D = c("-", "-", "-", "-A-")
  )
  expect_identical(
    remove.whitespace(in.df),
    out.df
  )
})

test_that("collapse consecutive characters into a single replacement", {
  in.df <- data.frame(
    A = c("\\/", "0..112"),
    B = c("//////", "0..\\//\\..01")
  )
  out.df <- data.frame(
    A = c("/", "0.112"),
    B = c("/", "0./.01")
  )
  expect_identical(
    collapse.repeats(in.df, c("\\\\/", "\\."), c("/", ".")),
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

test_that("remove.nonword.chars preserves decimal values with leading '.'", {
  in.df <- data.frame(
    A = c(".1", "other"),
    B = c(".AAB", ";thing:")
  )
  out.df <- data.frame(
    A = c("0.1", "other"),
    B = c("AAB", "thing")
  )
  expect_identical(
    remove.nonword.chars(in.df),
    out.df
  )
})

test_that("remove.nonword.chars preserves trailing ')' ']' '}'", {
  in.df <- data.frame(
    A = c("[thing1", "thing2]"),
    B = c("thing3(", "thing4}"),
    C = c("(thing5", "thing6])"),
    D = c("thing9]", "[]thing8")
  )
  out.df <- data.frame(
    A = c("thing1", "thing2]"),
    B = c("thing3", "thing4}"),
    C = c("thing5", "thing6])"),
    D = c("thing9]", "thing8")
  )
  expect_identical(remove.nonword.chars(in.df), out.df)
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
