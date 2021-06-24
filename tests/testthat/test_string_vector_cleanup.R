df <- data.frame(
  c("WeiRd CaPs", "Trailing ", "NA", "Nil"),
  c(";.something+", "Not Applicable", "Too   muchspace", "")
)
colnames(df) <- c("A", "B")

test_that("all characters are converted to lowercase", {
  out.df <- data.frame(
    A = c("weird caps", "trailing ", "na", "nil"),
    B = c(";.something+", "not applicable", "too   muchspace", "")
  )
  expect_identical(
    make.lowercase(df),
    out.df
  )
})

test_that("all leading, trailing, and multiple whitespaces are removed", {
  out.df <- data.frame(
    A = c("WeiRd CaPs", "Trailing", "NA", "Nil"),
    B = c(";.something+", "Not Applicable", "Too muchspace", "")
  )
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
    A = c("WeiRd CaPs", "Trailing", "NA", "Nil"),
    B = c("something", "Not Applicable", "Too   muchspace", "")
  )
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
test_that("make.lowercase normalizes all missing values", {
  out.df <- make.lowercase(data.frame(
    A = c("WeiRd CaPs", "Trailing ", NA, NA),
    B = c(";.something+", NA, "Too   muchspace", NA)
  ))
  expect_identical(
    normalize.missing.values(make.lowercase(df)),
    out.df
  )
})

test_that("is.blood.pressure successfully discerns SBP/DBP-style data", {
  in.vec <- c("100/80", "1.0/20", "100    /    40", "100/", "/40", "100/40mmhg")
  out.vec <- c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)
  expect_identical(
    is.blood.pressure(in.vec, FALSE),
    out.vec
  )
})

test_that("is.blood.pressure can understand suffixes when requested", {
  in.vec <- c("100/80", "1.0/20", "100    /    40", "100/", "/40", "100/40mmhg")
  out.vec <- c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)
  expect_identical(
    is.blood.pressure(in.vec, TRUE),
    out.vec
  )
})

test_that("reformat.numerics casts all numbers and number-like values to numeric", {
  in.df <- data.frame(
    A = c("1", "2.3", "a", "."),
    B = c("1cm", "-4.0mmhg", "1", "1"),
    C = c("not", "a", "numeric", "100/80"),
    D = c("100/80", "120/90", "130/85", "110/80")
  )
  out.df <- data.frame(
    A = c(1, 2.3, NA, NA),
    B = c(1, -4.0, 1, 1),
    C = c("not", "a", "numeric", "100/80"),
    D = c("100/80", "120/90", "130/85", "110/80")
  )
  expect_identical(reformat.numerics(in.df, accept.proportion = 0.5), out.df)
})

test_that("reformat.blood.pressure identifies and reformats SBP/DBP measures", {
  in.df <- data.frame(
    A = c("1", "2.3", "a", "."),
    B = c("1cm", "-4.0mmhg", "1", "1"),
    C = c("not", "a", "numeric", "100/80"),
    D = c("100/80mmhg", "120/ 90", "130/85", "110/80")
  )
  out.df <- data.frame(
    A = c("1", "2.3", "a", "."),
    B = c("1cm", "-4.0mmhg", "1", "1"),
    C = c("not", "a", "numeric", "100/80"),
    D = c("100/80", "120/90", "130/85", "110/80")
  )
  expect_identical(reformat.blood.pressure(in.df, accept.proportion = 0.5), out.df)
})
