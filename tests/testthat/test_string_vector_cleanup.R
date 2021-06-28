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

test_that("normalize.missing.values normalizes all missing values", {
  in.df <- data.frame(
    A = c(NA, "na", "not apppplplicabe", " ", "notmissing"),
    B = c("nil", "not sure", "nan", "not assessed", "unavailable"),
    C = c("none", "unknown", "not available", "not done", ""),
    D = c("not asessed", "not assesed", "unavailabe", "unavalable", "unvailable"),
    E = c("ukown", "unkown", "not avalabe", "not avialable", "")
  )
  out.df <- data.frame(
    A = c(NA, NA, NA, " ", "notmissing"),
    B = as.character(c(NA, NA, NA, NA, NA)),
    C = as.character(c(NA, NA, NA, NA, NA)),
    D = c(NA, NA, NA, NA, "unvailable"),
    E = as.character(c(NA, NA, NA, NA, NA))
  )
  expect_identical(normalize.missing.values(in.df), out.df)
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
    "CV0001" = c("1", "2.3", "a", NA),
    "CV0002" = c("1cm", "-4.0mmhg", "1", "1"),
    "CV0003" = c("not", "a", "numeric", "100/80"),
    "CV0004" = c("100/80", "120/90", "130/85", "110/80")
  )

  s1 <- c("quantile1" = 0.2, "quantile2" = 0.5, "mean" = 0.3)
  s2 <- c("quantile1" = 0.1, "quantile2" = 0.5, "mean" = 0.6)
  s3 <- c("not" = 1, "a" = 1, "numeric" = 1, "100/80" = 1)
  s4 <- c("100/80" = 1, "120/90" = 1, "130/85" = 1, "110/80" = 1)
  n1 <- TRUE
  n2 <- TRUE
  n3 <- FALSE
  n4 <- FALSE
  i1 <- c("a")
  i2 <- character()

  in1 <- list("summary" = s1)
  in2 <- list("summary" = s2)
  in3 <- list("summary" = s3)
  in4 <- list("summary" = s4)
  in.var.summary <- list("CV0001" = in1, "CV0002" = in2, "CV0003" = in3, "CV0004" = in4)

  out1 <- list("summary" = s1, "numeric.detected" = n1, "invalid.numeric.entries" = i1)
  out2 <- list("summary" = s2, "numeric.detected" = n2, "invalid.numeric.entries" = i2)
  out3 <- list("summary" = s3, "numeric.detected" = n3)
  out4 <- list("summary" = s4, "numeric.detected" = n4)
  out.var.summary <- list("CV0001" = out1, "CV0002" = out2, "CV0003" = out3, "CV0004" = out4)

  out.df <- data.frame(
    "CV0001" = c(1, 2.3, NA, NA),
    "CV0002" = c(1, -4.0, 1, 1),
    "CV0003" = c("not", "a", "numeric", "100/80"),
    "CV0004" = c("100/80", "120/90", "130/85", "110/80")
  )

  expect_identical(
    reformat.numerics(in.df, in.var.summary, accept.proportion = 0.5),
    list(phenotype.data = out.df, variable.summary = out.var.summary)
  )
})

test_that("reformat.blood.pressure identifies and reformats SBP/DBP measures", {
  in.df <- data.frame(
    "CV0001" = c("1", "2.3", "a", NA),
    "CV0002" = c("1cm", "-4.0mmhg", "1", "1"),
    "CV0003" = c("not", "a", "numeric", "100/80"),
    "CV0004" = c("100/80mmhg", "120/90", "130/85", "110/80")
  )

  s1 <- c("quantile1" = 0.2, "quantile2" = 0.5, "mean" = 0.3)
  s2 <- c("quantile1" = 0.1, "quantile2" = 0.5, "mean" = 0.6)
  s3 <- c("not" = 1, "a" = 1, "numeric" = 1, "100/80" = 1)
  s4 <- c("100/80" = 1, "120/90" = 1, "130/85" = 1, "110/80" = 1)
  n1 <- FALSE
  n2 <- FALSE
  n3 <- FALSE
  n4 <- TRUE
  i4 <- character()

  in1 <- list("summary" = s1)
  in2 <- list("summary" = s2)
  in3 <- list("summary" = s3)
  in4 <- list("summary" = s4)
  in.var.summary <- list("CV0001" = in1, "CV0002" = in2, "CV0003" = in3, "CV0004" = in4)

  out1 <- list("summary" = s1, "blood.pressure.detected" = n1)
  out2 <- list("summary" = s2, "blood.pressure.detected" = n2)
  out3 <- list("summary" = s3, "blood.pressure.detected" = n3)
  out4 <- list("summary" = s4, "blood.pressure.detected" = n4, "invalid.blood.pressure.entries" = i4)
  out.var.summary <- list("CV0001" = out1, "CV0002" = out2, "CV0003" = out3, "CV0004" = out4)

  out.df <- data.frame(
    "CV0001" = c("1", "2.3", "a", NA),
    "CV0002" = c("1cm", "-4.0mmhg", "1", "1"),
    "CV0003" = c("not", "a", "numeric", "100/80"),
    "CV0004" = c("100/80", "120/90", "130/85", "110/80")
  )
  expect_identical(
    reformat.blood.pressure(in.df, in.var.summary, accept.proportion = 0.5),
    list(phenotype.data = out.df, variable.summary = out.var.summary)
  )
})
