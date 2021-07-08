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
  var.summary <- list(variables = list(
    A = list(params = list(type = "categorical")),
    B = list(params = list(type = "categorical"))
  ))
  expect_identical(
    remove.nonword.chars(df, var.summary),
    out.df
  )
})

test_that("remove.nonword.chars preserves decimal values with leading '.'", {
  in.df <- data.frame(
    A = c(".1", "other"),
    B = c(".AAB", ";thing:")
  )
  var.summary <- list(variables = list(
    A = list(params = list(type = "categorical")),
    B = list(params = list(type = "categorical"))
  ))
  out.df <- data.frame(
    A = c("0.1", "other"),
    B = c("AAB", "thing")
  )
  expect_identical(
    remove.nonword.chars(in.df, var.summary),
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
  var.summary <- list(variables = list(
    A = list(params = list(type = "categorical")),
    B = list(params = list(type = "categorical")),
    C = list(params = list(type = "categorical")),
    D = list(params = list(type = "categorical"))
  ))
  out.df <- data.frame(
    A = c("thing1", "thing2]"),
    B = c("thing3", "thing4}"),
    C = c("thing5", "thing6])"),
    D = c("thing9]", "thing8")
  )
  expect_identical(remove.nonword.chars(in.df, var.summary), out.df)
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
    C = as.character(c("none", NA, NA, NA, NA)),
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
  in.vec <- c(
    "1", "2.3", "a", NA,
    "1cm", "-4.0mmhg", "1", "1",
    "not", "a", "numeric", "100/80",
    "100/80", "120/90", "130/85", "110 / 80"
  )
  out.vec <- c(
    1, 2.3, NA, NA,
    1, -4.0, 1, 1,
    NA, NA, NA, NA,
    NA, NA, NA, NA
  )
  in.summary <- list(
    original.name = "testname",
    summary = c(q1 = 0.2, q2 = 0.5, mean = 0.3)
  )
  out.summary <- list(
    original.name = "testname",
    summary = c(q1 = 0.2, q2 = 0.5, mean = 0.3),
    invalid.numeric.entries = c(
      "a", "not", "a", "numeric", "100/80",
      "100/80", "120/90", "130/85", "110 / 80"
    )
  )
  expect_identical(
    reformat.numerics(in.vec, in.summary),
    list(phenotype.data = out.vec, variable.summary = out.summary)
  )
})

test_that("reformat.blood.pressure identifies and reformats SBP/DBP measures", {
  in.vec <- c(
    "1", "2.3", "a", NA,
    "1cm", "-4.0mmhg", "1", "1",
    "not", "a", "numeric", "100/80",
    "100/80", "120/90", "130/85", "110 / 80"
  )
  out.vec <- c(
    NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA, "100/80",
    "100/80", "120/90", "130/85", "110/80"
  )
  in.summary <- list(
    original.name = "testname",
    summary = c(q1 = 0.2, q2 = 0.5, mean = 0.3)
  )
  out.summary <- list(
    original.name = "testname",
    summary = c(q1 = 0.2, q2 = 0.5, mean = 0.3),
    invalid.blood.pressure.entries = c(
      "1", "2.3", "a", "1cm",
      "-4.0mmhg", "1", "1",
      "not", "a", "numeric"
    )
  )
  expect_identical(
    reformat.blood.pressure(in.vec, in.summary),
    list(phenotype.data = out.vec, variable.summary = out.summary)
  )
})

test_that("reformat.factor converts character vectors to factors with a level order", {
  in.vec <- c("pineapple", "orange", "lemon", "lemon", "cucumber")
  in.summary <- list(
    original.name = "testname",
    type = "categorical",
    params = list(levels = list(
      "0" = list(name = "orange"),
      "1" = list(name = "lemon"),
      "2" = list(name = "pineapple"),
      "3" = list(name = "cucumber")
    ))
  )
  out.vec <- factor(in.vec, levels = c("orange", "lemon", "pineapple", "cucumber"))
  out.summary <- in.summary
  out.summary$invalid.factor.entries <- character()
  out.list <- list(
    phenotype.data = out.vec,
    variable.summary = out.summary
  )
  expect_identical(
    reformat.factor(in.vec, in.summary),
    out.list
  )
})

test_that("reformat.factor reassigns alternate levels to the correct primary before conversion", {
  in.vec <- c("pineapple", "orange", "lemon", "lemon", "Pineapple", "cucumber", "pointy yellow thing")
  in.summary <- list(
    original.name = "testname",
    type = "categorical",
    params = list(levels = list(
      "0" = list(name = "orange"),
      "1" = list(name = "lemon"),
      "2" = list(
        name = "pineapple",
        alternate_patterns = c(
          "Pineapple",
          "pointy yellow thing"
        )
      ),
      "3" = list(name = "cucumber")
    ))
  )
  out.vec <- factor(c(
    "pineapple",
    "orange",
    "lemon",
    "lemon",
    "pineapple",
    "cucumber",
    "pineapple"
  ), levels = c("orange", "lemon", "pineapple", "cucumber"))
  out.summary <- in.summary
  out.summary$invalid.factor.entries <- character()
  out.list <- list(
    phenotype.data = out.vec,
    variable.summary = out.summary
  )
  expect_identical(
    reformat.factor(in.vec, in.summary),
    out.list
  )
})

test_that("Recognized Unicode characters are replaced", {
  in.phenotype.data <- data.frame(TN001 = c(
    "some text (\U00B1 1)",
    "thing\U2192otherthing",
    "\U1F645",
    "98.6\U00B0",
    "\U2018thing\U2019",
    "\U201Cthing\U201D"
  ))
  in.variable.summary <- list(variables = list(TN001 = list(
    original.name = "Header",
    params = list(
      name = "Header",
      type = "string"
    )
  )))
  out.phenotype.data <- data.frame(TN001 = c(
    "some text (+/- 1)",
    "thing->otherthing",
    "#error!",
    "98.6degrees",
    "'thing'",
    "\"thing\""
  ))
  expect_identical(
    process.unicode.characters(in.phenotype.data),
    out.phenotype.data
  )
})

test_that("Excel error codes are detected and reported", {
  in.phenotype.data <- data.frame(TN001 = c("ok thing!", "#ERROR!", "#VALUE!", "#error!", "puppies#error!", "=#value!"))
  in.variable.summary <- list(variables = list(TN001 = list(
    original.name = "Header",
    params = list(
      name = "Header",
      type = "string"
    )
  )))
  out.phenotype.data <- data.frame(TN001 = c("ok thing!", NA, NA, NA, "puppies#error!", NA))
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables$TN001$excel.problem.count <- as.integer(4)
  expect_identical(
    exclude.excel.failures(in.phenotype.data, in.variable.summary),
    list(phenotype.data = out.phenotype.data, variable.summary = out.variable.summary)
  )
})

test_that("Unhandled Unicode characters are detected and reported", {
  in.phenotype.data <- data.frame(TN001 = c("ok thing!", "puppies!", "\U1F436", "\U1F436", "\U1F436"))
  in.variable.summary <- list(variables = list(TN001 = list(
    original.name = "Header",
    params = list(
      name = "Header",
      type = "string"
    )
  )))
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables$TN001$unicode.entries <- table(rep("\U1F436", 3))
  expect_identical(
    detect.unicode.characters(in.phenotype.data, in.variable.summary),
    out.variable.summary
  )
})
