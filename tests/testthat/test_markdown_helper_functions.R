test_that("get.top.ten returns the top ten named and values from a named vector ", {
  input.vec <- c(
    "a" = 1, "b" = 2, "c" = 20, "d" = 3, "e" = 4, "f" = 5,
    "g" = 6, "h" = 7, "i" = 8, "j" = 9, "k" = 10
  )
  output.df <- data.frame(
    Subjects = c("a", "b", "d", "e", "f", "g", "h", "i", "j", "k"),
    Counts = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  )
  expect_identical(get.top.ten(decreasing = FALSE, input.vec, "Subjects"), output.df)
})

test_that("get.bins handles vectors with greater than 50 unique values", {
  in.vec <- rnorm(1000)
  expected <- 50
  output <- get.bins(in.vec)
  expect_equal(output, expected)
})

test_that("get.bins handles vectors with at most 50 unique values", {
  in.vec <- rep(1:48, 2)
  expected <- 48
  output <- get.bins(in.vec)
  expect_equal(output, expected)
})

test_that("report.name.and.code reports name only if code missing", {
  var.summary <- list(params = list(
    name = "placeholder",
    canonical_name = "myname"
  ))
  expect_output(report.name.and.code(var.summary),
    regexp = "^Official variable identity: \"myname\"\n$"
  )
})

test_that("report.name.and.code reports code only if name missing", {
  var.summary <- list(params = list(
    name = "placeholder",
    code = "mycode"
  ))
  expect_output(report.name.and.code(var.summary),
    regexp = paste("^\n\nThe logic to create this derived variable is ",
      "as follows:\n\n```\nmycode\n```\n$",
      sep = ""
    )
  )
})

test_that("report.name.and.code reports name and code both", {
  var.summary <- list(params = list(
    name = "placeholder",
    canonical_name = "myname",
    code = "mycode"
  ))
  expect_output(report.name.and.code(var.summary),
    regexp = paste("^Official variable identity: \"myname\"\n\n\n\n",
      "The logic to create this derived variable is ",
      "as follows:\n\n```\nmycode\n```\n$",
      sep = ""
    )
  )
})

test_that("report.name.and.code does nothing if name and code both missing", {
  var.summary <- list(params = list(name = "dummyname"))
  expect_output(report.name.and.code(var.summary),
    regexp = NA
  )
})

test_that("report.excel.problems reports single error", {
  variable.entry <- list(excel.problem.count = 1)
  suppress.reporting <- FALSE
  expect_output(report.excel.problems(variable.entry, suppress.reporting),
    regexp = "^\nWARNING: 1 Excel error code detected in this variable.$"
  )
})

test_that("report.excel.problems reports multiple errors", {
  variable.entry <- list(excel.problem.count = 55)
  suppress.reporting <- FALSE
  expect_output(report.excel.problems(variable.entry, suppress.reporting),
    regexp = "^\nWARNING: 55 Excel error codes detected in this variable.$"
  )
})

test_that("report.excel.problems respects no errors", {
  variable.entry <- list(params = list(
    name = "myname",
    canonical_name = "canonname"
  ))
  suppress.reporting <- FALSE
  expect_output(report.excel.problems(variable.entry, suppress.reporting),
    regexp = NA
  )
})

test_that("report.excel.problems respects suppress.reporting", {
  variable.entry <- list(excel.problem.count = 4200)
  suppress.reporting <- TRUE
  expect_output(report.excel.problems(variable.entry, suppress.reporting),
    regexp = NA
  )
})

rns.valid.phenotype.data <- data.frame(
  HW00001 = 1:50,
  HW00002 = rep(c("group1", "group2"), each = 25)
)
rns.invalid.phenotype.data <- data.frame(
  HW00001 = as.numeric(rep(NA, 22)),
  HW00002 = rep("notanumber", 22)
)
rns.variable.entry <- list(
  params = list(
    name = "myname",
    canonical_name = "canonname",
    bounds = list(
      "min" = 10,
      "max" = 40,
      "sd" = 1
    )
  ),
  num.below.min = 9,
  num.above.max = 10,
  num.beyond.sd = 12
)
rns.name <- "HW00001"
rns.pretty.name <- "such a nice variable"
my.theme <- ggplot2::theme_light() + ggplot2::theme(
  plot.title = ggplot2::element_text(hjust = 0.5, size = 16),
  axis.title = ggplot2::element_text(size = 14),
  axis.text = ggplot2::element_text(size = 12)
)
test_that("report.numeric.summary respects suppress.reporting", {
  expect_output(report.numeric.summary(
    rns.valid.phenotype.data[, 1],
    rns.valid.phenotype.data,
    rns.variable.entry,
    rns.name,
    rns.pretty.name,
    my.theme,
    TRUE
  ),
  regexp = NA
  )
})

test_that("report.numeric.summary suppresses plot when all entries are NA", {
  expect_output(output <- report.numeric.summary(
    rns.invalid.phenotype.data[, 1],
    rns.invalid.phenotype.data,
    rns.variable.entry,
    rns.name,
    rns.pretty.name,
    my.theme,
    FALSE
  ),
  regexp = NA
  )
  expect_true(is.null(output$hist.plot))
  expect_true(inherits(output$tab.summary, "knitr_kable"))
  expect_equal(attr(output$tab.summary, "format"), "html")
  expect_true(stringr::str_detect(
    output$tab.summary,
    stringr::regex(paste("<caption>Numeric bounds on HW00001 \\(such a nice variable\\)</caption>",
      ".*> Type <.*> Value <.*> Count <",
      ".*> minimum <.*> 10 <.*> 9 <",
      ".*> maximum <.*> 40 <.*> 10 <",
      ".*> standard deviation <.*> 1 <.*> 12 <",
      sep = ""
    ),
    dotall = TRUE
    )
  ))
})
