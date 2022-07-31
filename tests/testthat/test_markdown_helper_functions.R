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
  HW00001 = c(1:50, NA),
  HW00002 = c(rep(c("group1", "group2"), each = 25), "group3")
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

test_that("report.numeric.summary reports plot and bounds table", {
  ## test automatic sd bounds by nullifying that entry from input
  var.entry <- rns.variable.entry
  var.entry$params$bounds$sd <- NULL
  var.entry$num.beyond.sd <- NULL
  expect_output(output <- report.numeric.summary(
    rns.valid.phenotype.data[, 1],
    rns.valid.phenotype.data,
    var.entry,
    rns.name,
    rns.pretty.name,
    my.theme,
    FALSE
  ),
  regexp = "^\n\n#### Histogram of HW00001 \\(such a nice variable\\) Distribution\n"
  )
  ## check that standard theme has been applied
  expect_true(!is.null(output$hist.plot$theme))
  expect_equal(output$hist.plot$theme$plot.title$hjust, 0.5)
  ## check that it's a geom_histogram
  expect_true(!is.null(output$hist.plot$layers))
  expect_true(inherits(output$hist.plot$layers[[1]]$geom, "GeomBar"))
  ## check that it has three layers: histogram, -sd bound, +sd bound
  expect_equal(length(output$hist.plot$layers), 3)
  expect_true(inherits(output$hist.plot$layers[[2]]$geom, "GeomVline"))
  expect_true(inherits(output$hist.plot$layers[[3]]$geom, "GeomVline"))
  ## check that bounds still report as normal
  expect_true(inherits(output$tab.summary, "knitr_kable"))
  expect_equal(attr(output$tab.summary, "format"), "html")
  expect_true(stringr::str_detect(
    output$tab.summary,
    stringr::regex(paste("<caption>Numeric bounds on HW00001 \\(such a nice variable\\)</caption>",
      ".*> Type <.*> Value <.*> Count <",
      ".*> minimum <.*> 10 <.*> 9 <",
      ".*> maximum <.*> 40 <.*> 10 <",
      sep = ""
    ),
    dotall = TRUE
    )
  ))
  ## be sure that dropping sd bound dropped that entry from the bounds table
  expect_false(stringr::str_detect(
    output$tab.summary,
    stringr::regex(".*> standard deviation <.*> 1 <.*> 12 <",
      dotall = TRUE
    )
  ))
})

test_that("report.numeric.summary reports multimodal distribution", {
  ## override basic test with multimodal test
  var.entry <- rns.variable.entry
  var.entry$params$multimodal <- "HW00002"
  expect_output(output <- report.numeric.summary(
    rns.valid.phenotype.data[, 1],
    rns.valid.phenotype.data,
    var.entry,
    rns.name,
    rns.pretty.name,
    my.theme,
    FALSE
  ),
  regexp = "^\n\n#### Histogram of HW00001 \\(such a nice variable\\) Distribution\n"
  )
  ## check that standard theme has been applied
  expect_true(!is.null(output$hist.plot$theme))
  expect_equal(output$hist.plot$theme$plot.title$hjust, 0.5)
  ## check that it's a geom_histogram
  expect_true(!is.null(output$hist.plot$layers))
  expect_true(inherits(output$hist.plot$layers[[1]]$geom, "GeomBar"))
  ## check that it has even more layers:
  ##   - first distribution histogram
  ##   - first distribution mean
  ##   - first distribution text annotation
  ##   - second distribution histogram
  ##   - second distribution mean
  ##   - second distribution text annotation
  expect_equal(length(output$hist.plot$layers), 6)
  expect_true(inherits(output$hist.plot$layers[[2]]$geom, "GeomVline"))
  expect_true(inherits(output$hist.plot$layers[[3]]$geom, "GeomText"))
  expect_true(inherits(output$hist.plot$layers[[4]]$geom, "GeomBar"))
  expect_true(inherits(output$hist.plot$layers[[5]]$geom, "GeomVline"))
  expect_true(inherits(output$hist.plot$layers[[6]]$geom, "GeomText"))
  ## check that bounds still report as normal
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

linked.year.phenotype.data <- data.frame(
  HW00001 = c("A", "B", "C", "D"),
  HW00002 = 2000:2003,
  HW00003 = 18:21
)
linked.year.variable.summary <- list(variables = list(
  HW00001 = list(params = list(
    name = "var1",
    type = "string",
    subject_id = TRUE
  )),
  HW00002 = list(params = list(
    name = "var2",
    type = "date"
  )),
  HW00003 = list(params = list(
    name = "var3",
    type = "numeric",
    subject_age = TRUE,
    linked_date = list(
      reported_year = "HW00002",
      reference_year = 2020
    )
  ))
))

test_that("report.linked.date respects suppress.reporting", {
  expect_output(output <- report.linked.date(
    linked.year.phenotype.data[, 3],
    linked.year.phenotype.data,
    linked.year.variable.summary$variables$HW00003,
    "HW00003",
    my.theme,
    TRUE
  ),
  regexp = NA
  )
  expect_null(output)
})

test_that("report.linked.date emits a plot of expected type", {
  expect_output(output <- report.linked.date(
    linked.year.phenotype.data[, 3],
    linked.year.phenotype.data,
    linked.year.variable.summary$variables$HW00003,
    "HW00003",
    my.theme,
    FALSE
  ),
  regexp = "^\n\n#### Comparison between reported age HW00003 and age derived from date HW00002\n"
  )
  ## check that standard theme has been applied
  expect_true(!is.null(output$theme))
  expect_equal(output$theme$plot.title$hjust, 0.5)
  ## check that it's a geom_point with a geom_abline on top
  expect_true(!is.null(output$layers))
  expect_equal(length(output$layers), 2)
  expect_true(inherits(output$layers[[1]]$geom, "GeomPoint"))
  expect_true(inherits(output$layers[[2]]$geom, "GeomAbline"))
})

test_that("report.linked.date can access dynamic reference year in data frame", {
  in.phenotype.data <- linked.year.phenotype.data
  in.variable.summary <- linked.year.variable.summary
  in.phenotype.data$HW00004 <- 1998:2001
  in.variable.summary$variables$HW00003$params$linked_date$reference_year <- "HW00004"
  in.variable.summary$variables$HW00004 <- list(params = list(
    name = "var4",
    type = "numeric"
  ))

  expect_output(output <- report.linked.date(
    in.phenotype.data[, 3],
    in.phenotype.data,
    in.variable.summary$variables$HW00003,
    "HW00003",
    my.theme,
    FALSE
  ),
  regexp = "^\n\n#### Comparison between reported age HW00003 and age derived from date HW00002\n"
  )
  ## check that standard theme has been applied
  expect_true(!is.null(output$theme))
  expect_equal(output$theme$plot.title$hjust, 0.5)
  ## check that it's a geom_point with a geom_abline on top
  expect_true(!is.null(output$layers))
  expect_equal(length(output$layers), 2)
  expect_true(inherits(output$layers[[1]]$geom, "GeomPoint"))
  expect_true(inherits(output$layers[[2]]$geom, "GeomAbline"))
})

bmicomp.phenotype.data <- data.frame(
  HW00001 = c("A", "B", "C", "D"),
  HW00002 = seq(25, 40, 5),
  HW00003 = c(1.5, 1.4, 1.3, 1.5),
  HW00004 = seq(65, 80, 5)
)
bmicomp.variable.summary <- list(variables = list(
  HW00001 = list(params = list(
    name = "var1",
    type = "string",
    subject_id = TRUE
  )),
  HW00002 = list(params = list(
    name = "var2",
    type = "numeric",
    computed_bmi = list(
      height = "HW00003",
      weight = "HW00004"
    )
  )),
  HW00003 = list(params = list(
    name = "var3",
    type = "numeric"
  )),
  HW00004 = list(params = list(
    name = "var4",
    type = "numeric"
  ))
))

test_that("report.bmi.comparison respects suppress.reporting", {
  expect_output(output <- report.bmi.comparison(
    bmicomp.phenotype.data,
    bmicomp.variable.summary$variables$HW00002,
    "HW00002",
    my.theme,
    TRUE
  ),
  regexp = NA
  )
  expect_null(output)
})

test_that("report.bmi.comparison emits a plot of expected type", {
  expect_output(output <- report.bmi.comparison(
    bmicomp.phenotype.data,
    bmicomp.variable.summary$variables$HW00002,
    "HW00002",
    my.theme,
    FALSE
  ),
  regexp = paste(
    "^\n\n#### Comparison between reported BMI HW00002 and",
    "BMI derived from height HW00003 and weight HW00004\n"
  )
  )
  ## check that standard theme has been applied
  expect_true(!is.null(output$theme))
  expect_equal(output$theme$plot.title$hjust, 0.5)
  ## check that it's a geom_point with a geom_abline on top
  expect_true(!is.null(output$layers))
  expect_equal(length(output$layers), 2)
  expect_true(inherits(output$layers[[1]]$geom, "GeomPoint"))
  expect_true(inherits(output$layers[[2]]$geom, "GeomAbline"))
})

bp.ratio.phenotype.data <- data.frame(
  HW00001 = c("A", "B", "C", "D"),
  HW00002 = c(180, 170, 160, 150),
  HW00003 = c(90, 80, 75, 65)
)
bp.ratio.variable.summary <- list(variables = list(
  HW00001 = list(params = list(
    name = "var1",
    type = "string",
    subject_id = TRUE
  )),
  HW00002 = list(params = list(
    name = "var2",
    type = "numeric",
    computed_bp_ratio = list(diastolic = "HW00003")
  )),
  HW00003 = list(params = list(
    name = "var3",
    type = "numeric"
  ))
))

test_that("report.bp.ratio respects suppress.reporting", {
  expect_output(output <- report.bp.ratio(
    bp.ratio.phenotype.data,
    bp.ratio.variable.summary$variables$HW00003,
    "HW00003",
    my.theme,
    TRUE
  ),
  regexp = NA
  )
  expect_null(output)
})

test_that("report.bp.ratio emits a plot of expected type", {
  expect_output(output <- report.bp.ratio(
    bp.ratio.phenotype.data,
    bp.ratio.variable.summary$variables$HW00002,
    "HW00002",
    my.theme,
    FALSE
  ),
  regexp = paste(
    "^\n\n#### Comparison between reported systolic HW00002 and",
    "diastolic blood pressure HW00003\n"
  )
  )
  ## check that standard theme has been applied
  expect_true(!is.null(output$theme))
  expect_equal(output$theme$plot.title$hjust, 0.5)
  ## check that it's a geom_point with a geom_abline on top
  expect_true(!is.null(output$layers))
  expect_equal(length(output$layers), 2)
  expect_true(inherits(output$layers[[1]]$geom, "GeomPoint"))
  expect_true(inherits(output$layers[[2]]$geom, "GeomAbline"))
})

rcs.phenotype.data <- data.frame(
  HW00001 = c("A", "B", "C", "D"),
  HW00002 = 1:4,
  HW00003 = factor(rep(c("cat1", "cat2"), each = 2), levels = c("cat1", "cat2"))
)

rcs.uniq.prop <- 0.6

test_that("report.content.summary respects suppress.reporting", {
  var.summary <- table(rcs.phenotype.data[, 1])
  expect_output(output <- report.content.summary(
    rcs.phenotype.data,
    var.summary,
    rcs.uniq.prop,
    "HW00001",
    "first variable",
    TRUE
  ),
  regexp = "^\n\nReporting for variable HW00001 was suppressed in the configuration."
  )
  expect_null(output)
})

test_that("report.content.summary respect variable unique proportion threshold", {
  var.summary <- table(rcs.phenotype.data[, 1])
  expect_output(output <- report.content.summary(
    rcs.phenotype.data,
    var.summary,
    rcs.uniq.prop,
    "HW00001",
    "first variable",
    FALSE
  ),
  regexp = paste(
    "^\n\nVariable HW00001 \\(first variable\\) has 4 unique values,",
    "and thus report output of individual value counts is suppressed."
  )
  )
  expect_null(output)
})

test_that("report.content.summary reports numeric summary", {
  var.summary <- c(
    "Mean" = 2.5,
    "Min" = 1.0,
    "10%" = 1.3,
    "20%" = 1.6,
    "30%" = 1.9,
    "40%" = 2.2,
    "Median" = 2.5,
    "60%" = 2.8,
    "70%" = 3.1,
    "80%" = 3.4,
    "90%" = 3.7,
    "Max" = 4.0
  )
  expect_output(output <- report.content.summary(
    rcs.phenotype.data,
    var.summary,
    rcs.uniq.prop,
    "HW00002",
    "second variable",
    FALSE
  ),
  regexp = NA
  )
  expect_true(stringr::str_detect(
    output,
    stringr::regex(paste("<caption>Summary of HW00002 \\(second variable\\)</caption>",
      ".*> Value <.*> Summary statistics <",
      ".*> Mean <.*> 2.5 <",
      ".*> Min <.*> 1.0 <",
      ".*> 10% <.*> 1.3 <",
      ".*> 20% <.*> 1.6 <",
      ".*> 30% <.*> 1.9 <",
      ".*> 40% <.*> 2.2 <",
      ".*> Median <.*> 2.5 <",
      ".*> 60% <.*> 2.8 <",
      ".*> 70% <.*> 3.1 <",
      ".*> 80% <.*> 3.4 <",
      ".*> 90% <.*> 3.7 <",
      ".*> Max <.*> 4.0 <",
      sep = ""
    ),
    dotall = TRUE
    )
  ))
})

test_that("report.content.summary reports factor summary", {
  var.summary <- table(rcs.phenotype.data[, 3])
  expect_output(output <- report.content.summary(
    rcs.phenotype.data,
    var.summary,
    rcs.uniq.prop,
    "HW00003",
    "third variable",
    FALSE
  ),
  regexp = NA
  )
  expect_true(stringr::str_detect(
    output,
    stringr::regex(paste("<caption>Summary of HW00003 \\(third variable\\)</caption>",
      ".*> Value <.*> Summary statistics <",
      ".*> cat1 <.*> 2 <",
      ".*> cat2 <.*> 2 <",
      sep = ""
    ),
    dotall = TRUE
    )
  ))
})





noncomp.bp.variable.summary <- list(variables = list(
  HW00001 = list(params = list(
    name = "var1",
    type = "string",
    subject_id = TRUE
  )),
  HW00002 = list(
    params = list(
      name = "bp",
      type = "bp"
    ),
    invalid.blood.pressure.entries = "noice"
  )
))

test_that("report.noncompliant.bp respects suppress.reporting", {
  expect_output(output <- report.noncompliant.bp(
    noncomp.bp.variable.summary$variables$HW00002,
    "HW00002",
    TRUE
  ),
  regexp = NA
  )
  expect_null(output)
})

test_that("report.noncompliant.bp emits expected kable", {
  expect_output(output <- report.noncompliant.bp(
    noncomp.bp.variable.summary$variables$HW00002,
    "HW00002",
    FALSE
  ),
  regexp = NA
  )
  expect_true(stringr::str_detect(
    output,
    stringr::regex(paste("<caption>Values that were not consistent with ",
      "blood pressure measurement format.</caption>",
      ".*> HW00002 <.*> Count <",
      ".*> noice <.*> 1 <",
      sep = ""
    ),
    dotall = TRUE
    )
  ))
})

test_that("report.noncompliant.bp understands when all values were well formatted", {
  in.var.summary <- noncomp.bp.variable.summary$variables$HW00002
  in.var.summary$invalid.blood.pressure.entries <- character()
  expect_output(output <- report.noncompliant.bp(
    in.var.summary,
    "HW00002",
    FALSE
  ),
  regexp = "\n\nAll values consistent with blood pressure format or missing data.\n"
  )
  expect_null(output)
})



noncomp.num.variable.summary <- list(variables = list(
  HW00001 = list(params = list(
    name = "var1",
    type = "string",
    subject_id = TRUE
  )),
  HW00002 = list(
    params = list(
      name = "numeric",
      type = "numeric"
    ),
    invalid.numeric.entries = "bummer"
  )
))

test_that("report.noncompliant.numerics respects suppress.reporting", {
  expect_output(output <- report.noncompliant.numerics(
    noncomp.num.variable.summary$variables$HW00002,
    "HW00002",
    TRUE
  ),
  regexp = NA
  )
  expect_null(output)
})

test_that("report.noncompliant.numerics emits expected kable", {
  expect_output(output <- report.noncompliant.numerics(
    noncomp.num.variable.summary$variables$HW00002,
    "HW00002",
    FALSE
  ),
  regexp = NA
  )
  expect_true(stringr::str_detect(
    output,
    stringr::regex(paste("<caption>Values that were not consistent with ",
      "numeric format.</caption>",
      ".*> HW00002 <.*> Count <",
      ".*> bummer <.*> 1 <",
      sep = ""
    ),
    dotall = TRUE
    )
  ))
})

test_that("report.noncompliant.numerics understands when all values were well formatted", {
  in.var.summary <- noncomp.num.variable.summary$variables$HW00002
  in.var.summary$invalid.numeric.entries <- character()
  expect_output(output <- report.noncompliant.numerics(
    in.var.summary,
    "HW00002",
    FALSE
  ),
  regexp = "\n\nAll values consistent with numeric format or missing data.\n"
  )
  expect_null(output)
})





noncomp.dates.variable.summary <- list(variables = list(
  HW00001 = list(params = list(
    name = "var1",
    type = "string",
    subject_id = TRUE
  )),
  HW00002 = list(
    params = list(
      name = "date",
      type = "date"
    ),
    invalid.date.entries = "one-teenth of march-tember"
  )
))

test_that("report.noncompliant.dates respects suppress.reporting", {
  expect_output(output <- report.noncompliant.dates(
    noncomp.dates.variable.summary$variables$HW00002,
    "HW00002",
    TRUE
  ),
  regexp = NA
  )
  expect_null(output)
})

test_that("report.noncompliant.dates emits expected kable", {
  expect_output(output <- report.noncompliant.dates(
    noncomp.dates.variable.summary$variables$HW00002,
    "HW00002",
    FALSE
  ),
  regexp = NA
  )
  expect_true(stringr::str_detect(
    output,
    stringr::regex(paste("<caption>Values that were not consistent with ",
      "date \\(year only\\) format.</caption>",
      ".*> HW00002 <.*> Count <",
      ".*> one-teenth of march-tember <.*> 1 <",
      sep = ""
    ),
    dotall = TRUE
    )
  ))
})

test_that("report.noncompliant.dates understands when all values were well formatted", {
  in.var.summary <- noncomp.dates.variable.summary$variables$HW00002
  in.var.summary$invalid.date.entries <- character()
  expect_output(output <- report.noncompliant.dates(
    in.var.summary,
    "HW00002",
    FALSE
  ),
  regexp = "\n\nAll values consistent with date format \\(year only\\) or missing data.\n"
  )
  expect_null(output)
})

unicode.variable.summary <- list(variables = list(
  HW00001 = list(params = list(
    name = "var1",
    type = "string",
    subject_id = TRUE
  )),
  HW00002 = list(
    params = list(
      name = "string",
      type = "string"
    ),
    unicode.entries = table(rep("h\u0228llo", 2))
  )
))

test_that("report.unicode.entries respects suppress.reporting", {
  expect_output(output <- report.unicode.entries(
    unicode.variable.summary$variables$HW00002,
    TRUE
  ),
  regexp = NA
  )
  expect_null(output)
})

test_that("report.unicode.entries emits expected kable", {
  expect_output(output <- report.unicode.entries(
    unicode.variable.summary$variables$HW00002,
    FALSE
  ),
  regexp = NA
  )
  expect_true(stringr::str_detect(
    output,
    stringr::regex(paste("<caption>String entries containing unresolved Unicode characters.",
      "</caption>",
      ".*> Unicode String <.*> Observations <",
      ".*> h\u0228llo <.*> 2 <",
      sep = ""
    ),
    dotall = TRUE
    )
  ))
})
