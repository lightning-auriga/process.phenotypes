test_that("exclude.by.missing.subject.id functions excludes appropriate targets", {
  in.phenotype.data <- data.frame(
    TN001 = c("A", NA, "B", "C", NA, "D"),
    TN002 = 1:6,
    TN003 = 7:12
  )
  in.var.summary <- list(variables = list(
    TN001 = list(
      original.name = "name1",
      params = list(
        name = "name1",
        type = "string",
        suppress_reporting = TRUE,
        subject_id = TRUE
      )
    ),
    TN002 = list(
      original.name = "name2",
      params = list(
        name = "name2",
        type = "numeric",
        subject_id = FALSE
      )
    ),
    TN003 = list(
      original.name = "name3",
      params = list(
        name = "name3",
        type = "numeric"
      )
    )
  ))
  expected.df <- in.phenotype.data[c(1, 3, 4, 6), ]
  expected.list <- in.var.summary
  expected.list$na.subject.id.count <- 2
  res <- exclude.by.missing.subject.id(in.phenotype.data, in.var.summary)
  expect_equal(res$phenotype.data, expected.df)
  expect_equal(res$variable.summary, expected.list)
})

test_that("apply.type.conversions minimally functions for all types", {
  in.phenotype.data <- data.frame(
    TN001 = c("freeform", "text", "entry", "field"),
    TN002 = c("yes", "no", NA, "no"),
    TN003 = c("1-10", "11-20", "11-20", "41-50"),
    TN004 = c("orange", "orange", "pineapple", "banana"),
    TN005 = c("1.05", "4.44mm", "3.21", "169 / 100"),
    TN006 = c("100/90", "100 / 80mhg", "200/ 100", "40"),
    TN007 = c("A", "B", "C", "D"),
    TN008 = c("2004", "12/8/08", "April 2012", "hello_world"),
    TN009 = c("1", "4", "2", "3")
  )
  out.phenotype.data <- data.frame(
    TN001 = c("freeform", "text", "entry", "field"),
    TN002 = factor(c("yes", "no", NA, "no"), levels = c("yes", "no")),
    TN003 = ordered(c("1-10", "11-20", "11-20", "41-50"),
      levels = c(
        "1-10",
        "11-20",
        "21-30",
        "31-40",
        "41-50",
        "51-60"
      )
    ),
    TN004 = factor(c("orange", "orange", "pineapple", "banana"),
      levels = c("pineapple", "banana", "orange")
    ),
    TN005 = c(1.05, 4.44, 3.21, NA),
    TN006 = c("100/90", "100/80", "200/100", NA),
    TN007 = c("A", "B", "C", "D"),
    TN008 = c(2004, 2008, 2012, NA),
    TN009 = c(1, NA, 2, 3)
  )
  in.var.summary <- list(
    variables = list(
      TN001 = list(
        original.name = "random thoughts",
        params = list(
          name = "random thoughts",
          type = "string"
        )
      ),
      TN002 = list(
        original.name = "has panda bear",
        params = list(
          name = "has panda bear",
          type = "binary",
          levels = list(
            "0" = list(name = "yes"),
            "1" = list(name = "no")
          )
        )
      ),
      TN003 = list(
        original.name = "age exploded",
        params = list(
          name = "age exploded",
          type = "ordinal",
          levels = list(
            "0" = list(name = "1-10"),
            "1" = list(name = "11-20"),
            "2" = list(name = "21-30"),
            "3" = list(name = "31-40"),
            "4" = list(name = "41-50"),
            "5" = list(name = "51-60")
          )
        )
      ),
      TN004 = list(
        original.name = "popsicle type",
        params = list(
          name = "popsicle type",
          type = "categorical",
          levels = list(
            "0" = list(name = "pineapple"),
            "1" = list(name = "banana"),
            "2" = list(name = "orange")
          )
        )
      ),
      TN005 = list(
        original.name = "spider leg height",
        params = list(
          name = "spider leg height",
          type = "numeric"
        )
      ),
      TN006 = list(
        original.name = "sleepwalking blood pressure",
        params = list(
          name = "sleepwalking blood pressure",
          type = "blood pressure"
        )
      ),
      TN007 = list(
        original.name = "something",
        params = list(
          name = "something",
          type = "string",
          subject_id = TRUE
        )
      ),
      TN008 = list(
        original.name = "fundate",
        params = list(
          name = "fundate",
          type = "date"
        )
      ),
      TN009 = list(
        original.name = "weirdcat",
        params = list(
          name = "weirdcat",
          type = "categorical_to_numeric",
          levels = list(
            "1" = list("name" = "1"),
            "2" = list("name" = "2"),
            "3" = list("name" = "3")
          )
        )
      )
    )
  )
  out.var.summary <- in.var.summary
  out.var.summary$variables$TN002$invalid.factor.entries <- character()
  out.var.summary$variables$TN002$subjects.wrong.type <- character()
  out.var.summary$variables$TN003$invalid.factor.entries <- character()
  out.var.summary$variables$TN003$subjects.wrong.type <- character()
  out.var.summary$variables$TN004$invalid.factor.entries <- character()
  out.var.summary$variables$TN004$subjects.wrong.type <- character()
  out.var.summary$variables$TN005$invalid.numeric.entries <- c("169 / 100")
  out.var.summary$variables$TN005$subjects.wrong.type <- c("D")
  out.var.summary$variables$TN006$invalid.blood.pressure.entries <- c("40")
  out.var.summary$variables$TN006$subjects.wrong.type <- c("D")
  out.var.summary$variables$TN008$invalid.date.entries <- c("hello_world")
  out.var.summary$variables$TN008$subjects.wrong.type <- c("D")
  out.var.summary$variables$TN009$params$type <- "numeric"
  out.var.summary$variables$TN009$invalid.factor.entries <- c("4")
  out.var.summary$variables$TN009$invalid.numeric.entries <- character()
  out.var.summary$variables$TN009$subjects.wrong.type <- c("B")
  expect_identical(
    apply.type.conversions(in.phenotype.data, in.var.summary),
    list(
      phenotype.data = out.phenotype.data,
      variable.summary = out.var.summary
    )
  )
})

test_that("convert.type recognizes and complains about unexpected types", {
  in.phenotype.data <- data.frame(
    TN001 = c("A", "B"),
    TN002 = c("val1", "val2")
  )
  in.var.summary <- list(variables = list(
    TN001 = list(
      original.name = "name1",
      params = list(
        name = "name1",
        type = "string",
        subject_id = TRUE
      )
    ),
    TN002 = list(
      original.name = "name2",
      params = list(
        name = "name2",
        type = "alien_type"
      )
    )
  ))
  expect_error(convert.type(
    in.phenotype.data,
    in.var.summary,
    "alien_type"
  ))
})

test_that("apply.type.conversions correctly skips over null type entries", {
  in.phenotype.data <- data.frame(
    TN001 = c("freeform", "text", "entry", "field")
  )
  out.phenotype.data <- data.frame(
    TN001 = c("freeform", "text", "entry", "field")
  )
  in.var.summary <- list(
    variables = list(
      TN001 = list(
        original.name = "random thoughts",
        params = list(
          name = "random thoughts"
        )
      )
    )
  )
  out.var.summary <- in.var.summary
  output <- NULL
  expected <- list(
    phenotype.data = out.phenotype.data,
    variable.summary = out.var.summary
  )
  expect_warning(output <- apply.type.conversions(in.phenotype.data, in.var.summary))
  expect_identical(output, expected)
})


test_that("apply.type.conversions correctly errors when it finds unhandled type specification", {
  in.phenotype.data <- data.frame(
    TN001 = c("freeform", "text", "entry", "field")
  )
  out.phenotype.data <- data.frame(
    TN001 = c("freeform", "text", "entry", "field")
  )
  in.var.summary <- list(
    variables = list(
      TN001 = list(
        original.name = "random thoughts",
        params = list(
          name = "random thoughts",
          type = "invalid_type"
        )
      )
    )
  )
  out.var.summary <- in.var.summary
  expect_error(apply.type.conversions(in.phenotype.data, in.var.summary))
})


test_that("apply.bounds changes values below/above the provided min/max to NA", {
  in.var.summary <- list(
    variables = list(
      TN001 = list(
        original.name = "random thoughts",
        params = list(
          name = "random thoughts",
          type = "numeric",
          bounds = list(
            min = 5,
            max = 10
          )
        )
      )
    )
  )
  in.phenotype.data <- data.frame(TN001 = c(1, 2, 3, 5, 6, 8, 10, 11, 12, NA))
  out.var.summary <- in.var.summary
  out.var.summary$variables$TN001$num.below.min <- as.integer(3)
  out.var.summary$variables$TN001$num.above.max <- as.integer(2)
  out.phenotype.data <- data.frame(TN001 = c(NA, NA, NA, 5, 6, 8, 10, NA, NA, NA))
  expect_identical(apply.bounds(in.phenotype.data, in.var.summary), list(
    phenotype.data = out.phenotype.data,
    variable.summary = out.var.summary
  ))
})

test_that("apply.bounds handles missing min or max correctly", {
  in.var.summary <- list(
    variables = list(
      TN001 = list(
        original.name = "random thoughts",
        params = list(
          name = "random thoughts",
          type = "numeric",
          bounds = list(min = 5)
        )
      ),
      TN002 = list(
        original.name = "random thoughts",
        params = list(
          name = "random thoughts",
          type = "numeric",
          bounds = list(max = 5)
        )
      )
    )
  )
  in.phenotype.data <- data.frame(TN001 = c(1, 2, 3, 5, 6, 8, 10, 11, 12), TN002 = 1:9)
  out.var.summary <- in.var.summary
  out.var.summary$variables$TN001$num.below.min <- as.integer(3)
  out.var.summary$variables$TN002$num.above.max <- as.integer(4)
  out.phenotype.data <- data.frame(TN001 = c(NA, NA, NA, 5, 6, 8, 10, 11, 12), TN002 = c(1:5, rep(NA, 4)))
  expect_identical(apply.bounds(in.phenotype.data, in.var.summary), list(
    phenotype.data = out.phenotype.data,
    variable.summary = out.var.summary
  ))
})

test_that("apply.bounds handles bidirectional standard deviation bound", {
  in.var.summary <- list(
    variables = list(
      TN001 = list(
        original.name = "random thoughts",
        params = list(
          name = "random thoughts",
          type = "numeric",
          bounds = list(sd = 1)
        )
      )
    )
  )
  in.phenotype.data <- data.frame(TN001 = c(1, 2, 3, 5, 6, 8, 10, 11, 12))
  out.var.summary <- in.var.summary
  out.var.summary$variables$TN001$num.beyond.sd <- as.integer(4)
  out.phenotype.data <- data.frame(TN001 = c(NA, NA, 3, 5, 6, 8, 10, NA, NA))
  expect_identical(apply.bounds(in.phenotype.data, in.var.summary), list(
    phenotype.data = out.phenotype.data,
    variable.summary = out.var.summary
  ))
})

test_that("apply.bounds handles bidirectional standard deviation bound after min/max", {
  in.var.summary <- list(
    variables = list(
      TN001 = list(
        original.name = "random thoughts",
        params = list(
          name = "random thoughts",
          type = "numeric",
          bounds = list(min = -20, max = 20, sd = 0.5)
        )
      )
    )
  )
  in.phenotype.data <- data.frame(TN001 = c(-20000, 1, 2, 3, 4, 5, 1000))
  out.var.summary <- in.var.summary
  out.var.summary$variables$TN001$num.below.min <- as.integer(1)
  out.var.summary$variables$TN001$num.above.max <- as.integer(1)
  out.var.summary$variables$TN001$num.beyond.sd <- as.integer(4)
  out.phenotype.data <- data.frame(TN001 = c(NA, NA, NA, 3, NA, NA, NA))
  expect_identical(apply.bounds(in.phenotype.data, in.var.summary), list(
    phenotype.data = out.phenotype.data,
    variable.summary = out.var.summary
  ))
})

test_that("apply.bounds errors if sd factor is negative", {
  in.var.summary <- list(
    variables = list(
      TN001 = list(
        original.name = "random thoughts",
        params = list(
          name = "random thoughts",
          type = "numeric",
          bounds = list(min = -20, max = 20, sd = -0.5)
        )
      )
    )
  )
  in.phenotype.data <- data.frame(TN001 = c(-20000, 1, 2, 3, 4, 5, 1000))
  out.var.summary <- in.var.summary
  out.var.summary$variables$TN001$num.below.min <- as.integer(1)
  out.var.summary$variables$TN001$num.above.max <- as.integer(1)
  out.var.summary$variables$TN001$num.beyond.sd <- as.integer(4)
  out.phenotype.data <- data.frame(TN001 = c(NA, NA, NA, 3, NA, NA, NA))
  expect_error(apply.bounds(in.phenotype.data, in.var.summary))
})

test_that("convert.variable.specific.na sets instances of strings to NA", {
  in.phenotype.data <- data.frame(TN001 = c("apple", "banana", "river", "cranberry"))
  in.variable.summary <- list(variables = list(TN001 = list(
    original.name = "something",
    params = list(
      type = "categorical",
      levels = list(
        "1" = list(name = "apple"),
        "2" = list(name = "banana"),
        "3" = list(name = "cranberry")
      ),
      "na-values" = c("window", "river", "Austria")
    )
  )))
  out.phenotype.data <- data.frame(TN001 = c("apple", "banana", NA, "cranberry"))
  out.variable.summary <- in.variable.summary
  expect_identical(
    convert.variable.specific.na(in.phenotype.data, in.variable.summary),
    out.phenotype.data
  )
})

test_that("convert.variable.specific.na enforces suppress.output override", {
  in.phenotype.data <- data.frame(
    TN001 = c("apple", "banana", "river", "cranberry"),
    TN002 = c("redmeat", "chicken", "fish", "pork")
  )
  in.variable.summary <- list(variables = list(
    TN001 = list(
      original.name = "something",
      params = list(
        type = "categorical",
        levels = list(
          "1" = list(name = "apple"),
          "2" = list(name = "banana"),
          "3" = list(name = "cranberry")
        ),
        "na-values" = c("window", "river", "Austria"),
        suppress_output = FALSE
      )
    ),
    TN002 = list(original.name = "meattypes", params = list(
      name = "meattypes",
      type = "string",
      suppress_output = TRUE
    ))
  ))
  out.phenotype.data <- data.frame(
    TN001 = c("apple", "banana", NA, "cranberry"),
    TN002 = rep(NA, 4)
  )
  out.variable.summary <- in.variable.summary
  expect_identical(
    convert.variable.specific.na(in.phenotype.data, in.variable.summary),
    out.phenotype.data
  )
})

test_that("exclude.by.age correctly removes subjects with ages below the given threshold", {
  in.var.summary <- list(
    variables = list(
      TN001 = list(
        original.name = "age",
        params = list(
          name = "age",
          type = "numeric",
          subject_age = TRUE
        )
      ),
      TN002 = list(
        original.name = "not age",
        params = list(
          name = "not age",
          type = "numeric"
        )
      )
    ),
    globals = list(
      min_age_for_inclusion = 16
    )
  )
  in.phenotype.data <- data.frame(TN001 = c(15:20), TN002 = 12:17)
  out.var.summary <- in.var.summary
  out.var.summary$subjects.excluded.for.age <- as.integer(1)
  out.phenotype.data <- data.frame(TN001 = c(16:20), TN002 = 13:17, row.names = 2:6)
  expect_identical(exclude.by.age(in.phenotype.data, in.var.summary), list(
    phenotype.data = out.phenotype.data,
    variable.summary = out.var.summary
  ))
})

test_that("parse.date extracts year and infers century correctly", {
  in.vec <- c("10/20/19", "1/2/95", "2/3/1993", "1/6/123", "21 March 2002", "5-4-04")
  in.var.summary <- list()
  out.vec <- as.numeric(c(2019, 1995, 1993, NA, 2002, 2004))
  out.var.summary <- list(invalid.date.entries = c("1/6/123"))
  expect_identical(
    parse.date(in.vec, in.var.summary),
    list(phenotype.data = out.vec, variable.summary = out.var.summary)
  )
})

test_that("parse.date distinguishes between YYYY-##-## and ##-##-YY", {
  in.vec <- c("10-20-19", "2014-05-06")
  in.var.summary <- list()
  out.vec <- as.numeric(c(2019, 2014))
  out.var.summary <- list(invalid.date.entries = as.character(c()))
  expect_identical(
    parse.date(in.vec, in.var.summary),
    list(phenotype.data = out.vec, variable.summary = out.var.summary)
  )
})

test_that("parse.date can capture dates of the format monthname,?year", {
  in.vec <- c(
    "january,2021",
    "february,1991",
    "march2020",
    "april1993",
    "may,2022",
    "june,2002",
    "july1988",
    "august,1999",
    "september,2021",
    "october1999",
    "november1941",
    "december,1944",
    "June,1999",
    "Junetember,1999",
    "May,199"
  )
  in.var.summary <- list()
  out.vec <- as.numeric(c(
    2021,
    1991,
    2020,
    1993,
    2022,
    2002,
    1988,
    1999,
    2021,
    1999,
    1941,
    1944,
    NA,
    NA,
    NA
  ))
  out.var.summary <- list(invalid.date.entries = as.character(c(
    "June,1999",
    "Junetember,1999",
    "May,199"
  )))
  expect_identical(
    parse.date(in.vec, in.var.summary),
    list(phenotype.data = out.vec, variable.summary = out.var.summary)
  )
})
