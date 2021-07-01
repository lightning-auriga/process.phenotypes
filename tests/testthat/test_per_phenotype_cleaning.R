test_that("apply.type.conversions minimally functions for all types", {
  in.phenotype.data <- data.frame(
    TN001 = c("freeform", "text", "entry", "field"),
    TN002 = c("yes", "no", NA, "no"),
    TN003 = c("1-10", "11-20", "11-20", "41-50"),
    TN004 = c("orange", "orange", "pineapple", "banana"),
    TN005 = c("1.05", "4.44mm", "3.21", "169 / 100"),
    TN006 = c("100/90", "100 / 80mhg", "200/ 100", "40")
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
    TN006 = c("100/90", "100/80", "200/100", NA)
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
      )
    )
  )
  out.var.summary <- in.var.summary
  out.var.summary$variables$TN002$invalid.factor.entries <- character()
  out.var.summary$variables$TN003$invalid.factor.entries <- character()
  out.var.summary$variables$TN004$invalid.factor.entries <- character()
  out.var.summary$variables$TN005$invalid.numeric.entries <- c("169 / 100")
  out.var.summary$variables$TN006$invalid.blood.pressure.entries <- c("40")
  expect_identical(
    apply.type.conversions(in.phenotype.data, in.var.summary),
    list(
      phenotype.data = out.phenotype.data,
      variable.summary = out.var.summary
    )
  )
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
