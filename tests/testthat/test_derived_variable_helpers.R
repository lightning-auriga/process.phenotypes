test_that("conversion from family members to yes/no first degree relative works", {
  in.phenotype.data <- data.frame("PPB001" = c(
    "mother:grandmother:mother in law:aunt: children",
    "uncle:aunt:father in law:grandfather"
  ))
  in.variable.summary <- list(
    variables = list("PPB001" = list(
      name = "fam",
      type = "string"
    )),
    derived = list("PPB001_derived" = list(
      name = "first_degree",
      type = "string",
      code = "process.phenotypes::derive.first.degree(PPB001, PPB001_derived)"
    ))
  )
  out.phenotype.data <- in.phenotype.data
  out.phenotype.data[["PPB001_derived"]] <- c("yes", "no")
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables[["PPB001_derived"]] <- list(
    original.name = "first_degree",
    params = out.variable.summary$derived[["PPB001_derived"]]
  )
  expect_identical(
    create.derived.variables(in.phenotype.data, in.variable.summary),
    list(
      phenotype.data = out.phenotype.data,
      variable.summary = out.variable.summary
    )
  )
})


test_that("rank normal transform works at all", {
  in.phenotype.data <- data.frame(TV0001 = c(1, 2, 3, 4, 5))
  in.variable.summary <- list(
    variables = list(TV0001 = list(params = list(
      name = "test variable",
      type = "numeric"
    ))),
    derived = list(TV0001_derived = list(
      name = "normal transform",
      code = "derive.rank.normal.transform(TV0001, offset = 0.5)"
    ))
  )
  out.phenotype.data <- in.phenotype.data
  out.phenotype.data[, "TV0001_derived"] <- c(-1.2815516, -0.5244005, 0, 0.5244005, 1.2815516)
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables[["TV0001_derived"]] <- list(
    original.name = in.variable.summary$derived[["TV0001_derived"]]$name,
    params = in.variable.summary$derived[["TV0001_derived"]]
  )
  expect_equal(create.derived.variables(
    in.phenotype.data,
    in.variable.summary
  ),
  list(
    phenotype.data = out.phenotype.data,
    variable.summary = out.variable.summary
  ),
  tolerance = 1e-5
  )
})

test_that("rank normal transform returns NA vector with NA input", {
  expect_identical(
    derive.rank.normal.transform(as.numeric(rep(NA, 100))),
    rep(NA, 100)
  )
})

test_that("rank normal transform resolves ties randomly", {
  x <- c(1, 2, 2, 3, 4, 5)
  y.expected <- c(
    -1.3829941, -0.2104284, -0.6744898,
    0.2104284, 0.6744898, 1.3829941
  )
  n.random.mismatch <- 0
  n.consistent.hits <- 0
  n.total <- 100
  for (i in seq_len(n.total)) {
    y <- derive.rank.normal.transform(x)
    if (isTRUE(all.equal(abs(y[c(-2, -3)] - y.expected[c(-2, -3)]) < 1e-5, rep(TRUE, 4)))) {
      n.consistent.hits <- n.consistent.hits + 1
    }
    if (abs(y[2] - y.expected[2]) > 1e-5 |
      abs(y[3] - y.expected[3]) > 1e-5) {
      n.random.mismatch <- n.random.mismatch + 1
    }
  }
  expect_equal(n.consistent.hits, n.total)
  expect_true(binom.test(n.random.mismatch, n.total, 0.5, alt = "two")$p.value >= 0.01)
})

test_that("rank normal transform stratifies by one variable", {
  in.phenotype.data <- data.frame(
    TV0001 = c(1, 2, 3, 4, 5),
    TVSTRAT = factor(c(1, 1, 1, 2, 2))
  )
  in.variable.summary <- list(
    variables = list(
      TV0001 = list(params = list(
        name = "test variable",
        type = "numeric"
      )),
      TVSTRAT = list(params = list(
        name = "stratification variable",
        type = "ordinal",
        levels = list(
          "1" = list(name = "1"),
          "2" = list(name = "2")
        )
      ))
    ),
    derived = list(TV0001_derived = list(
      name = "normal transform",
      code = "derive.rank.normal.transform(TV0001, strat = list(TVSTRAT), offset = 0.5)"
    ))
  )
  out.phenotype.data <- in.phenotype.data
  out.phenotype.data[, "TV0001_derived"] <- c(-1.2815516, 0.0000000, 1.2815516, -0.5244005, 0.5244005)
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables[["TV0001_derived"]] <- list(
    original.name = in.variable.summary$derived[["TV0001_derived"]]$name,
    params = in.variable.summary$derived[["TV0001_derived"]]
  )
  expect_equal(create.derived.variables(
    in.phenotype.data,
    in.variable.summary
  ),
  list(
    phenotype.data = out.phenotype.data,
    variable.summary = out.variable.summary
  ),
  tolerance = 1e-5
  )
})

test_that("rank normal transform stratifies by two variables", {
  tv0001 <- c(1, 2, 3, 4, 5, 6)
  tvstrat1 <- factor(c(1, 1, 1, 2, 2, 2))
  tvstrat2 <- factor(c(1, 1, 2, 2, 3, 3))
  set1 <- c(-1.3829941, -0.6744898)
  set2 <- c(-0.2104284, 0.2104284)
  set3 <- -1 * set1[2:1]
  targets <- list(set1, set3, set2, set2, set1, set3)
  n.random.mismatch <- 0
  n.total <- 100
  hit.first.entry.counts <- rep(0, 6)
  hit.any.entry.counts <- rep(0, 6)
  for (i in seq_len(n.total)) {
    y <- derive.rank.normal.transform(tv0001,
      strat = list(tvstrat1, tvstrat2)
    )
    for (index in seq_len(6)) {
      if (abs(y[index] - targets[[index]][1]) < 1e-5 |
        abs(y[index] - targets[[index]][2]) < 1e-5) {
        hit.any.entry.counts[index] <- hit.any.entry.counts[index] + 1
      }
      if (abs(y[index] - targets[[index]][1]) < 1e-5) {
        hit.first.entry.counts[index] <- hit.first.entry.counts[index] + 1
      }
    }
  }
  expect_equal(hit.any.entry.counts, rep(n.total, 6))
  for (index in seq_len(6)) {
    expect_true(binom.test(hit.first.entry.counts[index],
      n.total,
      0.5,
      alt = "two"
    )$p.value >= 0.01)
  }
})

test_that("rank normal transform detects when stratification variables are not in a list", {
  expect_error(derive.rank.normal.transform(1:20,
    stratification.vars = 1:20
  ))
  expect_error(derive.rank.normal.transform(1:20,
    stratification.vars = "XYY"
  ))
})

test_that("rank normal transform detects when offset is malformed", {
  expect_error(derive.rank.normal.transform(1:20,
    offset = "abc"
  ))
})

test_that("rank normal transform detects when include.subjects is malformed", {
  expect_error(derive.rank.normal.transform(1:20,
    include.subjects = rep(TRUE, 19)
  ))
})

test_that("rank normal transform detects when primary.call is malformed", {
  expect_error(derive.rank.normal.transform(1:20,
    primary.call = rnorm(10)
  ))
})
