tag.name <- "DF"
phenotype.data <- data.frame(
  c(1, 2, 3),
  c(4, 5, 6)
)
colnames(phenotype.data) <- c(
  "weird phenotype name 1",
  "weird ** ////!! name __2"
)

config.data <- list(
  tag = "testtag",
  variables = list(
    var001 = list(
      name = "weird phenotype name 1",
      type = "string"
    ),
    var002 = list(
      name = "weird ** ////!! name __2",
      type = "string"
    )
  )
)

map.header.expected.output <- list(
  variables = list(
    var001 = list(
      original.name = "weird phenotype name 1",
      params = list(
        name = "weird phenotype name 1",
        type = "string"
      )
    ),
    var002 = list(
      original.name = "weird ** ////!! name __2",
      params = list(
        name = "weird ** ////!! name __2",
        type = "string"
      )
    )
  )
)

test_that("variable names are mapped correctly in order", {
  expect_identical(
    map.header(phenotype.data, tag.name, config.data),
    map.header.expected.output
  )
})

test_that("duplicate header names don't ruin mapping", {
  test.data <- data.frame(
    1:3,
    4:6,
    7:9,
    10:12
  )
  colnames(test.data) <- c("a", "b", "c", "b")
  config.data <- list(
    tag = "testtag",
    variables = list(
      var001 = list(name = "a", type = "string"),
      var002 = list(name = "b", type = "string"),
      var003 = list(name = "c", type = "string"),
      var004 = list(name = "b", type = "string")
    )
  )
  expected.output <- list(
    variables = list(
      var001 = list(original.name = "a", params = list(name = "a", type = "string")),
      var002 = list(original.name = "b", params = list(name = "b", type = "string")),
      var003 = list(original.name = "c", params = list(name = "c", type = "string")),
      var004 = list(original.name = "b", params = list(name = "b", type = "string"))
    )
  )
  expect_identical(
    map.header(test.data, tag.name, config.data),
    expected.output
  )
})

test_that("data frame column names are updated in place", {
  expected.df <- phenotype.data
  colnames(expected.df) <- c("var001", "var002")
  expect_identical(sanitize.header(phenotype.data, map.header.expected.output), expected.df)
})

test_that("duplicate header names are handled correctly in sanitization", {
  test.data <- data.frame(
    1:3,
    4:6,
    7:9,
    10:12
  )
  colnames(test.data) <- c("a", "b", "c", "b")
  mapped.variables <- list(
    variables = list(
      DF00001 = list(original.name = "a", params = list(name = "a", type = "string")),
      DF00002 = list(original.name = "b", params = list(name = "b", type = "string")),
      DF00003 = list(original.name = "c", params = list(name = "c", type = "string")),
      DF00004 = list(original.name = "b", params = list(name = "b", type = "string"))
    )
  )
  expected.df <- test.data
  colnames(expected.df) <- c("DF00001", "DF00002", "DF00003", "DF00004")
  expect_identical(sanitize.header(test.data, mapped.variables), expected.df)
})

test_that("map.header correctly errors when phenotype data and variable config mismatch length", {
  in.phenotype.data <- data.frame(
    var1 = 1:4,
    var2 = 5:8
  )
  in.variable.summary <- list(variables = list(HW00001 = list(
    name = "var1",
    type = "numeric"
  )))
  expect_error(map.header(in.phenotype.data, "HW", in.variable.summary))
})

test_that("map.header reports an informative message and errors when column names mismatch", {
  in.phenotype.data <- data.frame(
    var1 = 1:4,
    var2 = 5:8
  )
  in.variable.summary <- list(variables = list(
    HW00001 = list(
      name = "VAR1",
      type = "numeric"
    ),
    HW00002 = list(
      name = "VAR2",
      type = "numeric"
    )
  ))
  expect_snapshot(map.header(in.phenotype.data, "HW", in.variable.summary), error = TRUE)
})

test_that("map.header respects request to override variable name sanity check", {
  in.phenotype.data <- data.frame(
    var1 = 1:4,
    var2 = 5:8
  )
  in.variable.summary <- list(
    globals = list(consent.inclusion.file = "filename"),
    variables = list(
      HW00001 = list(
        name = "VAR1",
        type = "numeric"
      ),
      HW00002 = list(
        name = "VAR2",
        type = "numeric"
      )
    ),
    derived = list(dervar1 = list(
      name = "dervar1",
      type = "string",
      code = "c(\"A\", \"B\", \"C\", \"D\")"
    ))
  )
  out.variable.summary <- list(
    variables = list(
      HW00001 = list(
        original.name = "var1",
        params = list(
          name = "VAR1",
          type = "numeric"
        )
      ),
      HW00002 = list(
        original.name = "var2",
        params = list(
          name = "VAR2",
          type = "numeric"
        )
      )
    ),
    globals = list(consent.inclusion.file = "filename"),
    derived = list(dervar1 = list(
      name = "dervar1",
      type = "string",
      code = "c(\"A\", \"B\", \"C\", \"D\")"
    ))
  )
  res <- map.header(in.phenotype.data, "HW", in.variable.summary, TRUE)
  expect_identical(res, out.variable.summary)
})
