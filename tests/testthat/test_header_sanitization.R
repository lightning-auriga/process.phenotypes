tag.name <- "DF"
phenotype.data <- data.frame(
  c(1, 2, 3),
  c(4, 5, 6)
)
colnames(phenotype.data) <- c(
  "weird phenotype name 1",
  "weird ** ////!! name __2"
)
map.header.expected.output <- list(
  DF00001 = list(original.name = "weird phenotype name 1"),
  DF00002 = list(original.name = "weird ** ////!! name __2")
)

test_that("variable names are mapped correctly in order", {
  expect_identical(
    map.header(phenotype.data, tag.name),
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
  expected.output <- list(
    DF00001 = list(original.name = "a"),
    DF00002 = list(original.name = "b"),
    DF00003 = list(original.name = "c"),
    DF00004 = list(original.name = "b")
  )
  expect_identical(
    map.header(test.data, tag.name),
    expected.output
  )
})

test_that("data frame column names are updated in place", {
  expected.df <- phenotype.data
  colnames(expected.df) <- c("DF00001", "DF00002")
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
    DF00001 = list(original.name = "a"),
    DF00002 = list(original.name = "b"),
    DF00003 = list(original.name = "c"),
    DF00004 = list(original.name = "b")
  )
  expected.df <- test.data
  colnames(expected.df) <- c("DF00001", "DF00002", "DF00003", "DF00004")
  expect_identical(sanitize.header(test.data, mapped.variables), expected.df)
})
