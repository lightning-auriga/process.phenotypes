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

test_that("data frame column names are updated in place", {
  expected.df <- phenotype.data
  colnames(expected.df) <- c("DF00001", "DF00002")
  expect_identical(sanitize.header(phenotype.data, map.header.expected.output), expected.df)
})
