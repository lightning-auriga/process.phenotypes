in.dataset.tsv <- "files/create_phenotype_report/dataset.tsv"
in.dataset.yaml <- "files/create_phenotype_report/dataset.yaml"
in.shared.models.yaml <- "files/create_phenotype_report/shared_models.yaml"
out.expected.tsv <- "files/create_phenotype_report/expected_output.tsv"

test_that("create.phenotype.report runs end-to-end and emits expected output", {
  out.html <- tempfile("cproutputfile", fileext = ".html")
  expect_output(output <- create.phenotype.report(
    in.dataset.tsv,
    in.dataset.yaml,
    in.shared.models.yaml,
    out.html
  ))
  expect_null(output)
  ## we don't currently have an effective method of inspecting
  ## the contents of the output html; as such, we're going to
  ## rely very heavily on the unit tests of the markdown
  ## helper functions
  expect_true(file.exists(out.html))
  ## actually inspect the output of the tsv versus expected
  out.tsv <- stringr::str_replace(out.html, "\\.html$", ".tsv")
  expect_true(file.exists(out.tsv))
  expected.output <- read.table(out.expected.tsv,
    header = TRUE, stringsAsFactors = FALSE,
    sep = "\t", comment.char = "", quote = "\""
  )
  observed.output <- read.table(out.tsv,
    header = TRUE, stringsAsFactors = FALSE,
    sep = "\t", comment.char = "", quote = "\""
  )
  expect_equal(observed.output, expected.output)
})
