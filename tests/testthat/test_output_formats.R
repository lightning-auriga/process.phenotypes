phenotype.data <- data.frame(
  HW00001 = 1:4,
  HW00002 = 5:8
)
variable.summary <- list(
  tag = "HW",
  globals = list(
    consent_inclusion_file = NULL,
    consent_exclusion_file = NULL
  ),
  variables = list(
    HW00001 = list(params = list(
      name = "var1",
      type = "numeric",
      suppress_reporting = TRUE,
      canonical_name = "var1"
    )),
    HW00002 = list(params = list(
      name = "var2",
      shared_model = "model1",
      canonical_name = "var2"
    ))
  )
)

test_that("write.output.formats emits reasonable yaml", {
  out.fileprefix <- tempfile("wof_yaml_output")
  expect_false(file.exists(paste(out.fileprefix, ".yaml", sep = "")))
  write.output.formats(
    phenotype.data,
    variable.summary,
    out.fileprefix,
    FALSE, FALSE, FALSE, FALSE, TRUE
  )
  expect_true(file.exists(paste(out.fileprefix, ".yaml", sep = "")))
  ## configuration writing is handled by separate tested function, just need
  ## to confirm that the output ends up where it's expected to
})

test_that("write.output.formats emits reasonable tsv", {
  out.fileprefix <- tempfile("wof_tsv_output")
  expect_false(file.exists(paste(out.fileprefix, ".tsv", sep = "")))
  write.output.formats(
    phenotype.data,
    variable.summary,
    out.fileprefix,
    TRUE, FALSE, FALSE, FALSE, FALSE
  )
  expect_true(file.exists(paste(out.fileprefix, ".tsv", sep = "")))
  output <- read.table(paste(out.fileprefix, ".tsv", sep = ""),
    sep = "\t", header = TRUE, comment.char = "",
    quote = "\"", stringsAsFactors = FALSE
  )
  expect_equal(output, phenotype.data)
})

test_that("write.output.formats emits expected stata files", {
  out.fileprefix <- tempfile("wof_dta_output")
  expect_false(file.exists(paste(out.fileprefix, ".dta", sep = "")))
  write.output.formats(
    phenotype.data,
    variable.summary,
    out.fileprefix,
    FALSE, TRUE, FALSE, FALSE, FALSE
  )
  expect_true(file.exists(paste(out.fileprefix, ".dta", sep = "")))
  ## we know nothing about the expected format, and so that is not tested
})

test_that("write.output.formats emits expected spss files", {
  out.fileprefix <- tempfile("wof_zsav_output")
  expect_false(file.exists(paste(out.fileprefix, ".zsav", sep = "")))
  write.output.formats(
    phenotype.data,
    variable.summary,
    out.fileprefix,
    FALSE, FALSE, TRUE, FALSE, FALSE
  )
  expect_true(file.exists(paste(out.fileprefix, ".zsav", sep = "")))
  ## we know nothing about the expected format, and so that is not tested
})

test_that("write.output.formats emits expected sas files", {
  out.fileprefix <- tempfile("wof_sas7bdat")
  expect_false(file.exists(paste(out.fileprefix, ".sas7bdat", sep = "")))
  expect_false(file.exists(paste(out.fileprefix, ".sas", sep = "")))
  write.output.formats(
    phenotype.data,
    variable.summary,
    out.fileprefix,
    FALSE, FALSE, FALSE, TRUE, FALSE
  )
  expect_true(file.exists(paste(out.fileprefix, ".sas7bdat", sep = "")))
  expect_true(file.exists(paste(out.fileprefix, ".sas", sep = "")))
  ## we know nothing about the expected format, and so that is not tested
})
