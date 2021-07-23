test_that("simple derived variable calculation works", {
  in.phenotype.data <- data.frame("PPB001" = 1:5)
  in.variable.summary <- list(
    variables = list("PPB001" = list(
      name = "example variable 1",
      type = "numeric"
    )),
    derived = list("PPB001_derived" = list(
      name = "derived variable 1",
      type = "numeric",
      code = "PPB001 * 2"
    ))
  )
  out.phenotype.data <- in.phenotype.data
  out.phenotype.data[["PPB001_derived"]] <- out.phenotype.data$PPB001 * 2
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables[["PPB001_derived"]] <- out.variable.summary$derived[["PPB001_derived"]]
  expect_identical(
    create.derived.variables(in.phenotype.data, in.variable.summary),
    list(
      phenotype.data = out.phenotype.data,
      variable.summary = out.variable.summary
    )
  )
})

test_that("derived variable code cannot overwrite phenotype data", {

})

test_that("derived variables may depend on other derived variables", {

})

test_that("derived variable blocks with bugs crash gracefully", {

})
