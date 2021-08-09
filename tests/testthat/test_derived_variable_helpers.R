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
      code = "process.phenotypes::derive.first.degree(PPB001)"
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
