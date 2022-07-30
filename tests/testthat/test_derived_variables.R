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
  out.variable.summary$variables[["PPB001_derived"]] <- list(
    original.name = "derived variable 1",
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

test_that("derived variable code cannot overwrite phenotype data", {
  in.phenotype.data <- data.frame("PPB001" = 1:5)
  in.variable.summary <- list(
    variables = list("PPB001" = list(
      name = "example variable 1",
      type = "numeric"
    )),
    derived = list("PPB001_derived" = list(
      name = "derived variable 2",
      type = "numeric",
      code = "PPB001 <- PPB001 * 40 \n PPB001 * 2"
    ))
  )
  out.phenotype.data <- data.frame(
    "PPB001" = 1:5,
    "PPB001_derived" = (1:5) * 80
  )
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables[["PPB001_derived"]] <- list(
    original.name = "derived variable 2",
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

test_that("derived variables may depend on other derived variables", {
  in.phenotype.data <- data.frame("PPB001" = 1:5)
  in.variable.summary <- list(
    variables = list("PPB001" = list(
      name = "example variable 1",
      type = "numeric"
    )),
    derived = list(
      "PPB001_derived" = list(
        name = "derived variable 3",
        type = "numeric",
        code = "PPB001_derived_2 * 2"
      ),
      "PPB001_derived_2" = list(
        name = "derived variable 4",
        type = "numeric",
        code = "PPB001 * 2.5"
      )
    )
  )
  out.phenotype.data <- data.frame(
    "PPB001" = 1:5,
    "PPB001_derived_2" = (1:5) * 2.5,
    "PPB001_derived" = (1:5) * 5
  )
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables[["PPB001_derived_2"]] <- list(
    original.name = "derived variable 4",
    params = out.variable.summary$derived[["PPB001_derived_2"]]
  )
  out.variable.summary$variables[["PPB001_derived"]] <- list(
    original.name = "derived variable 3",
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

test_that("derived variable blocks with bugs crash gracefully", {
  in.phenotype.data <- data.frame("PPB001" = 1:5)
  in.variable.summary <- list(
    variables = list("PPB001" = list(
      name = "example variable 1",
      type = "numeric"
    )),
    derived = list("PPB001_derived" = list(
      name = "derived variable 5",
      type = "numeric",
      code = "PPB001 * PPB002"
    ))
  )
  expect_error(
    create.derived.variables(in.phenotype.data, in.variable.summary)
  )
})

test_that("create.derived.variables refrains from reevaluating existing columns", {
  in.phenotype.data <- data.frame(
    "PPB001" = 1:5,
    "PPB001_derived" = 1:5
  )
  in.variable.summary <- list(
    variables = list("PPB001" = list(
      name = "example variable 1",
      type = "numeric"
    )),
    derived = list("PPB001_derived" = list(
      name = "derived variable 5",
      type = "numeric",
      code = "6:10"
    ))
  )
  res <- create.derived.variables(in.phenotype.data, in.variable.summary)
  expect_identical(res, list(
    phenotype.data = in.phenotype.data,
    variable.summary = in.variable.summary
  ))
})

test_that("create.derived.variables skips code blocks that do nothing", {
  in.phenotype.data <- data.frame(
    "PPB001" = 1:5
  )
  in.variable.summary <- list(
    variables = list("PPB001" = list(
      name = "example variable 1",
      type = "numeric"
    )),
    derived = list("PPB001_derived" = list(
      name = "derived variable 5",
      type = "numeric",
      code = ""
    ))
  )
  res <- create.derived.variables(in.phenotype.data, in.variable.summary)
  expect_identical(res, list(
    phenotype.data = in.phenotype.data,
    variable.summary = in.variable.summary
  ))
})
