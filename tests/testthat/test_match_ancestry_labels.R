test_that("load.ancestry.linker successfully maps groups to harmonized labels", {
  input.filename <- "files/match_ancestry_labels/dummy_ancestries.tsv"
  output.vec <- c("anc1", "anc1", "anc1", "anc2", "anc3", "anc3")
  names(output.vec) <- c("anc1", "alt1", "alt2", "anc2", "anc3", "alt3")
  expect_identical(
    load.ancestry.linker(input.filename),
    output.vec
  )
})

test_that("weak.ancestry.match appropriately discerns partial matches", {
  ## note that just for test case reasons, some of these are perfect matches;
  ## those should actually be handled separately upstream. but these make
  ## straightforward examples that my brain can understand
  in.self.reports <- c("anc1", "anc2", "anc3", "anc", "panda")
  in.anc.data <- c("ancestry1", "ancestry1", "ancestry2", "ancestry2", "ancestry2", "ancestry3", "ancestry3")
  names(in.anc.data) <- c("anc1", "ancestry1", "anc2", "ance2", "ancestry2", "anc3", "ancestry3")
  out.phenotype <- c("anc1", "anc2", "anc3", NA, NA)
  out.reasoning <- c(
    "success",
    "low discernment but top matches concordant",
    "success",
    "low discernment between discordant calls",
    "top match quality too low"
  )
  out.top.match <- c("anc1", "anc2", "anc3", "anc1", "anc1")
  out.top.value <- c(1, 1, 1, 3 / 4, 2 / 5)
  out.second.match <- c("anc2", "ance2", "anc1", "anc2", "anc2")
  out.second.value <- c(3 / 4, 4 / 5, 3 / 4, 3 / 4, 2 / 5)
  expect_equal(
    weak.ancestry.match(in.self.reports, in.anc.data, 0.75, 0.78),
    list(
      phenotype = out.phenotype,
      reasoning = out.reasoning,
      top.match = out.top.match,
      top.value = out.top.value,
      second.match = out.second.match,
      second.value = out.second.value
    )
  )
})

test_that("harmonize.ancestry.from.linker addresses perfect, imperfect, and failed matches", {
  in.self.reports <- c("anc1", "anc2", "anc3", "anc", NA, "anc12", "bussian", "belorussian")
  in.variable.summary <- list(params = list(subject_ancestry = TRUE))
  in.anc.data <- c(
    "ancestry1", "ancestry1",
    "ancestry2", "ancestry2", "ancestry2",
    "ancestry3", "ancestry3",
    "ancestry4", "ancestry4", "ancestry4",
    "ancestry5", "ancestry5", "ancestry5"
  )
  names(in.anc.data) <- c(
    "anc1", "ancestry1",
    "anc2", "ance2", "ancestry2",
    "anc3", "ancestry3",
    "russian", "russkiy", "ancestry4",
    "belarusian", "belarus", "ancestry5"
  )
  out.phenotype <- c("ancestry1", "ancestry2", "ancestry3", NA, NA, NA, "ancestry4", "ancestry5")
  out.phenotype <- factor(out.phenotype, levels = sort(unique(unname(in.anc.data))))
  out.variable.summary <- in.variable.summary
  out.variable.summary$perfect.ancestry.matches <- as.integer(3)
  out.variable.summary$imperfect.ancestry.matches <- as.integer(2)
  out.variable.summary$failed.ancestry.matches <- as.integer(2)
  out.variable.summary$ancestry.conversion.before <- c("anc", "anc12", "bussian", "belorussian")
  out.variable.summary$ancestry.conversion.after <- c(NA, NA, "russian", "belarusian")
  out.variable.summary$ancestry.reasoning <- c(
    "top match quality too low",
    "low discernment between discordant calls",
    "success",
    "success"
  )
  out.variable.summary$ancestry.best.match <- c("anc1", "anc1", "russian", "belarusian")
  out.variable.summary$ancestry.best.value <- c(3 / 4, 4 / 5, 0.8571429, 0.8181818)
  out.variable.summary$ancestry.second.match <- c("anc2", "anc2", "belarusian", "russian")
  out.variable.summary$ancestry.second.value <- c(3 / 4, 4 / 5, 1 / 2, 0.6363636)
  out.variable.summary$invalid.factor.entries <- c("anc", "anc12")
  expect_equal(
    harmonize.ancestry.from.linker(
      in.self.reports,
      in.variable.summary,
      in.anc.data,
      0.78,
      0.8
    ),
    list(
      phenotype = out.phenotype,
      variable = out.variable.summary
    ),
    tolerance = 1e-5
  )
})


test_that("harmonize.ancestry deploys ancestry harmonization for phenotype dataset", {
  in.anc.filename <- "files/match_ancestry_labels/dummy_ancestries_2.tsv"
  in.phenotype.data <- data.frame(
    TV001 = c("A", "B", "C", "D", "E", "F", "G", "H"),
    TV002 = c("anc1", "anc2", "anc3", "anc", NA, "anc12", "bussian", "belorussian"),
    TV003 = rnorm(8)
  )
  in.variable.summary <- list(variables = list(
    TV001 = list(params = list(subject_id = TRUE)),
    TV002 = list(params = list(subject_ancestry = TRUE)),
    TV003 = list()
  ))
  out.phenotype.data <- in.phenotype.data
  out.phenotype.data[, 2] <- c("ancestry1", "ancestry2", "ancestry3", NA, NA, NA, "ancestry4", "ancestry5")
  out.phenotype.data[, 2] <- factor(out.phenotype.data[, 2], levels = paste("ancestry", 1:5, sep = ""))
  out.variable.summary <- in.variable.summary
  out.variable.summary$variables$TV002$perfect.ancestry.matches <- as.integer(3)
  out.variable.summary$variables$TV002$imperfect.ancestry.matches <- as.integer(2)
  out.variable.summary$variables$TV002$failed.ancestry.matches <- as.integer(2)
  out.variable.summary$variables$TV002$ancestry.conversion.before <- c("anc", "anc12", "bussian", "belorussian")
  out.variable.summary$variables$TV002$ancestry.conversion.after <- c(NA, NA, "russian", "belarusian")
  out.variable.summary$variables$TV002$ancestry.reasoning <- c(
    "top match quality too low",
    "low discernment between discordant calls",
    "success",
    "success"
  )
  out.variable.summary$variables$TV002$ancestry.best.match <- c("anc1", "anc1", "russian", "belarusian")
  out.variable.summary$variables$TV002$ancestry.best.value <- c(3 / 4, 4 / 5, 0.8571429, 0.8181818)
  out.variable.summary$variables$TV002$ancestry.second.match <- c("anc2", "anc2", "belarusian", "russian")
  out.variable.summary$variables$TV002$ancestry.second.value <- c(3 / 4, 4 / 5, 1 / 2, 0.6363636)
  out.variable.summary$variables$TV002$invalid.factor.entries <- c("anc", "anc12")
  expect_equal(
    harmonize.ancestry(
      in.phenotype.data,
      in.variable.summary,
      in.anc.filename,
      0.78,
      0.8
    ),
    list(
      phenotype.data = out.phenotype.data,
      variable.summary = out.variable.summary
    ),
    tolerance = 1e-5
  )
})
