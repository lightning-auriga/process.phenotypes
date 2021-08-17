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
  in.anc.data <- c("ancestry1", "ancestry1", "ancestry2", "ancestry2", "ancestry3")
  names(in.anc.data) <- c("anc1", "ancestry1", "anc2", "ance2", "anc3")
  out.vec <- c("anc1", "anc2", "anc3", NA, NA)
  expect_identical(
    weak.ancestry.match(in.self.reports, in.anc.data, 0.9, 0.8),
    out.vec
  )
})

test_that("harmonize.ancestry.from.linker addresses perfect, imperfect, and failed matches", {
  in.self.reports <- c("anc1", "anc2", "anc3", "anc", NA, "anc12", "bussian", "belorussian")
  in.variable.summary <- list(params = list(subject_ancestry = TRUE))
  in.anc.data <- c(
    "ancestry1", "ancestry1",
    "ancestry2", "ancestry2",
    "ancestry3",
    "ancestry4", "ancestry4",
    "ancestry5", "ancestry5"
  )
  names(in.anc.data) <- c(
    "anc1", "ancestry1",
    "anc2", "ance2",
    "anc3",
    "russian", "russkiy",
    "belarusian", "belarus"
  )
  out.phenotype <- c("ancestry1", "ancestry2", "ancestry3", NA, NA, NA, "ancestry4", "ancestry5")
  out.phenotype <- factor(out.phenotype, levels = sort(unique(unname(in.anc.data))))
  out.variable.summary <- in.variable.summary
  out.variable.summary$perfect.ancestry.matches <- as.integer(3)
  out.variable.summary$imperfect.ancestry.matches <- as.integer(2)
  out.variable.summary$failed.ancestry.matches <- as.integer(2)
  out.variable.summary$ancestry.conversion.before <- c("anc", "anc12", "bussian", "belorussian")
  out.variable.summary$ancestry.conversion.after <- c(NA, NA, "russian", "belarusian")
  expect_identical(
    harmonize.ancestry.from.linker(
      in.self.reports,
      in.variable.summary,
      in.anc.data,
      0.8,
      0.8
    ),
    list(
      phenotype = out.phenotype,
      variable = out.variable.summary
    )
  )
})
