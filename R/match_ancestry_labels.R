#' Use external label dictionary to harmonize free-text
#' ancestry labels
#'
#' @description
#' Ancestry information has been collected as free-text entries
#' with little effort at formal harmonization. Though it may be
#' impossible to perfectly recapture the underlying data due
#' to high string similarity and short length of distinct labels,
#' it's worth a shot.
#'
#' @details
#' TBD
#'
#' @param phenotype.data data frame, input phenotype data
#' @param variable.summary list, variable configuration data
#' @param ancestry.source character vector, name of data
#' source to pull known labels from; e.g. "nigeria"
#' @return list; first entry is modified phenotype dataset
#' with ancestry labels updated, second entry is modified
#' variable summary with ancestry label update metrics added
harmonize.ancestry <- function(phenotype.data, variable.summary, ancestry.source = "nigeria") {
  stopifnot(is.data.frame(phenotype.data))
  stopifnot(is.list(variable.summary))
  stopifnot(
    is.character(ancestry.source),
    length(ancestry.source) == 1,
    ancestry.source == "nigeria"
  )
  stopifnot(length(variable.summary) == ncol(phenotype.data))
  ancestry.filename <- system.file("external",
    paste(ancestry.source, ".ancestry.tsv", sep = ""),
    package = "process.phenotypes"
  )
  ancestry.data <- load.ancestry.linker(ancestry.filename)
  for (i in seq_len(length(variable.summary$variables))) {
    if (!is.null(variable.summary$variables[[i]]$params$subject_ancestry)) {
      res <- harmonize.ancestry.from.linker(
        phenotype.data[, i],
        variable.summary$variables[[i]],
        ancestry.data
      )
      phenotype.data[, i] <- res$phenotype
      variable.summary$variables[[i]] <- res$variable
    }
  }
  list(
    phenotype.data = phenotype.data,
    variable.summary = variable.summary
  )
}

#' Read ancestry linker data and format it for use in R
#'
#' @description
#' Free-text self-reported ancestry is expected to be heterogeneous
#' and complicated. For use in genomics contexts, the labels should
#' be harmonized, at the very least applying single standardized
#' group labels to groups with multiple possible (typically quite
#' similar) names.
#'
#' @details
#' Input ancestry linker format is based off the source of
#' ancestry information from the phase 2 questionnaire, which
#' appears to actually be
#' https://en.wikipedia.org/wiki/List_of_ethnic_groups_in_Nigeria
#' (accessed 17 August 2021). So, in short, the first and only
#' column (tab-delimited) should be formatted as
#' "Name (Alt1, Alt2, ...)",
#' with one row per unique possible group in the output. No
#' header should be present. Note that triple colons are not
#' allowed in any ancestry group name for the time being.
#'
#' @param filename character vector, name of file containing
#' structured linker data
#' @return character vector, named vector of ancestries;
#' names are unique expected inputs in phenotype dataset,
#' values are harmonized labels to be applied in the output
load.ancestry.linker <- function(filename) {
  stopifnot(
    is.character(filename),
    length(filename) == 1,
    file.exists(filename)
  )
  h <- read.table(filename, header = FALSE, stringsAsFactors = FALSE, sep = "\t")
  stopifnot(ncol(h) == 1)
  h <- h[, 1]
  stopifnot(identical(grepl(":::", h), rep(FALSE, length(h))))
  primary.name <- stringr::str_replace(h, "^([^\\(]+) (\\([^\\)]+\\))?$", "\\1")
  alternates <- stringr::str_extract(h, "\\([^\\)]+\\)")
  alternates <- stringr::str_extract(alternates, "[^\\(][^\\)]+")
  alternates <- strsplit(alternates, ", ")
  scoped.names <- unlist(sapply(seq_len(length(alternates)), function(i) {
    paste(primary.name[i],
      c(
        primary.name[i],
        alternates[[i]][!is.na(alternates[[i]])]
      ),
      sep = ":::"
    )
  }), use.names = FALSE)
  res <- stringr::str_replace(scoped.names, "^(.*):::.*$", "\\1")
  ## apply standard string harmonization to harmonized group labels
  res <- remove.whitespace(make.lowercase(data.frame(res)))[, 1]
  unique.names <- stringr::str_replace(scoped.names, "^.*:::(.*)$", "\\1")
  ## apply standard string harmonization to unique group names
  unique.names <- remove.whitespace(make.lowercase(data.frame(unique.names)))[, 1]
  stopifnot(length(unique(unique.names)) == length(unique.names))
  names(res) <- unique.names
  res
}

#' Update ancestry variable with harmonized ancestry names,
#' and report how well you did
#'
#' @description
#' This function attempts to map free-text ancestry data
#' to a set of fixed ancestry labels based on an input linker
#' between known ancestries and harmonized labels. Exact matches
#' are assigned as expected. Then, an attempt is made to harmonize
#' partial matches based on closest match. Actual algorithm TBD.
#'
#' @details
#' Very much TBD
#'
#' @param phenotype character vector, input self-reported ancestry
#' @param variable list, configuration data for input phenotype
#' @param ancestry.data named character vector, linking from unique
#' possible input ancestries to harmonized ancestry label. note that
#' both entries and names in this vector are assumed to have gone
#' through approximately analogous processing as the input free-text
#' ancestry group labels, e.g. made lowercase and had whitespace removed.
#' @param best.match.threshold numeric, on [0,1], denoting how high
#' the best match similarity must be to permit a call
#' @param best.match.discernment numeric, on [0,1] denoting the proportion
#' of the top match the next best match must be less than (assuming the
#' next best match does not map to the same harmonized label) in order
#' to accept the top match
#' @return list, first entry is harmonized phenotype data as a factor
#' with levels set to the available ancestry levels in the known ancestry
#' input, second entry is input configuration list with added metrics
#' about mapping performance
#' @seealso load.ancestry.linker
harmonize.ancestry.from.linker <- function(phenotype,
                                           variable,
                                           ancestry.data,
                                           best.match.threshold = 0.9,
                                           best.match.discernment = 0.75) {
  stopifnot(is.character(phenotype))
  stopifnot(is.list(variable))
  stopifnot(
    is.vector(ancestry.data),
    !is.null(names(ancestry.data))
  )
  stopifnot(
    is.numeric(best.match.threshold),
    length(best.match.threshold) == 1,
    best.match.threshold >= 0,
    best.match.threshold <= 1
  )
  stopifnot(
    is.numeric(best.match.discernment),
    length(best.match.discernment) == 1,
    best.match.discernment >= 0,
    best.match.discernment <= 1
  )
  ## handle ancestries with exact information in the linker
  perfect.matches <- phenotype %in% names(ancestry.data)
  imperfect.matches <- !perfect.matches & !is.na(phenotype)
  phenotype[perfect.matches] <- ancestry.data[phenotype[perfect.matches]]
  ## count these perfect matches for downstream reporting
  variable$perfect.ancestry.matches <- length(which(perfect.matches))
  ## handle ancestries with imperfect matches in the linker
  partial.match.replacements <- weak.ancestry.match(
    phenotype[imperfect.matches],
    ancestry.data,
    best.match.threshold,
    best.match.discernment
  )
  ## count these imperfect matches for downstream reporting
  variable$imperfect.ancestry.matches <- length(which(!is.na(partial.match.replacements)))
  ## count how many conversions failed entirely
  variable$failed.ancestry.matches <- length(phenotype) -
    length(which(is.na(phenotype))) -
    variable$perfect.ancestry.matches -
    variable$imperfect.ancestry.matches
  ## for the time being: report *all* imperfect matches in the
  ## variable summary for reporting purposes
  variable$ancestry.conversion.before <- phenotype[imperfect.matches]
  variable$ancestry.conversion.after <- partial.match.replacements
  ## update the partial matches
  phenotype[imperfect.matches] <- partial.match.replacements
  ## link to standard labels
  phenotype[imperfect.matches & !is.na(phenotype)] <-
    ancestry.data[phenotype[imperfect.matches & !is.na(phenotype)]]
  ## return the results
  list(
    phenotype = factor(phenotype, levels = sort(unique(unname(ancestry.data)))),
    variable = variable
  )
}

#' Attempt to use string similarity to guess what people
#' meant by their self-reported ancestry.
#'
#' @description
#' Self-reported free-text ancestry is messy. If it perfectly
#' matches the ancestry dictionary, that's great; but if it
#' doesn't, and plenty of them don't, we have to try to guess.
#' But guessing is hard, so deliberately fail a lot.
#'
#' @details
#' Will report them when I have them.
#'
#' @param phenotype character vector, input free-text
#' self-reported ancestry; only for subjects with no
#' perfect match to ancestry dictionary!!
#' @param ancestry.data named character vector, linking from unique
#' possible input ancestries to harmonized ancestry label. note that
#' both entries and names in this vector are assumed to have gone
#' through approximately analogous processing as the input free-text
#' ancestry group labels, e.g. made lowercase and had whitespace removed.
#' @param best.match.threshold numeric, on [0,1], denoting how high
#' the best match similarity must be to permit a call
#' @param best.match.discernment numeric, on [0,1] denoting the proportion
#' of the top match the next best match must be less than (assuming the
#' next best match does not map to the same harmonized label) in order
#' to accept the top match
#' @return character vector, modified version of the input
#' ancestry reports, modified to best guess ancestry, or NA if
#' the best match is poor
weak.ancestry.match <- function(phenotype,
                                ancestry.data,
                                best.match.threshold = 0.9,
                                best.match.discernment = 0.75) {
  stopifnot(
    is.numeric(best.match.threshold),
    length(best.match.threshold) == 1,
    best.match.threshold >= 0,
    best.match.threshold <= 1
  )
  stopifnot(
    is.numeric(best.match.discernment),
    length(best.match.discernment) == 1,
    best.match.discernment >= 0,
    best.match.discernment <= 1
  )
  ## attempt #1: stringdist
  ## compute a similarity matrix between all inputs and all
  ## possible ancestries. then pick the top match, if the
  ## match is at least 0.9 (on [0,1]) and the next best match
  ## is at most 80% as good
  ## TODO(lightning.auriga): expose matching parameters if it looks
  ## like this will work at all
  sim.matrix <- stringdist::stringsimmatrix(phenotype, names(ancestry.data))
  ## find index of
  best.match <- apply(sim.matrix, 1, which.max)
  best.value <- sapply(seq_len(nrow(sim.matrix)), function(i) {
    sim.matrix[i, best.match[i]]
  })
  next.best.match <- sapply(seq_len(nrow(sim.matrix)), function(i) {
    vec <- sim.matrix[i, ]
    vec[best.match[i]] <- -1
    which.max(vec)
  })
  next.best.value <- sapply(seq_len(nrow(sim.matrix)), function(i) {
    sim.matrix[i, next.best.match[i]]
  })
  res <- names(ancestry.data[best.match])
  res[best.value < best.match.threshold] <- NA
  res[next.best.value >= best.match.discernment * best.value &
    ancestry.data[best.match] != ancestry.data[next.best.match]] <- NA
  res
}
