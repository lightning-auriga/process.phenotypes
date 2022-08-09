#' @title
#' Use external label dictionary to harmonize free-text
#' ancestry labels
#'
#' @description
#' Ancestry information is often collected as free-text entries
#' with little effort at formal harmonization. Though it may be
#' impossible to perfectly recapture the underlying data due
#' to high string similarity and short length of distinct labels,
#' it's worth a shot.
#'
#' @details
#' The logic of this method is as follows.
#'
#' Ideally, the respondents to a study questionnaire are reporting
#' repeated responses to what is effectively a categorical ancestry
#' variable for which the categories are masked. In the best case,
#' we would ask subject matter experts to evaluate
#' the input data and construct harmonized mappings that ensure that
#' everyone's responses get converted into standard labels and
#' groups with concordant labels, at whatever level of granularity
#' you're interested in, get grouped together appropriately for
#' your downstream modeling.
#'
#' In reality, very frequently there is insufficient interest
#' in this process for people to dedicate the resources necessary
#' to get subject matter experts to resolve these labels. And
#' so we're left with imperfect data and inadequate options.
#'
#' This function tries to construct mappings between a known group
#' of expected categories and a potentially large pool of imperfect
#' typographic representations of those categories by considering
#' string distance between the labels and the self-reported data.
#' The match settings are configured in an attempt to bias the
#' method false negatives; that is, it would prefer to set
#' subjects to NA ancestry instead of misassigning them to
#' a partial match category that doesn't represent what the respondent
#' intended.
#'
#' In practice, what has worked for this method has been:
#'
#' - Run first with stringent parameters. Require a high quality
#'   primary match and a substantially lower secondary match,
#'   or a secondary match that is an alias of the primary one.
#' - Check the report for every last assignment and guess.
#'   This information is tabulated with count information representing
#'   the number of times this particular self-reported value was
#'   observed in the input data. The idea here is that values
#'   with really high counts are likely indicative of groups that
#'   you've overlooked in your backend label set. However, note
#'   that high count groups might also represent some response
#'   that isn't quite what you asked (e.g. instead of `Polish` you
#'   get `New York`). We've also observed instances of high count
#'   responses that are clearly indicative of a single data uploader
#'   who went a bit wild.
#' - Update the backend table. This step is tedious but critical to
#'   high quality results. Ultimately this function shouldn't be doing
#'   much if any reassignment itself; rather, its reporting information
#'   should guide you to valid aliases for your ancestry labels, which
#'   you can place in your alias mappings, such that in the future
#'   those responses will be considered known matches and mapped correctly.
#' - Repeat the above, until the set of failed matches contains only
#'   response that you in fact don't want to have mapped to categories.
#'
#' This function is completely experimental, and ymmv. We don't specifically
#' recommend its use, and don't expose it directly to configuration, but
#' if you're curious, you can in theory call it from the derived variables
#' block of a dataset and provide a custom backend file. See
#' system.file("external", "nigeria.ancestry.tsv", package = "process.phenotypes")
#' as an example of the two column format it expects; in brief, the file
#' should be two tab-delimited columns, the first column containing the
#' desired group label, and the second column a comma-delimited list
#' of verified aliases for the label.
#'
#' @param phenotype.data Data frame containing input phenotype data.
#' @param variable.summary List of variable configuration data.
#' @param ancestry.source Character vector name of data
#' source to pull known labels from (e.g. "nigeria"); or
#' a file in the format expected by load.ancestry.linker.
#' @param best.match.threshold Numeric in \[0,1\], denoting how high
#' the best match similarity must be to permit a call.
#' @param best.match.discernment Numeric in \[0,1\], denoting the proportion
#' of the top match the next best match must be less than (assuming the
#' next best match does not map to the same harmonized label) in order
#' to accept the top match.
#' @return List; first entry `phenotype.data` is modified phenotype dataset
#' with ancestry labels updated; second entry `variable.summary` is modified
#' input variable summary with ancestry label update metrics added.
#' @seealso load.ancestry.linker, harmonize.ancestry.from.linker, weak.ancestry.match
#' @examples
#' phenotype.data <- data.frame(
#'   HW00001 = c("A", "B", "C", "D"),
#'   HW00002 = c("hausa", "kanuri", "ibibio", "ibibioo")
#' )
#' variable.summary <- list(variables = list(
#'   HW00001 = list(params = list(
#'     name = "subjid",
#'     type = "string",
#'     subject_id = TRUE
#'   )),
#'   HW00002 = list(params = list(
#'     name = "subjanc",
#'     type = "string",
#'     subject_ancestry = TRUE
#'   ))
#' ))
#' result <- process.phenotypes:::harmonize.ancestry(
#'   phenotype.data,
#'   variable.summary
#' )
harmonize.ancestry <- function(phenotype.data,
                               variable.summary,
                               ancestry.source = "nigeria",
                               best.match.threshold = 0.9,
                               best.match.discernment = 0.75) {
  stopifnot(is.data.frame(phenotype.data))
  stopifnot(is.list(variable.summary))
  stopifnot(
    is.character(ancestry.source),
    length(ancestry.source) == 1,
    ancestry.source == "nigeria" | file.exists(ancestry.source)
  )
  stopifnot(length(variable.summary$variables) == ncol(phenotype.data))
  ancestry.filename <- ancestry.source
  if (!file.exists(ancestry.filename)) {
    ancestry.filename <- system.file("external",
      paste(ancestry.source, ".ancestry.tsv", sep = ""),
      package = "process.phenotypes"
    )
  }
  ancestry.data <- load.ancestry.linker(ancestry.filename)
  for (i in seq_len(length(variable.summary$variables))) {
    if (!is.null(variable.summary$variables[[i]]$params$subject_ancestry)) {
      res <- harmonize.ancestry.from.linker(
        phenotype.data[, i],
        variable.summary$variables[[i]],
        ancestry.data,
        best.match.threshold,
        best.match.discernment
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

#' @title
#' Read ancestry linker data and format it for use in heuristic
#' ancestry mapping
#'
#' @description
#' Free-text self-reported ancestry is expected to be heterogeneous
#' and complicated. For use in genomics contexts, the labels should
#' be harmonized, at the very least applying single standardized
#' group labels to groups with multiple possible (typically quite
#' similar) names.
#'
#' @details
#' See the help documentation of harmonize.ancestry for a discussion
#' of the motivation behind this and related functions.
#'
#' Note that, because this function is goofy nonsense,
#' triple colons are not allowed in any ancestry group name
#' for the time being.
#'
#' @param filename Character vector name of file containing
#' structured linker data.
#' @return Named character vector of ancestries;
#' names are unique expected inputs in phenotype dataset,
#' values are harmonized labels to be applied in the output.
#' @seealso harmonize.ancestry, harmonize.ancestry.from.linker,
#' weak.ancestry.match
#' @examples
#' test.file <- system.file("external", "nigeria.ancestry.tsv", package = "process.phenotypes")
#' anc.data <- process.phenotypes:::load.ancestry.linker(test.file)
load.ancestry.linker <- function(filename) {
  stopifnot(
    is.character(filename),
    length(filename) == 1,
    file.exists(filename)
  )
  h <- read.table(filename, header = FALSE, stringsAsFactors = FALSE, sep = "\t", quote = "")
  stopifnot(ncol(h) == 1)
  h <- h[, 1]
  stopifnot(identical(grepl(":::", h), rep(FALSE, length(h))))
  primary.name <- stringr::str_replace(h, "^([^\\[]+) (\\[[^\\]]+\\])?$", "\\1")
  alternates <- stringr::str_extract(h, "\\[[^\\]]+\\]")
  alternates <- stringr::str_extract(alternates, "[^\\[][^\\]]+")
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
  print(unique.names[duplicated(unique.names)])
  stopifnot(length(unique(unique.names)) == length(unique.names))
  names(res) <- unique.names
  res
}

#' @title
#' Update ancestry variable with harmonized ancestry names,
#' and report how well you did
#'
#' @description
#' This function attempts to map free-text ancestry data
#' to a set of fixed ancestry labels based on an input linker
#' between known ancestries and harmonized labels. Exact matches
#' are assigned as expected. Then, an attempt is made to harmonize
#' partial matches based on closest match.
#'
#' @details
#' See the help documentation of harmonize.ancestry for a discussion
#' of the motivation behind this and related functions.
#'
#' @param phenotype Character vector of input self-reported ancestry.
#' @param variable List of configuration data for input phenotype.
#' @param ancestry.data Named character vector, linking from unique
#' possible input ancestries to harmonized ancestry label. Note that
#' both entries and names in this vector are assumed to have gone
#' through approximately analogous processing as the input free-text
#' ancestry group labels, e.g. made lowercase and had whitespace removed.
#' @param best.match.threshold Numeric on \[0,1\], denoting how high
#' the best match similarity must be to permit a call.
#' @param best.match.discernment Numeric on \[0,1\], denoting the proportion
#' of the top match the next best match must be less than (assuming the
#' next best match does not map to the same harmonized label) in order
#' to accept the top match.
#' @return List; first entry `phenotype` is harmonized phenotype data as a factor
#' with levels set to the available ancestry levels in the known ancestry
#' input, second entry `variable` is input configuration list with added metrics
#' about mapping performance.
#' @seealso load.ancestry.linker, harmonize.ancestry, weak.ancestry.match
#' @examples
#' phenotype <- c("hausa", "kanuri", "fulani", "ibibio", "ibibioo")
#' variable <- list()
#' anc.file <- system.file("external", "nigeria.ancestry.tsv", package = "process.phenotypes")
#' anc.data <- process.phenotypes:::load.ancestry.linker(anc.file)
#' result <- process.phenotypes:::harmonize.ancestry.from.linker(
#'   phenotype,
#'   variable,
#'   anc.data
#' )
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
  weak.match.results <- list(
    phenotype = c(),
    reasoning = c(),
    top.match = c(),
    top.value = numeric(),
    second.match = c(),
    second.value = numeric()
  )
  if (length(which(imperfect.matches)) > 0) {
    weak.match.results <- weak.ancestry.match(
      phenotype[imperfect.matches],
      ancestry.data,
      best.match.threshold,
      best.match.discernment
    )
  }
  partial.match.replacements <- weak.match.results$phenotype
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
  ## report even more tracking data: best and second best matches
  ## and quality of those matches; also report a text description
  ## of the reasoning behind the acceptance or rejection of a match
  variable$ancestry.reasoning <- weak.match.results$reasoning
  variable$ancestry.best.match <- weak.match.results$top.match
  variable$ancestry.best.value <- weak.match.results$top.value
  variable$ancestry.second.match <- weak.match.results$second.match
  variable$ancestry.second.value <- weak.match.results$second.value
  ## as this is about to become a factor, update the standard factor
  ## conversion tracking data
  variable$invalid.factor.entries <-
    phenotype[imperfect.matches][is.na(partial.match.replacements)]
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

#' @title
#' Attempt to use string similarity to guess what people
#' meant by their self-reported ancestry
#'
#' @description
#' Self-reported free-text ancestry is messy. If it perfectly
#' matches the ancestry dictionary, that's great; but if it
#' doesn't, and plenty of them don't, we have to try to guess.
#' But guessing is hard, so deliberately fail a lot.
#'
#' @details
#' See the help documentation of harmonize.ancestry for a discussion
#' of the motivation behind this and related functions.
#'
#' @param phenotype Character vector of input free-text
#' self-reported ancestry. This should only contain subjects with no
#' perfect match to the known ancestry label dictionary.
#' @param ancestry.data Named character vector, linking from unique
#' possible input ancestries to harmonized ancestry label. Note that
#' both entries and names in this vector are assumed to have gone
#' through approximately analogous processing as the input free-text
#' ancestry group labels, e.g. made lowercase and had whitespace removed.
#' @param best.match.threshold Numeric on \[0,1\], denoting how high
#' the best match similarity must be to permit a call.
#' @param best.match.discernment Numeric on \[0,1\] denoting the proportion
#' of the top match the next best match must be less than (assuming the
#' next best match does not map to the same harmonized label) in order
#' to accept the top match.
#' @return List; first entry is character vector, a version of
#' the input ancestry reports, modified to best guess ancestry, or
#' NA if the best match is poor; second entry is character vector
#' describing the reasoning for accepting or rejecting partial matches;
#' third is best ancestry match regardless of acceptance; fourth is
#' quality of best match; fifth is second best ancestry match;
#' sixth is quality of second best match.
#' @seealso load.ancestry.linker, harmonize.ancestry, harmonize.ancestry.from.linker
#' @examples
#' phenotype <- c("ibibioo")
#' anc.file <- system.file("external", "nigeria.ancestry.tsv", package = "process.phenotypes")
#' anc.data <- process.phenotypes:::load.ancestry.linker(anc.file)
#' result <- process.phenotypes:::weak.ancestry.match(
#'   phenotype,
#'   anc.data
#' )
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
  ## match is at least X (on [0,1]) and the next best match
  ## is at most Y*100% as good
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
  reasoning <- rep("success", length(res))
  exclude.by.quality <- best.value < best.match.threshold
  res[exclude.by.quality] <- NA
  reasoning[exclude.by.quality] <- "top match quality too low"
  exclude.by.discernment <- !exclude.by.quality & next.best.value >= best.match.discernment * best.value
  override.by.collision <- ancestry.data[best.match] == ancestry.data[next.best.match]
  res[exclude.by.discernment & !override.by.collision] <- NA
  reasoning[exclude.by.discernment & override.by.collision] <- "low discernment but top matches concordant"
  reasoning[exclude.by.discernment & !override.by.collision] <- "low discernment between discordant calls"
  list(
    phenotype = res,
    reasoning = reasoning,
    top.match = names(ancestry.data[best.match]),
    top.value = best.value,
    second.match = names(ancestry.data[next.best.match]),
    second.value = next.best.value
  )
}
