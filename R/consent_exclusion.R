#' Apply consent exclusion to data
#'
#' @description
#' Load subject list(s) specifying inclusion and exclusion of
#' subjects based on consent status. Depending on the presence
#' or absence of subjects and input lists, report summary
#' information about how many people have definitive information.
#'
#' @details
#' TBD
#'
#' @param phenotype.data data.frame, input phenotype information
#' with a subject ID column
#' @param variable.summary list, dataset configuration information
#' with a variable tagged with "subject_id: true"
#' @return list, first entry modified version of the input
#' phenotype data with any excluded subjects removed; second
#' entry modified version of input config data with summary
#' information included about consent status for report.
apply.consent.exclusion <- function(phenotype.data, variable.summary) {
  ## annotate initial sample size
  variable.summary$total.initial.sample.size <- nrow(phenotype.data)
  ## get filenames from configuration data
  subj.inc.filename <- variable.summary$globals$consent_inclusion_file
  subj.exc.filename <- variable.summary$globals$consent_exclusion_file
  ## either or both files can be NULL
  subj.inc <- c()
  subj.exc <- c()
  has.subj.inc <- !is.null(subj.inc.filename)
  has.subj.exc <- !is.null(subj.exc.filename)
  if (has.subj.inc) {
    stopifnot(file.exists(subj.inc.filename))
    if (file.info(subj.inc.filename)$size == 0) {
      subj.inc <- character()
    } else {
      subj.inc <- read.table(subj.inc.filename,
        header = FALSE,
        stringsAsFactors = FALSE, comment.char = ""
      )[, 1]
      subj.inc <- make.lowercase(subj.inc)
      subj.inc <- remove.whitespace(subj.inc)
    }
  }
  if (has.subj.exc) {
    stopifnot(file.exists(subj.exc.filename))
    if (file.info(subj.exc.filename)$size == 0) {
      subj.exc <- character()
    } else {
      subj.exc <- read.table(subj.exc.filename,
        header = FALSE,
        stringsAsFactors = FALSE, comment.char = ""
      )[, 1]
      subj.exc <- make.lowercase(subj.exc)
      subj.exc <- remove.whitespace(subj.exc)
    }
  }
  ## if there are collisions, this is treated as an error
  stopifnot(length(which(subj.inc %in% subj.exc)) == 0)
  subj.id.index <- find.subject.id.index(variable.summary)
  ## step 1: remove subjects who are excluded
  variable.summary$subjects.consent.no <- as.integer(0)
  if (has.subj.exc) {
    subj.consent.no <- phenotype.data[, subj.id.index] %in% subj.exc
    phenotype.data <- phenotype.data[!subj.consent.no, ]
    variable.summary$subjects.consent.no <- length(which(subj.consent.no))
  }
  ## step 2: find for bookkeeping subjects who are included
  variable.summary$subjects.consent.yes <- as.integer(0)
  variable.summary$subjects.ambiguous.consent <- character()
  if (has.subj.inc) {
    subj.consent.yes <- phenotype.data[, subj.id.index] %in% subj.inc
    variable.summary$subjects.consent.yes <- length(which(subj.consent.yes))
    ## step 3: report subjects not included on either list
    ## (if both lists were provided)
    if (has.subj.exc) {
      variable.summary$subjects.ambiguous.consent <-
        phenotype.data[!subj.consent.yes, subj.id.index]
    }
    phenotype.data <- phenotype.data[subj.consent.yes, ]
  }
  list(
    phenotype.data = phenotype.data,
    variable.summary = variable.summary
  )
}
