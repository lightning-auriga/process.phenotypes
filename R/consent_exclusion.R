#' @title
#' Apply consent exclusion to data.
#'
#' @description
#' Load subject list(s) specifying inclusion and exclusion of
#' subjects based on consent status. Depending on the presence
#' or absence of subjects and input lists, report summary
#' information about how many people have definitive information.
#'
#' @details
#' The concept of applying consent data is integrated directly
#' into the process.phenotypes logic chain. If either inclusion
#' or exclusion file (but not both) is provided, the relevant
#' process will still be conducted. However, if both are specified,
#' an additional set of subjects with uncertain consent status
#' will be computed, excluded from output, and reported in the
#' cleaning report.
#'
#' Consent lists can be NULL in the input configuration (e.g.
#' `consent_inclusion_file: ~` in yaml). This is perfectly
#' appropriate in contexts where consent is not applicable
#' (e.g. model organism data, anonymized collection, or just
#' random files unrelated to human subjects). We nevertheless
#' recommend its use in all applicable situations.
#'
#' If a subject is specified in both the inclusion and exclusion
#' file, exclusion takes priority. If this behavior is not desired,
#' preprocess the input files such that the subject is only
#' present in the inclusion set.
#'
#' @param phenotype.data Data frame, containing input phenotype information
#' with a subject ID column.
#' @param variable.summary List of dataset configuration information
#' with a variable tagged with `subject_id = TRUE` to link subject
#' data to the consent list.
#' @return List, with first entry `phenotype.data` a modified version of the input
#' phenotype data with any excluded subjects removed, and second
#' entry `variable.summary` a modified version of input config data with summary
#' information included about consent status for report.
#' @examples
#' input.consent.inc.file <- tempfile("ace_example_inc")
#' input.consent.exc.file <- tempfile("ace_example_exc")
#' consent.inclusion <- c("A", "B")
#' consent.exclusion <- c("C", "D")
#' write.table(cbind(consent.inclusion), input.consent.inc.file,
#'   row.names = FALSE, col.names = FALSE, quote = FALSE
#' )
#' write.table(cbind(consent.exclusion), input.consent.exc.file,
#'   row.names = FALSE, col.names = FALSE, quote = FALSE
#' )
#' pheno.data <- data.frame(
#'   HW00001 = c("A", "B", "C", "D", "E"),
#'   HW00002 = 1:5
#' )
#' ## note that by this stage of the process.phenotypes logic
#' ## chain, the user-specified configuration entries from the
#' ## input yaml have been sequestered in a block `params`
#' ## for each variable.
#' var.sum <- list(
#'   globals = list(
#'     consent.inclusion.file = input.consent.inc.file,
#'     consent.exclusion.file = input.consent.exc.file
#'   ),
#'   variables = list(
#'     HW00001 = list(
#'       params = list(
#'         name = "subjid",
#'         type = "string",
#'         subject_id = TRUE
#'       )
#'     ),
#'     HW00002 = list(
#'       params = list(
#'         name = "measure",
#'         type = "numeric"
#'       )
#'     )
#'   )
#' )
#' res <- process.phenotypes:::apply.consent.exclusion(pheno.data, var.sum)
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
  ## if there are collisions, exclusion takes priority
  subj.inc <- subj.inc[!(subj.inc %in% subj.exc)]
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
