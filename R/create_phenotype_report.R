#' @title
#' Create markdown report summarizing phenotype data and cleaning process
#'
#' @description
#' Given phenotype information in a text file and configuration information
#' in yaml format, this function runs the primary cleaning logic of the
#' package, aggregates summary information about the cleaning process,
#' emits the cleaned data in one of various output formats, and summarizes
#' the cleaning process in an html report.
#'
#' @details
#' This function is the main entry point for phenotype data cleaning.
#' The primary cleaning process proceeds as follows:
#'
#'  - load dataset and shared model configuration
#'  - load phenotype data
#'  - map phenotype headers based on input configuration
#'    - errors about mismatching column headers are detected here, and
#'      generally indicate desync between input configuration and dataset.
#'      this is particularly problematic with iterative runs of SurveyCTO
#'      data, where columns get injected into the output based on the
#'      number of responses to repeat variables.
#'  - make all inputs lowercase
#'  - remove superfluous whitespace
#'  - collapse certain types of character repeats
#'  - apply mappings of Unicode characters to more widely tolerated representations.
#'    - this is a particularly meaningful step for some datasets. if you're curious
#'      about the kinds of chaos we've encountered in datasets, feel free to look
#'      at system.file("unicode_pattern_replacements.tsv", package = "process.phenotypes")
#'  - exclude patterns matching known Excel error codes.
#'    - this processing step will be specifically reported in the output report
#'      as a possible indicator of data malformation. Excel (Calc, Sheets, etc.) should generally not
#'      be used to process biological data at any step. for more information on why,
#'      please see \url{https://www.nature.com/articles/d41586-021-02211-4}.
#'  - detect remaining Unicode characters
#'    - these are reported in the cleaning report html. Unicode characters
#'      are permitted in the output dataset, but in general we've found that
#'      their inclusion is often erroneous, and may potentially conflict with
#'      harmonization of responses across multiple subjects. if Unicode characters
#'      are reported in your cleaning report, please consider extending the mapping
#'      data mentioned above to improve the cleaning process.
#'  - remove certain nonword characters
#'  - normalize missing data based on recognized patterns
#'  - apply consent inclusion and exclusion, if configured
#'  - normalize missing data based on user-configured patterns
#'  - apply type conversions based on user-configured types
#'  - apply age exclusion
#'  - remove subjects with empty subject IDs
#'  - apply user-configured bounds to numeric variables
#'  - if `subject_ancestry = TRUE` is found for any variables in the user configuration,
#'    apply heuristic harmonization with backend annotations
#'    - this is currently configured to run versus an _ad hoc_ set of Nigerian
#'      ancestry groups, and should not be used in other cases without first
#'      creating a reference list and adding it to the package. if there's sufficient
#'      interest, this methodology may be expanded and improved
#'  - if `derived` block is present in user configuration, create derived
#'    variables from cleaned primary variables
#'  - apply bounds on derived numeric variables
#'  - check user-configured dependencies between variables
#'  - if dependency failures were detected, apply user-configured handling on failures
#'  - aggregate summary information about: distributions, NA conversions, etc., for html report
#'  - remove subjects that fail NA rate limits
#'    - this is not much used currently, but is intended to flag truly toxic
#'      subjects that may in fact represent catastrophic row or column shifts in
#'      the input data. please be extremely cautious if any input subjects are removed
#'      based on this criterion
#'  - emit cleaning report as html
#'  - if requested, emit configuration data in yaml format
#'  - if requested, emit cleaned data as tsv or in formats for other stats languages
#'
#' The actual functionality and configuration in this package is extensive and frequently
#' expanding. Full details on exactly what cleaning is performed and available
#' configuration options for the input data are available at
#' \url{https://54geneprocessphenotypes.readthedocs.io/en/latest/}
#'
#' @param in.filename Character vector filename of input phenotype data.
#' @param dataset.yaml character vector filename of the yaml-format configuration
#' data for the dataset.
#' @param shared.model.yaml Character vector filename of the yaml configuration
#' for shared model specifications used by the dataset configuration.
#' @param out.filename Character vector filename of output report html file.
#' @param quote Character vector used to quote string tokens in input phenotypes,
#' and passed to read.table. Defaults to NULL. This parameter is exposed for
#' greater compability with unpredictable input formats (see Details).
#' @param sep Character vector used to delimit input fields in input phenotypes,
#' and passed to read.table. Defaults to tab (\\t). This parameter is exposed for
#' greater compatibility with unpredictable input formats (see Details).
#' @param uniq.var.inclusion.prop Numeric proportion of total values of a string
#' variable that can be unique before tabular output is automatically suppressed
#' from the output report. If set to a value greater than 1, all variables are
#' reported; if set below 0, all variable reporting is suppressed.
#' @param write.tsv Logical indicating whether to emit output phenotype data in tsv
#' tab-delimited plaintext. This is the primary intended output control for the
#' function, and should probably be set to TRUE.
#' @param write.stata Logical indicating whether to emit output phenotype data in
#' STATA .dta format. This method was added experimentally in order to provide
#' useful output formats for users of other statistical languages, and is subject
#' to modification based on downstream user feedback.
#' @param write.spss Logical indicating whether to emit output phenotype data
#' in SPSS .zsav format. This method was added experimentally in order to provide
#' useful output formats for users of other statistical languages, and is subject
#' to modification based on downstream user feedback.
#' @param write.sas Logical indicating whether to emit output phenotype data
#' in SAS .sas7bdat format, along with a source .sas file that needs to be run to
#' assign category levels and types. This method was added experimentally in order
#' to provide useful output formats for users of other statistical languages,
#' and is subject to modification based on downstream user feedback.
#' @param write.yaml Logical indicating whether to emit final version of stored
#' configuration data in YAML format. In addition to general recordkeeping, the
#' goal of this output is to provide a configuration file that can be used to
#' reload the output tsv back into process.phenotypes.
#' @return NULL
#' @export create.phenotype.report
#' @examples
#' ## create.phenotype.report operates on input files, which can be constructed
#' ## from dataframes and lists in R if desired.
#' input.phenotypes <- tempfile("cpr.input.data", fileext = ".tsv")
#' input.dataset.yaml <- tempfile("cpr.input.data", fileext = ".dataset.yaml")
#' input.shared.models <- tempfile("cpr.input.data", fileext = ".shared_models.yaml")
#' output.html <- tempfile("cpr.output.data", fileext = ".html")
#' ## a minimal dataset has to contain at least a subject ID column and
#' ## a subject age column. everything else is optional.
#' pheno.data <- data.frame(
#'   var1 = c("A", "B", "C", "D", "E"),
#'   var2 = 16:20,
#'   var3 = sample(c("yes", "no"), 5, replace = TRUE)
#' )
#' var.summary <- list(
#'   tag = "HW",
#'   globals = list(
#'     consent_inclusion_file = NULL,
#'     consent_exclusion_file = NULL,
#'     max_invalid_datatypes_per_subject = 10,
#'     min_age_for_inclusion = 18
#'   ),
#'   variables = list(
#'     HW00001 = list(
#'       name = "var1",
#'       type = "string",
#'       suppress_reporting = TRUE,
#'       subject_id = TRUE
#'     ),
#'     HW00002 = list(
#'       name = "var2",
#'       type = "numeric",
#'       subject_age = TRUE
#'     ),
#'     HW00003 = list(
#'       name = "var3",
#'       shared_model = "yesno"
#'     )
#'   )
#' )
#' shared.models <- list(models = list("yesno" = list(
#'   type = "categorical",
#'   levels = list(
#'     "1" = list(name = "no"),
#'     "2" = list(name = "yes")
#'   )
#' )))
#' write.table(pheno.data, input.phenotypes,
#'   row.names = FALSE,
#'   col.names = TRUE, quote = TRUE, sep = "\t"
#' )
#' yaml::write_yaml(var.summary, input.dataset.yaml)
#' yaml::write_yaml(shared.models, input.shared.models)
#' ## output datasets take their output prefix from the filename provided for the html report,
#' ## so for example purposes the other output formats are suppressed.
#'
#' \dontrun{
#' create.phenotype.report(input.phenotypes,
#'   input.dataset.yaml,
#'   input.shared.models,
#'   output.html,
#'   sep = "\t",
#'   quote = "\"",
#'   write.tsv = FALSE
#' )
#' }
#' @importFrom stats qnorm quantile sd
#' @importFrom utils read.table type.convert write.table
#' @importFrom magrittr `%>%`
create.phenotype.report <- function(in.filename,
                                    dataset.yaml,
                                    shared.model.yaml,
                                    out.filename,
                                    quote = "",
                                    sep = "\t",
                                    uniq.var.inclusion.prop = 1 / 3,
                                    write.tsv = TRUE,
                                    write.stata = FALSE,
                                    write.spss = FALSE,
                                    write.sas = FALSE,
                                    write.yaml = FALSE) {
  ## sanity check for in.filename param
  stopifnot(
    is.vector(in.filename, mode = "character"),
    length(in.filename) == 1,
    file.exists(in.filename)
  )
  ## sanity check for dataset.yaml param
  stopifnot(
    is.vector(dataset.yaml, mode = "character"),
    length(dataset.yaml) == 1
  )
  ## sanity check for shared.model.yaml param
  stopifnot(
    is.vector(shared.model.yaml, mode = "character"),
    length(shared.model.yaml) == 1
  )
  ## sanity check for out.filename param
  stopifnot(
    is.vector(out.filename, mode = "character"),
    length(out.filename) == 1
  )
  ## sanity check for unique.variable.value.inclusion.proportion param
  stopifnot(
    is.numeric(uniq.var.inclusion.prop),
    length(uniq.var.inclusion.prop) == 1
  )
  ## sanity checks for output format control variables
  stopifnot(
    is.logical(write.tsv),
    length(write.tsv) == 1
  )
  stopifnot(
    is.logical(write.stata),
    length(write.stata) == 1
  )
  stopifnot(
    is.logical(write.spss),
    length(write.spss) == 1
  )
  stopifnot(
    is.logical(write.sas),
    length(write.sas) == 1
  )
  stopifnot(
    is.logical(write.yaml),
    length(write.yaml) == 1
  )

  phenotype.data <- read.table(in.filename,
    header = TRUE,
    stringsAsFactors = FALSE,
    comment.char = "", quote = quote, sep = sep,
    check.names = FALSE,
    colClasses = "character"
  )

  ## load project yaml configuration data
  config.data <- load.configuration(dataset.yaml, shared.model.yaml)

  ## drop columns with NA or empty column names
  phenotype.data <- remove.invalid.columns(phenotype.data)

  ## sanitize headers
  variable.summary <- map.header(phenotype.data, config.data$tag, config.data)
  phenotype.data <- sanitize.header(phenotype.data, variable.summary)

  ## clean up strings (global functions across all variables)
  phenotype.data <- make.lowercase(phenotype.data)
  phenotype.data <- remove.whitespace(phenotype.data)
  phenotype.data <- collapse.repeats(phenotype.data)
  phenotype.data <- process.unicode.characters(phenotype.data)
  reformatted.list <- exclude.excel.failures(phenotype.data, variable.summary)
  phenotype.data <- reformatted.list$phenotype.data
  variable.summary <- reformatted.list$variable.summary
  variable.summary <- detect.unicode.characters(phenotype.data, variable.summary)
  phenotype.data <- remove.nonword.chars(phenotype.data, variable.summary)
  phenotype.data <- normalize.missing.values(phenotype.data)

  ## as soon as possible, remove subjects lacking consent
  ## I'd prefer to do this earlier, but without basic cleaning, sane specifications
  ## of the subject IDs will cause everything to get excluded
  reformatted.list <- apply.consent.exclusion(phenotype.data, variable.summary)
  phenotype.data <- reformatted.list$phenotype.data
  variable.summary <- reformatted.list$variable.summary
  ## add a sanity check: make sure there is at least one subject left
  ## after consent exclusion
  stopifnot(nrow(phenotype.data) > 0)

  ## apply variable-specific NA values
  phenotype.data <- convert.variable.specific.na(
    phenotype.data,
    variable.summary
  )

  ## attempt type conversion on post-cleaning string vectors
  reformatted.list <- apply.type.conversions(phenotype.data, variable.summary)

  ## exclude subjects below a given age from the dataset
  reformatted.list <- exclude.by.age(
    reformatted.list$phenotype.data, reformatted.list$variable.summary
  )

  ## exclude subjects without a subject ID from the dataset
  reformatted.list <- exclude.by.missing.subject.id(
    reformatted.list$phenotype.data, reformatted.list$variable.summary
  )

  ## apply variable-specific range restrictions
  reformatted.list <- apply.bounds(
    reformatted.list$phenotype.data,
    reformatted.list$variable.summary
  )

  ## attempt to harmonize ancestry variables using target list of ancestry tags
  reformatted.list <- harmonize.ancestry(
    reformatted.list$phenotype.data,
    reformatted.list$variable.summary,
    "nigeria",
    0.8,
    0.75
  )

  ## create derived variables from cleanest possible versions of standard variables
  reformatted.list <- create.derived.variables(
    reformatted.list$phenotype.data,
    reformatted.list$variable.summary
  )

  ## apply variable-specific range restrictions a second time, so that
  ## newly-calculated derived variables will also have bounds applied
  reformatted.list <- apply.bounds(
    reformatted.list$phenotype.data,
    reformatted.list$variable.summary
  )

  ## enforce yaml-specified variable relationships
  reformatted.list$variable.summary <- check.variable.dependencies(
    reformatted.list$phenotype.data,
    reformatted.list$variable.summary
  )
  ## apply NA exclusions based on dependency results
  ## however: do not apply in place, as this makes the output
  ## contingency tables very uninformative
  reformatted.list.na.applied <- dependency.failure.handling(
    reformatted.list$phenotype.data,
    reformatted.list$variable.summary
  )
  phenotype.data.na.applied <- reformatted.list.na.applied$phenotype.data
  reformatted.list$variable.summary <- reformatted.list.na.applied$variable.summary

  phenotype.data <- reformatted.list$phenotype.data
  variable.summary <- reformatted.list$variable.summary

  for (name in names(variable.summary$variables)) {
    if (is.vector(phenotype.data[, name], mode = "numeric")) {
      variable.summary$variables[[name]]$summary <- c(
        mean(phenotype.data[, name], na.rm = TRUE),
        quantile(phenotype.data[, name], probs = seq(0, 1, 0.1), na.rm = TRUE),
        length(which(is.na(phenotype.data[, name])))
      )
      names(variable.summary$variables[[name]]$summary) <- c(
        "Mean",
        "Min",
        names(variable.summary$variables[[name]]$summary)[3:6],
        "Median",
        names(variable.summary$variables[[name]]$summary)[8:11],
        "Max",
        "NAs"
      )
    } else {
      variable.summary$variables[[name]]$summary <- table(phenotype.data[, name],
        useNA = "ifany"
      )
    }
  }

  ## TODO(lightning.auriga): add things to summary list
  subjects.wrong.type <- aggregate.subjects.wrong.type(variable.summary)
  variables.wrong.type <- aggregate.variables.wrong.type(variable.summary)
  nas.by.subject <- compute.subject.na.count(phenotype.data, variable.summary)
  subjects.failing.deps <- aggregate.subject.dep.failures(variable.summary)

  ## apply per-subject exclusions based on some set of computed metrics and a bound
  phenotype.data.na.applied <- exclude.subjects.by.metric(
    phenotype.data.na.applied,
    variable.summary,
    subjects.wrong.type,
    variable.summary$globals$max_invalid_datatypes_per_subject
  )

  ## find Rmd file from system installation
  rmarkdown.template <- system.file("rmd", "report.Rmd",
    package = "process.phenotypes"
  )
  ## render output html report for this phenotype dataset
  rmarkdown::render(rmarkdown.template,
    output_file = out.filename,
    output_dir = dirname(out.filename),
    params = list(
      dataset.name = in.filename,
      variable.summary = variable.summary,
      phenotype.data = phenotype.data,
      unique.variable.value.inclusion.proportion = uniq.var.inclusion.prop,
      subjects.wrong.type = subjects.wrong.type,
      variables.wrong.type = variables.wrong.type,
      nas.by.subject = nas.by.subject,
      subjects.failing.deps = subjects.failing.deps,
      subj.invalid.type.max = variable.summary$globals$max_invalid_datatypes_per_subject
    ),
    intermediates_dir = tempdir(),
    knit_root_dir = tempdir()
  )
  ## report "cleaned" data as assorted output formats
  out.prefix <- stringr::str_replace(out.filename, ".html$", "")
  write.output.formats(
    phenotype.data.na.applied, variable.summary, out.prefix,
    write.tsv, write.stata, write.spss, write.sas, write.yaml
  )
}
