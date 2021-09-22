#' Create markdown report summarizing phenotype data in file
#'
#' @details
#' Given phenotype information in tsv file, run some TBD reporting/cleaning
#' and emit summary information.
#' @description
#' TBD
#'
#' @param in.filename character vector, name of input phenotype tsv file
#' @param dataset.yaml character vector, yaml configuration for project
#' @param shared.model.yaml character vector, yaml configuration for shared model specifications
#' @param out.filename character vector, name of output report html file
#' @param quote character vector, character used to quote string tokens; defaults to null
#' @param uniq.var.inclusion.prop numeric, proportion of total values of a string
#' variable that can be unique before tabular output is suppressed from the output report
#' @param write.tsv logical, whether to emit output phenotype data in tsv tab-delimited plaintext
#' @param write.stata logical, whether to emit output phenotype data in STATA .dta format
#' @param write.spss logical, whether to emit output phenotype data in native SPSS format;
#' currently not implemented
#' @param write.sas logical, whether to emit output phenotype data in SAS .sas7bdat format,
#' along with a source .sas file that needs to be run to assign category levels and types
#' @param write.yaml logical, whether to emit final version of stored configuration data
#' in YAML format; currently not tested
#' @seealso run.experiment
#' @keywords phenotypes
#' @export create.phenotype.report
#' @examples
#' create.phenotype.report("/path/to/directory/MM_FINAl_store.tsv", "MM", "/output/path/MM_report.html")
create.phenotype.report <- function(in.filename,
                                    dataset.yaml,
                                    shared.model.yaml,
                                    out.filename,
                                    magic.fix = TRUE,
                                    quote = "",
                                    uniq.var.inclusion.prop = 1 / 3,
                                    write.tsv = TRUE,
                                    write.stata = TRUE,
                                    write.spss = FALSE,
                                    write.sas = TRUE,
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
    sep = "\t", stringsAsFactors = FALSE,
    comment.char = "", quote = quote,
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
  if (magic.fix) {
    phenotype.data <- remove.whitespace(phenotype.data)
    phenotype.data <- collapse.repeats(phenotype.data)
    phenotype.data <- process.unicode.characters(phenotype.data)
    reformatted.list <- exclude.excel.failures(phenotype.data, variable.summary)
    phenotype.data <- reformatted.list$phenotype.data
    variable.summary <- reformatted.list$variable.summary
    variable.summary <- detect.unicode.characters(phenotype.data, variable.summary)
    phenotype.data <- remove.nonword.chars(phenotype.data, variable.summary)
    phenotype.data <- normalize.missing.values(phenotype.data)
  }

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
  if (magic.fix) {
    phenotype.data <- convert.variable.specific.na(
      phenotype.data,
      variable.summary
    )
  }

  ## attempt type conversion on post-cleaning string vectors
  if (magic.fix) {
    reformatted.list <- apply.type.conversions(phenotype.data, variable.summary)
  } else {
    phenotype.data <- type.convert(phenotype.data)
  }

  ## exclude subjects below a given age from the dataset
  if (magic.fix) {
    reformatted.list <- exclude.by.age(
      reformatted.list$phenotype.data, reformatted.list$variable.summary
    )
  }

  ## apply variable-specific range restrictions
  if (magic.fix) {
    reformatted.list <- apply.bounds(
      reformatted.list$phenotype.data,
      reformatted.list$variable.summary
    )
  }

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
  if (magic.fix) {
    reformatted.list <- apply.bounds(
      reformatted.list$phenotype.data,
      reformatted.list$variable.summary
    )
  }

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

  if (magic.fix) {
    phenotype.data <- reformatted.list$phenotype.data
    variable.summary <- reformatted.list$variable.summary
  }

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
    )
  )
  ## report "cleaned" data as assorted output formats
  ## TODO(lightning.auriga): implement handlers for SPSS, SAS
  out.prefix <- stringr::str_replace(out.filename, ".html$", "")
  write.output.formats(
    phenotype.data, variable.summary, out.prefix,
    write.tsv, write.stata, write.spss, write.sas, write.yaml
  )
}
