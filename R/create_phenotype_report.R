#' Create markdown report summarizing phenotype data in file
#'
#' @details
#' Given phenotype information in tsv file, run some TBD reporting/cleaning
#' and emit summary information.
#' @description
#' TBD
#'
#' @param in.filename character vector, name of input phenotype tsv file
#' @param dataset.tag character vector, dataset-specific tag to prefix variable names
#' @param dataset.yaml character vector, yaml configuration for project
#' @param shared.model.yaml character vector, yaml configuration for shared model specifications
#' @param out.filename character vector, name of output report html file
#' @param quote character vector, character used to quote string tokens; defaults to null
#' @param uniq.var.inclusion.prop numeric, proportion of total values of a string
#' variable that can be unique before tabular output is suppressed from the output report
#' @seealso run.experiment
#' @keywords phenotypes
#' @export create.phenotype.report
#' @examples
#' create.phenotype.report("/path/to/directory/MM_FINAl_store.tsv", "MM", "/output/path/MM_report.html")
create.phenotype.report <- function(in.filename,
                                    dataset.tag,
                                    dataset.yaml,
                                    shared.model.yaml,
                                    out.filename,
                                    magic.fix = TRUE,
                                    quote = "",
                                    uniq.var.inclusion.prop = 1 / 3) {
  ## sanity check for in.filename param
  stopifnot(
    is.vector(in.filename, mode = "character"),
    length(in.filename) == 1,
    file.exists(in.filename)
  )
  ## sanity check for dataset.tag param
  stopifnot(
    is.vector(dataset.tag, mode = "character"),
    length(dataset.tag) == 1
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
  ## TODO: eventually feed dataset tag from yaml
  variable.summary <- map.header(phenotype.data, dataset.tag, config.data)
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
  reformatted.list$phenotype.data <- dependency.failure.handling(
    reformatted.list$phenotype.data,
    reformatted.list$variable.summary
  )

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
      subjects.failing.deps = subjects.failing.deps
    )
  )
  ## temporary fix: report "cleaned" data as tsv file
  ## TODO: replace with something more formal
  write.table(phenotype.data, stringr::str_replace(out.filename, ".html$", ".tsv"),
    row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t"
  )
  write.configuration(variable.summary, stringr::str_replace(out.filename, ".html$", ".yaml"))
}
