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
#' @seealso run.experiment
#' @keywords phenotypes
#' @export create.phenotype.report
#' @examples
#' create.phenotype.report("/path/to/directory/MM_FINAl_store.tsv", "MM", "/output/path/MM_report.html")
create.phenotype.report <- function(in.filename,
                                    dataset.tag,
                                    dataset.yaml,
                                    shared.model.yaml,
                                    out.filename) {
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

  phenotype.data <- read.table(in.filename,
    header = TRUE,
    sep = "\t", stringsAsFactors = FALSE,
    comment.char = "", quote = "",
    check.names = FALSE,
    colClasses = "character"
  )

  ## load project yaml configuration data
  config.data <- phenotypeprocessing::load.configuration(dataset.yaml, shared.model.yaml)

  ## drop columns with NA or empty column names
  phenotype.data <- phenotypeprocessing::remove.invalid.columns(phenotype.data)

  ## sanitize headers
  ## TODO: eventually feed dataset tag from yaml
  variable.summary <- phenotypeprocessing::map.header(phenotype.data, dataset.tag, config.data)
  phenotype.data <- phenotypeprocessing::sanitize.header(phenotype.data, variable.summary)

  ## clean up strings (global functions across all variables)
  phenotype.data <- phenotypeprocessing::make.lowercase(phenotype.data)
  phenotype.data <- phenotypeprocessing::remove.whitespace(phenotype.data)
  phenotype.data <- phenotypeprocessing::collapse.repeats(phenotype.data)
  phenotype.data <- phenotypeprocessing::remove.nonword.chars(phenotype.data)
  phenotype.data <- phenotypeprocessing::normalize.missing.values(phenotype.data)

  ## attempt type conversion on post-cleaning string vectors
  reformatted.list <- phenotypeprocessing::apply.type.conversions(phenotype.data, variable.summary)

  ## exclude subjects below a given age from the dataset
  reformatted.list <- phenotypeprocessing::exclude.by.age(
    reformatted.list$phenotype.data, reformatted.list$variable.summary
  )

  ## TODO: apply variable-specific NA values
  ## TODO: apply variable-specific range restrictions
  reformatted.list <- phenotypeprocessing::apply.bounds(
    reformatted.list$phenotype.data,
    reformatted.list$variable.summary
  )
  ## TODO: enforce yaml-specified variable relationships

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

  ## find Rmd file from system installation
  rmarkdown.template <- system.file("rmd", "report.Rmd",
    package = "phenotypeprocessing"
  )
  ## render output html report for this phenotype dataset
  rmarkdown::render(rmarkdown.template,
    output_file = out.filename,
    output_dir = dirname(out.filename),
    params = list(
      dataset.name = in.filename,
      variable.summary = variable.summary
    )
  )
}
