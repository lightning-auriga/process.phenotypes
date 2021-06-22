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
#' @param out.filename character vector, name of output report html file
#' @seealso run.experiment
#' @keywords phenotypes
#' @export create.phenotype.report
#' @examples
#' create.phenotype.report("/path/to/directory/MM_FINAl_store.tsv", "MM", "/output/path/MM_report.html")
create.phenotype.report <- function(in.filename,
                                    dataset.tag,
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
  ## sanity check for out.filename param
  stopifnot(
    is.vector(out.filename, mode = "character"),
    length(out.filename) == 1
  )

  phenotype.data <- read.table(in.filename,
    header = TRUE,
    sep = "\t", stringsAsFactors = FALSE,
    comment.char = "", quote = "",
    check.names = FALSE
  )

  ## drop columns with NA or empty column names
  phenotype.data <- phenotypeprocessing::remove.invalid.columns(phenotype.data)

  ## sanitize headers
  variable.summary <- phenotypeprocessing::map.header(phenotype.data, dataset.tag)
  phenotype.data <- phenotypeprocessing::sanitize.header(phenotype.data, variable.summary)

  ## TODO(lightning.auriga): modify phenotype data based on previous observations
  for (name in names(variable.summary)) {
    if (is.vector(phenotype.data[, name], mode = "numeric")) {
      variable.summary[[name]]$summary <- summary(phenotype.data[, name])
    } else {
      variable.summary[[name]]$summary <- table(phenotype.data[, name])
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
