#' Launch primary runs for all phenotype files
#'
#' @description
#' Single entry point function to deploy markdown reports
#' for each phenotype tsv
#' @details
#' This is configured for the June 2021 Heritage "freeze"
#' and should probably be named to reflect that if this
#' is ever used in other contexts lol.
#' @param phenotype.path character vector, absolute or relative
#' path to directory containing phenotype files
#' @param output.path character vector, absolute or relative
#' path to directory for output report html files
#' @param phenotype.files character vector, one or more phenotype
#' files to be processed. all files expected to be .tsv
#' @param yaml.dir character vector, directory containing
#' project-specific and shared model yaml configuration files
#' @seealso create.phenotype.report
#' @keywords phenotypes
#' @examples
#' process.phenotypes("/path/to/phenotypes/pre-audit", "my_results")
process.phenotypes <- function(phenotype.path,
                               output.path,
                               phenotype.files = c(
                                 "CV_raw.tsv",
                                 "DM_raw.tsv",
                                 "ET_raw.tsv",
                                 "HO_raw.tsv",
                                 "MM_raw.tsv",
                                 "Neuro_raw.tsv",
                                 "SC_raw.tsv",
                                 "SO_raw.tsv"
                               ),
                               yaml.dir = "yaml-configuration") {
  ## sanity check on phenotype.path param
  stopifnot(
    is.vector(phenotype.path, mode = "character"),
    length(phenotype.path) == 1
  )
  ## sanity check on output.path param
  stopifnot(
    is.vector(output.path, mode = "character"),
    length(output.path) == 1
  )
  ## sanity check on phenotype.files param
  stopifnot(is.vector(phenotype.files, mode = "character"))
  ## sanity check on yaml.dir param
  stopifnot(
    is.vector(yaml.dir, mode = "character"),
    length(yaml.dir) == 1,
    dir.exists(yaml.dir)
  )
  ## create output directory if needed
  dir.create(output.path, recursive = TRUE, showWarnings = FALSE)
  ## temporary: assume shared model yaml is in a fixed name under yaml.dir
  shared.model.yaml <- paste(yaml.dir, "shared-models.yaml", sep = "/")
  ## dispatch report creation for each input file
  for (file in phenotype.files) {
    ## TODO: replace this tag assumption with configurable tag from yaml file
    dataset.tag <- stringr::str_replace(file, ".tsv", "")
    ## temporary: assume dataset-specific yaml is in a fixed name under yaml.dir
    dataset.yaml <- paste(yaml.dir, paste(dataset.tag, "yaml", sep = "."), sep = "/")
    output.filename <- paste(output.path, paste(dataset.tag, "report.html", sep = "_"), sep = "/")
    create.phenotype.report(
      paste(phenotype.path, file, sep = "/"),
      dataset.yaml,
      shared.model.yaml,
      output.filename,
      quote = "\""
    )
  }
  print("all done hooray!")
}
