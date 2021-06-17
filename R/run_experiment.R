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
#' @seealso create.phenotype.report
#' @keywords phenotypes
#' @export run.experiment
#' @examples
#' phenotypeprocessing::run.experiment()
run.experiment <- function(phenotype.path,
                           output.path,
                           phenotype.files = c(
                             "CV_FINAL_STORE.tsv",
                             "ET_Final_Store.tsv",
                             "HO_FINAL_STORE.tsv",
                             "MM_FINAl_store.tsv",
                             "Neuro_final_store.tsv",
                             "SC_final_store.tsv"
                           )) {
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
  ## create output directory if needed
  dir.create(output.path, recursive = TRUE)
  ## dispatch report creation for each input file
  for (file in phenotype.files) {
    dataset.tag <- strsplit(file, "_")[[1]][1]
    output.filename <- paste(output.path, paste(dataset.tag, "report.html", sep = "_"), sep = "/")
    create.phenotype.report(
      paste(phenotype.path, file, sep = "/"),
      dataset.tag,
      output.filename
    )
  }
  print("all done hooray!")
}
