#' Report final phenotype dataset after processing
#'
#' @description
#' This function contains handlers for the primary supported output formats
#' for this package: plaintext tab-delimited, STAT (.dta), SAS (.sas7bdat),
#' SPSS (.zsav), and YAML (experimental). Additional control
#' parameters are likely to be added at a later data. Any number of outputs
#' can be selected and they will all be reported sequentially.
#'
#' @param phenotype.data data.frame, stored phenotype dataset with all cleaning
#' and filtering applied
#' @param variable.summary list, loaded and modified configuration data at
#' the end of a cleaning run
#' @param out.prefix character vector, prefix for all requested output files.
#' will have the relevant format-specific suffixes appended
#' @param write.tsv logical, whether to emit output phenotype data in tsv tab-delimited plaintext
#' @param write.stata logical, whether to emit output phenotype data in STATA .dta format
#' @param write.spss logical, whether to emit output phenotype data in SPSS .zsav format
#' @param write.sas logical, whether to emit output phenotype data in SAS .sas7bdat format,
#' along with a source .sas file that needs to be run to assign category levels and types
#' @param write.yaml logical, whether to emit final version of stored configuration data
#' in YAML format; currently not tested
write.output.formats <- function(phenotype.data,
                                 variable.summary,
                                 out.prefix,
                                 write.tsv,
                                 write.stata,
                                 write.spss,
                                 write.sas,
                                 write.yaml) {
  stopifnot(is.data.frame(phenotype.data))
  stopifnot(is.list(variable.summary))
  stopifnot(
    is.vector(out.prefix, mode = "character"),
    length(out.prefix) == 1
  )
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
  if (write.tsv) {
    write.table(phenotype.data, paste(out.prefix, ".tsv", sep = ""),
      row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t"
    )
  }
  if (write.stata) {
    haven::write_dta(phenotype.data, paste(out.prefix, ".dta", sep = ""),
      version = 14, label = "phenotype.data"
    )
  }
  if (write.spss) {
    haven::write_sav(phenotype.data, paste(out.prefix, ".zsav", sep = ""), compress = TRUE)
  }
  if (write.sas) {
    ## note that foreign does not directly support sas7bcat output;
    ## so the function emits two output files, the sas7bdat as expected,
    ## and a SAS source code file that needs to be run to assign
    ## appropriate types and category levels to the sas7bdat content.
    foreign::write.foreign(phenotype.data,
      paste(out.prefix, ".sas7bdat", sep = ""),
      paste(out.prefix, ".sas", sep = ""),
      package = "SAS",
      dataname = "phenotype.data",
      validvarname = "V7"
    )
  }
  if (write.yaml) {
    write.configuration(variable.summary, paste(out.prefix, ".yaml", sep = ""))
  }
}
