#' @title
#' Report final phenotype dataset to file after processing
#'
#' @description
#' This function contains handlers for the primary supported output formats
#' for this package: plaintext tab-delimited, STAT (.dta), SAS (.sas7bdat),
#' SPSS (.zsav), and YAML (experimental). Additional control
#' parameters are likely to be added at a later date. Any number of outputs
#' can be selected and they will all be reported sequentially.
#'
#' @details
#' These files are generated with haven, foreign, and yaml. Each of those
#' libraries has its idiosyncracies, and in particular with the SAS/STATA/SPSS
#' files, the version restrictions on their proprietary formats make it such
#' that these output files may be of limited utility. We generally recommend
#' that the end user focus on using the output tsv file, and if the other
#' stats languages are desired, just load from tsv first.
#'
#' The output yaml file is a modified version of the input dataset yaml
#' that, in theory, can be used to reload the output tsv into
#' process.phenotypes for additional processing. We do not generally recommend
#' that use, however, and instead suggest that people iterate on the
#' initial input dataset/shared models yaml and rerun as needed. In our experience,
#' even fairly large datasets only take some seconds to run end-to-end,
#' so the reruns are not overly burdensome.
#'
#' @param phenotype.data Data frame of stored phenotype dataset with all cleaning
#' and filtering applied.
#' @param variable.summary List of loaded and modified configuration data at
#' the end of a cleaning run.
#' @param out.prefix Character vector prefix for all requested output files.
#' This is based on the user-specified html output filename provided to
#' create.phenotype.report, and will have the relevant format-specific suffixes appended.
#' @param write.tsv Logical controlling whether to emit output phenotype data in tsv
#' tab-delimited plaintext.
#' @param write.stata Logical controlling whether to emit output phenotype data in STATA
#' .dta format.
#' @param write.spss Logical controlling whether to emit output phenotype data in SPSS
#' .zsav format.
#' @param write.sas Logical controlling whether to emit output phenotype data in SAS
#' .sas7bdat format, along with a source .sas file that needs to be run to assign category
#' levels and types.
#' @param write.yaml Logical controlling whether to emit final version of stored
#' configuration data in YAML format. This output is a bit idiosyncratic, and should be
#' expected to change somewhat over time.
#' @return NULL
#' @seealso create.phenotype.report
#' @examples
#' outprefix <- tempfile("wof_output_prefix")
#' phenotype.data <- data.frame(
#'   HW00001 = c("A", "B", "C", "D"),
#'   HW00002 = 20:23
#' )
#' variable.summary <- list(
#'   tag = "HW",
#'   globals = list(
#'     min_age_for_inclusion = 16,
#'     max_invalid_datatypes_per_subject = 10,
#'     consent_inclusion_file = NULL,
#'     consent_exclusion_file = NULL
#'   ),
#'   variables = list(
#'     HW00001 = list(
#'       name = "subjid",
#'       type = "string",
#'       subject_id = TRUE
#'     ),
#'     HW00002 = list(
#'       name = "subjage",
#'       type = "numeric",
#'       subject_age = TRUE
#'     )
#'   )
#' )
#' process.phenotypes:::write.output.formats(
#'   phenotype.data,
#'   variable.summary,
#'   outprefix,
#'   TRUE, TRUE, TRUE, TRUE, TRUE
#' )
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
