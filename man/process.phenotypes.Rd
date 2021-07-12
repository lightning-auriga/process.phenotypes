% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_phenotypes.R
\name{process.phenotypes}
\alias{process.phenotypes}
\title{Launch primary runs for all phenotype files}
\usage{
process.phenotypes(
  phenotype.path,
  output.path,
  phenotype.files = c("CV_FINAL_STORE.tsv", "ET_Final_Store.tsv", "HO_FINAL_STORE.tsv",
    "MM_FINAl_store.tsv", "Neuro_final_store.tsv", "SC_final_store.tsv"),
  yaml.dir = "yaml-configuration"
)
}
\arguments{
\item{phenotype.path}{character vector, absolute or relative
path to directory containing phenotype files}

\item{output.path}{character vector, absolute or relative
path to directory for output report html files}

\item{phenotype.files}{character vector, one or more phenotype
files to be processed. all files expected to be .tsv}

\item{yaml.dir}{character vector, directory containing
project-specific and shared model yaml configuration files}
}
\description{
Single entry point function to deploy markdown reports
for each phenotype tsv
}
\details{
This is configured for the June 2021 Heritage "freeze"
and should probably be named to reflect that if this
is ever used in other contexts lol.
}
\examples{
process.phenotypes("/path/to/phenotypes/audit_jun2021", "my_results")
}
\seealso{
create.phenotype.report
}
\keyword{phenotypes}