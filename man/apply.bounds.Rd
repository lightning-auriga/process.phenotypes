% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/per_phenotype_cleaning.R
\name{apply.bounds}
\alias{apply.bounds}
\title{Apply range bounds to each variable as defined in yaml config}
\usage{
apply.bounds(phenotype.data, variable.summary)
}
\arguments{
\item{phenotype.data}{data frame, loaded phenotype data with
standardized headers; all columns should be character vectors}

\item{variable.summary}{list, per-column summary information
and parameters from yaml input}
}
\value{
list, entry 'phenotype.data' is a data frame (modified
version of phenotype input data with bounds applied as appropriate);
entry 'variable.summary' is a list (modified version of input list
with information on entries outside the defined bounds)
}
\description{
Function to pull bounds from yaml config and apply to the correct
variables.
}
\details{
Numeric variables can optionally have min/max bounds defined in
the config (under "bounds") - apply them here by replacing values
outside the bounds with NA.
}
\seealso{
load.configuration
}
\keyword{phenotypes}
\keyword{yaml}