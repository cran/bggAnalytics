% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_utils.R
\name{fetches}
\alias{fetches}
\alias{.fetch_internal}
\title{Generalised fetch for every class}
\usage{
.fetch_internal(xml, variable_names, var_specs, compress = FALSE)
}
\arguments{
\item{xml}{XML nodeset.}

\item{variable_names}{Character vector of variable names to extract.}

\item{var_specs}{Data.table with parameter specification.}

\item{compress}{a logical value, whether variables should be compressed.}
}
\value{
List of variables. Variables marked as 'scalar' in param
  specification will be unlisted.
}
\description{
It is a universal tool for fetching variables from XMLs, should be used
within fetch methods from every bgg* class. It uses internal variable
parameter specification to extract data.
}
\keyword{internal}
