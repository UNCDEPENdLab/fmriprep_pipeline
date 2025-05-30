% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup_study.R
\name{setup_bids_conversion}
\alias{setup_bids_conversion}
\title{Specify the heudiconv settings}
\usage{
setup_bids_conversion(scfg, fields = NULL)
}
\arguments{
\item{scfg}{a study configuration object, as produced by \code{load_study} or \code{setup_study}}

\item{fields}{a character vector of fields to be prompted for. If \code{NULL}, all fields will be prompted for.}
}
\value{
a modified version of \code{scfg} with \verb{$heudiconv} populated
}
\description{
Specify the heudiconv settings
}
\keyword{internal}
