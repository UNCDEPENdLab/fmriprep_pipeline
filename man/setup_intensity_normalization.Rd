% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup_postproc.R
\name{setup_intensity_normalization}
\alias{setup_intensity_normalization}
\title{Specify the intensity normalization settings for postprocessing}
\usage{
setup_intensity_normalization(scfg = list())
}
\arguments{
\item{scfg}{a study configuration object created by \code{setup_study}}
}
\value{
a modified version of \code{scfg} with the \verb{$postprocess$intensity_normalize} field populated
}
\description{
Specify the intensity normalization settings for postprocessing
}
\keyword{internal}
