% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipeline_functions.R
\name{extract_capturing_groups}
\alias{extract_capturing_groups}
\title{helper function to extract capturing groups from a string}
\usage{
extract_capturing_groups(strings, pattern, groups = NULL, sep = "_", ...)
}
\arguments{
\item{strings}{a character vector containing the strings to be processed}

\item{pattern}{a regex pattern to match the strings}

\item{groups}{a numeric vector specifying the indices of the capturing groups to be extracted.
Default: \code{NULL}, which extracts all capturing groups.}

\item{sep}{a character string to separate the captured groups. Default: \code{"_"}.}

\item{...}{additional arguments passed to \code{regexec} (e.g., \code{perl = TRUE})}
}
\value{
a character vector containing the captured groups
}
\description{
helper function to extract capturing groups from a string
}
\details{
This function uses the \code{regexec} and \code{regmatches} functions to extract
the capturing groups from the strings. The function returns a character vector
containing the captured groups. If no matches are found, \code{NA} is returned.
}
\keyword{internal}
