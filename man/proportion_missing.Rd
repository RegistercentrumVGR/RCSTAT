% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbl_statistics.R
\name{proportion_missing}
\alias{proportion_missing}
\title{Calculates proportion of missing data}
\usage{
proportion_missing(data, group_by, vars = NULL, obfuscate = TRUE, ...)
}
\arguments{
\item{data}{data.frame or tibble}

\item{group_by}{grouping to apply before calculation}

\item{vars}{vars to calculate proportion of missing
data on. Defaults to all except grouping vars.}

\item{obfuscate}{If data should be non-revealing}

\item{...}{Arguments passed to obfuscate}
}
\description{
Calculates n missing and proportion missing across
columns and by groups specified.
}
