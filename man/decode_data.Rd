% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_labels.R
\name{decode_data}
\alias{decode_data}
\title{Replace each value with its corresponding label}
\usage{
decode_data(
  data,
  labels = NULL,
  droplevels = TRUE,
  add_cols = FALSE,
  suffix = "_label",
  as_character = FALSE,
  missing_labels_na = TRUE
)
}
\arguments{
\item{data}{Data frame with Stratum register data}

\item{labels}{Value labels for the same register}

\item{droplevels}{If `TRUE`, factor levels not present in data will be
excluded. If `FALSE` no levels are dropped. If character
then the specified variables will have its levels dropped.}

\item{add_cols}{Add additional columns with the labels}

\item{suffix}{suffix to add to added columns}

\item{as_character}{If `TRUE` variables
will be set to characters. If `FALSE` variables will be factors.}

\item{missing_labels_na}{If `TRUE` values with no label will be set to `NA`}
}
\value{
data.frame with character values instead
        of numerical values.
}
\description{
Returns data with descriptive data points
instead of codes.
}
\examples{
\dontrun{
factor_data <- decode_vars(data, factor_levels)
}

}
