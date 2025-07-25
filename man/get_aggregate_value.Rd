% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbl_statistics.R
\name{get_aggregate_value}
\alias{get_aggregate_value}
\title{Create aggregate data across all combinations, including totals.
Can specify if proportion or average should be used.}
\usage{
get_aggregate_value(
  df,
  group_cols = NULL,
  vars = NULL,
  include_missing = TRUE,
  obfuscate_data = FALSE,
  censored_value = 0,
  pivot_prop_count = FALSE
)
}
\arguments{
\item{df}{A data frame.}

\item{group_cols}{The columns to group by.}

\item{vars}{Variables to be used to calculate the proportion from.}

\item{include_missing}{If missing values should be included in the total}

\item{obfuscate_data}{If data should be obfuscated}

\item{censored_value}{What value to replace censored values, used as argument in obfuscate_data}

\item{pivot_prop_count}{whether to pivot the resulting data.frame into long format}
}
\description{
Create aggregate data across all combinations, including totals.
Can specify if proportion or average should be used.
}
