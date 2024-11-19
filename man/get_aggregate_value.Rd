\name{get_aggregate_value}
\alias{get_aggregate_value}

\title{
Create aggregated data
}
\description{
A function that creates an aggregated data frame.
}
\usage{
get_aggregate_value(
    df,
    group_cols = NULL,
    vars = NULL,
    include_missing = TRUE,
    obfuscate_data = FALSE,
    censored_value = 0
)
}

\arguments{
\item{df}{A data frame}
\item{group_cols}{The columns that are used to group the dataframe}
\item{vars}{A list containing the variables that are used for the statistics}
\item{include_missing}{Wheter missing values are to be included in total}
\item{obfuscate_data}{Wheter the aggregated data should be obfuscated. Uses RCStat::obfuscate_data}
\item{censored_value}{What censored values to be replaced with}
}




