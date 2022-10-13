#' Add row with total
#'
#' @param df data.frame or tibble
#'
#' @return df with additional row with all numeric
#' columns summed up.
#'
#' @export add_total
add_total <- function(df){

  cols <- unlist(lapply(df, is.numeric))
  totals <- lapply(df[cols], sum, na.rm = TRUE)
  totals[names(cols[!cols])] <- NA

  rbind(df, totals)

}
#' Time between dates
#'
#' Calculates time between two dates.
#' @param from_date start date of interval
#' @param to_date end date for interval
#' @param unit Unit to use, passed to lubridate::time_length
#' (days, weeks, months, years)
#'
#' @export birthdate
age <- function(from_date, to_date, unit = "years"){
  time_interval <- lubridate::interval(from_date, to_date)
  #time_intervall <- difftime(to_date, from_date, unit = "days")
  lubridate::time_length(time_interval, unit = unit)
}
#' Derives birthdate from Swedish social security number
#'
#' @param x Swedish social security number
#'
#' @export birthdate
birthdate <- function(x){
  as.Date(paste(
    #year
    substr(x, 1L, 4L),
    #month
    substr(x, 5L, 6L),
    #day
    substr(x, 7L, 8L),
    sep = "-"
  ))
}
#' Derives gender from Swedish social security number
#'
#' @param x Swedish social security number
#' @return boolean
#' @export is_female
is_female <- function(x){
  (as.integer(substr(x, 12L, 12L)) %% 2L == 0L)
}
#' Derives gender from Swedish social security number
#'
#' @param x Swedish social security number
#' @return boolean
#' @export is_male
is_male <- function(x){
  !is_female(x)
}
#' Derives gender from Swedish social security number
#'
#' @param x Swedish social security number
#' @return 1 if male, 2 if female
#' @export gender
gender <- function(x){
  if(is_female){
    return(2L)
  }else{
    return(1L)
  }
}
