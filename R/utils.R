#' Add row with total
#'
#' Brutally sums up all numeric columns and appends
#' to data.
#'
#' @param data data.frame or tibble
#'
#' @return df with additional row with all numeric
#' columns summed up.
#'
#' @export add_total
add_total <- function(data) {

  cols <- unlist(lapply(data, is.numeric))
  totals <- lapply(data[cols], sum, na.rm = TRUE)
  totals[names(cols[!cols])] <- NA

  rbind(data, totals)
}
#' Simple time between dates
#'
#' Calculates time between two dates.
#'
#' @param from_date start date of interval
#' @param to_date end date for interval
#' @param unit Unit to use, (whole_years, days, weeks, months, years)
#'
#' @export age
age <- function(from_date, to_date, unit = "whole_years") {
  if(unit == "whole_years"){
    age <- lubridate::year(to_date) - lubridate::year(from_date) - 1
    birthday_month <- lubridate::month(from_date)
    date_month <- lubridate::month(to_date)
    age <- ifelse(
      # Add a year if current years birthday is before to_dat
      (birthday_month < date_month |
         (birthday_month == date_month &
            lubridate::day(from_date) <= lubridate::day(to_date)) ),
      age + 1,
      age
    )
  } else {
    time_interval <- lubridate::interval(from_date, to_date)
    age <- lubridate::time_length(time_interval, unit = unit)
  }
  age
}
#' Derives birthdate from Swedish social security number
#'
#' @param x Swedish social security number
#'
#' @export birthdate
birthdate <- function(x) {
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
is_female <- function(x) {
  (as.integer(substr(x, 12L, 12L)) %% 2L == 0L)
}
#' Derives gender from Swedish social security number
#'
#' @param x Swedish social security number
#' @return boolean
#' @export is_male
is_male <- function(x) {
  !is_female(x)
}
#' Derives gender from Swedish social security number
#'
#' @param x Swedish social security number
#' @return 1 if male, 2 if female.
#' @export gender
gender <- function(x) {
  if (is_female) {
    return(2L)
  }else {
    return(1L)
  }
}

#' Gets random password from API
#'
#' @name random_password
#' @title Random password
#'
#' @param char_types Possible character types to use in password as vector.
#' Documentation on API:
#' https://www.passwordrandom.com/pronounceable-password-generator
#'      Syntax:
#'        L - upper case letter; l (small L) - lower case letter;
#'        R or r - random case letter;
#'        N or n - number (0-9);
#'        # - symbol (!@#$.+);
#'        ! - all symbols;
#'        C - upper case consonant letter;
#'        c - lower case consonant letter;
#'        V - upper case vowel letter;
#'        v - lower case vowel letter;
#' @param password_length integer, number of characters in password
#'
#' @export
#' @examples \dontrun{rand_pass <- random_password(c("R", "N"), 10)}
random_password <- function(
    char_types = c("R", "N", "!"),
    password_length = 15){

  # Generera slumpmässig struktur för lösenord
  schema_pass <- paste0(sample(char_types, size = password_length,
                               replace = TRUE), collapse = "")

  # Skapa API-anrop
  query <-
    paste0(
      "https://www.passwordrandom.com/query?command=password&scheme=",
      schema_pass
    )

  # Hämta från API
  random_password <-
    httr::content(
      httr::GET(
        query,
        encoding = "UTF-8"
      ),
      as = "text",
      encoding = "UTF-8"
    )

  return(random_password)
}

#' zips chosen directory with password protection. Zip-file and
#' txt-file with the password are placed in working directory
#'
#' @name zip_dir_with_pass
#' @title Zip directory with password
#'
#' @param directory directory to zip. Defaults to `output`
#' @param pass password to use. Defaults to `random_password()`
#' @param file name of zip-file
#' @param overwrite Overwrite zip-file if it exists
#'
#' @export
#' @examples \dontrun{zip_dir_with_pass()}
zip_dir_with_pass <- function(
    directory = "output",
    pass = random_password(),
    # TODO: get DNR from Environment variable instead?
    file = "output",
    overwrite = TRUE
){

  sink("password.txt")
  # TODO: add this additional info to password.txt
  # if(exists(sos_dnr)){
  #   cat("Skickas till registerservice@socialstyrelsen.se, ange sos_dnr")
  # }
  cat(pass)
  sink()

  filenames <- dir(directory, full.names = TRUE)

  if (overwrite && file.exists(paste0(file, ".zip"))) {
    file.remove(paste0(file, ".zip"))
  }

  utils::zip(
    zipfile = paste0(file, ".zip"),
    files = filenames,
    flags = paste("--password", pass)
  )
}
