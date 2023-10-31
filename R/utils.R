

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
#' Calculates time between two dates. unit whole_years will
#' return age in years. Other units will returns numeric (decimal).
#'
#' @param from_date start date of interval
#' @param to_date end date for interval
#' @param unit Unit to use, (whole_years, days, weeks, months, years)
#'
#' @export age
age <- function(from_date, to_date, unit = "whole_years") {
  if (unit == "whole_years") {
    age <- lubridate::year(to_date) - lubridate::year(from_date) - 1
    birthday_month <- lubridate::month(from_date)
    date_month <- lubridate::month(to_date)
    age <- ifelse(
      # Add a year if current years birthday is before to_dat
      (birthday_month < date_month |
         (birthday_month == date_month &
            lubridate::day(from_date) <= lubridate::day(to_date))),
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
  second_last_character <- nchar(x) - 1

  (as.integer(substr(x, second_last_character, second_last_character)) %% 2L == 0L)
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
    password_length = 15) {

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
    overwrite = TRUE) {

  sink("password.txt")
  # TODO: add additional info to password.txt
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



#' Saves data.frames as .csv files and creates and saves the corresponding
#' metadata files. Can also zip the outputs with a random password which is
#' saved in a separate .txt file.
#'
#' @param dts A list of data.frames to export
#' @param file_names A list of the names to use when saving each data.frame in
#'  `dts`
#' @param encoding The encoding to use, by default `UTF-8`
#' @param separator The separator to use when saving the `dts`
#' @param zip Whether or not to zip the output directory after saving the files
#' @param output_dir The output directory in which to save the files
#' @param zip_file_name The name of the created zip file
#' @export sos_metadata
#'
#' @examples
#' \dontrun{
#' data_1 <- RCDBT::GetRegisterData(...) %>% filter(...)
#' data_2 <- RCDBT::GetRegisterData(...) %>% filter(...)
#' sos_metadata(dts = list(data_1, data_2),
#'              file_names = list("dnr_xxxx_yyyyy_1", "dnr_xxxx_yyyyy_2"))
#' }
sos_metadata <- function(dts = list(),
                         file_names = list(),
                         encoding = "UTF-8",
                         separator = ",",
                         zip = T,
                         output_dir = "Output",
                         zip_file_name = "output") {

  if (length(file_names) != length(dts))
    stop("file_names and dts is not the same length")

  if (!("list" %in% class(dts)) | !("list" %in% class(file_names)))
    stop("Both file_names and dts must be a list")



  for (i in seq_along(file_names)) {
    utils::write.table(x = dts[[i]],
                file = paste0("./", output_dir, "/", file_names[[i]], ".csv"),
                sep = separator,
                eol = "\r\n",
                fileEncoding = encoding,
                row.names = F,
                col.names = T)
  }

  metadata <- data.frame(
    filename = paste0(unlist(file_names), ".csv"),
    encoding = encoding,
    separator = separator,
    end_of_line = "CRLF",
    nrows = purrr::map_int(dts, nrow),
    ncols = purrr::map_int(dts, ncol)
  )

  var_names <- lapply(1:length(file_names), function(i) {
    tibble::tibble(filename = paste0(file_names[[i]], ".csv"),
                   variable = names(dts[[i]]),
                   position_in_file = seq_along(dts[[i]]),
                   type = purrr::map_chr(dts[[i]], \(x) utils::tail(class(x), 1)),
                   length =
                     dplyr::if_else(
                       # Ovan väljer vi att exportera logicals som 0/1 även om det är
                       # TRUE/FALSE i dt_out
                       type == "logical", as.integer(1),
                       purrr::map_int(
                         dts[[i]],
                         function(x) {
                           str_len <- stringr::str_length(x)
                           if (all(is.na(str_len))) {
                             return(1)
                           } else {
                             return(max(str_len, na.rm = T))
                           }
                         }
                       )
                     )
    )
  })

  var_names <- dplyr::bind_rows(var_names)

  writexl::write_xlsx(
    x = list(dataset = metadata, variabler = var_names),
    path = paste0("./", output_dir, "/metadata.xlsx")
  )

  if (zip)
    zip_dir_with_pass(directory = output_dir, file = zip_file_name)
}
