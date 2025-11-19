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
    # year
    substr(x, 1L, 4L),
    # month
    substr(x, 5L, 6L),
    # day
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
  data.table::fifelse(is_female(x), 2L, 1L)
}

#' Checks if the Swedish social security number is in the expected stratum
#' format "XXXXXXXX-XXXX"
#'
#' @param pnr Swedish social security number
#' @return Boolean indicating correct (TRUE) or incorrect (FALSE) format
#' @export valid_pnr_format
valid_pnr_format <- function(pnr) {

  pattern <- "^[0-9]{8}-[0-9]{4}$"

  return(
    grepl(pattern, pnr)
  )
}

#' Checks if the Swedish social security number is correctly formatted and
#' has the correct control number.
#'
#' @param pnr Swedish social security number
#' @param handle_invalid How to process social security numbers with invalid
#' formatting. If TRUE returns FALSE, if FALSE returns NA
#' @return Boolean indicating correct (TRUE) or incorrect (FALSE) social
#' security number
#' @export valid_pnr
valid_pnr <- function(pnr, handle_invalid = TRUE) {
  is_valid_format <- vapply(pnr, valid_pnr_format, logical(1))

  if (handle_invalid) {
    output <- rep(FALSE, length(pnr))
  } else {
    output <- rep(NA, length(pnr))
  }

  if (!any(is_valid_format)) {
    return(output)
  }

  good_pnr <- pnr[is_valid_format]

  pnr_digits <- stringr::str_remove(good_pnr, "-") |>
    stringr::str_split("")

  compute_check_digit <- function(digits) {
    nums <- as.integer(digits[3:11])

    multiplier <- rep(c(2, 1), length.out = length(nums))

    vals <- nums * multiplier

    vals <- ifelse(vals > 9, vals %/% 10 + vals %% 10, vals)

    check <- (10 - sum(vals) %% 10)
    if (check == 10) check <- 0

    check
  }

  check_digits <- vapply(pnr_digits, compute_check_digit, numeric(1))

  last_digits <- substring(good_pnr, 13, 13)

  output[is_valid_format] <- check_digits == as.integer(last_digits)

  output
}


#' Gets random password from API
#'
#' @name random_password
#' @title Random password
#'
#' @param char_types Possible character types to use in password as vector.
#' Available char_types:
#'      Syntax:
#'        L - upper case letter;
#'        l (small L) - lower case letter;
#'        R - random case letter;
#'        N - number (0-9);
#'        # - symbol (!@#$.+);
#'        ! - all symbols;
#'        C - upper case consonant letter (english consonants);
#'        c - lower case consonant letter (english consonants);
#'        V - upper case vowel letter (english vowels);
#'        v - lower case vowel letter (english vowels);
#' @param password_length integer, number of characters in password
#'
#' @export
#' @examples \dontrun{
#' rand_pass <- random_password(c("R", "N"), 10)
#' }
random_password <- function(
    char_types = c("R", "N", "!"),
    password_length = 15) {

  checkmate::assert_subset(
    char_types,
    c("L", "l", "R", "N", "!", "#", "C", "c", "V", "v"),
    empty.ok = FALSE
  )

  checkmate::assert_count(password_length)

  # List of the types of tokens available in the password
  tokens <- list(
    "L" = LETTERS,
    "l" = letters,
    "R" = c(LETTERS, letters),
    "N" = 0:9,
    "!" = c("!", "@", "#", "$", ".", "+", "%", "&", "/", "(", ")", "?"),
    "#" = c("!", "@", "#", "$", ".", "+"),
    "C" = c(
      "B", "C", "D", "F", "G", "H", "J", "K", "L", "M", "N", "P", "Q", "R", "S",
      "T", "V", "W", "X", "Y", "Z"
    ),
    "c" = c(
      "b", "c", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "q", "r", "s",
      "t", "v", "w", "x", "y", "z"
    ),
    "V" = c("A", "E", "I", "O", "U"),
    "v" = c("a", "e", "i", "o", "u")
  )

  # Sample the available tokens with replecment password_length-times.
  password <- paste0(
    sample(
      unique(
        unlist(
          tokens[char_types]
        )
      ), # The chosen subset of tokens
      password_length,
      replace = TRUE
    ),
    collapse = ""
  )

  return(password)
}

#' zips chosen directory with password protection. Zip-file and
#' txt-file with the password are placed in working directory
#'
#' @name zip_dir_with_pass
#' @title Zip directory with password
#'
#' @param directory directory to zip. Defaults to `output`
#' @param pass password to use. Defaults to `[random_password()]`. May only
#' contain `a–z`, `A–Z`, `0–9`, and `!`, `@`, `#`, `$`, `.`, `+`, `%`, `&`, `/`,
#' `(`, `)`, `?`.
#' @param file name of zip-file
#' @param overwrite Overwrite zip-file if it exists
#' @param sink_password whether or not to save the password in a file called
#' password.txt in the current working directory
#'
#' @export
#' @examples \dontrun{
#' zip_dir_with_pass()
#' }
zip_dir_with_pass <- function(
    directory = "output",
    pass = random_password(),
    file = "output",
    overwrite = TRUE,
    sink_password = TRUE) {

  checkmate::assert_string(pass, pattern = "^[a-zA-Z0-9!@#$.+%&/()?]+$")

  if (sink_password) {
    sink("password.txt")
    cat(pass)
    sink()
  }


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
#' @param dfs A list of data.frames to export
#' @param file_names A list of the names to use when saving each data.frame in
#'  `dfs` without file extension
#' @param separator The separator to use when saving the `dts`
#' @param zip Whether or not to zip the output directory after saving the files
#' @param output_dir The output directory in which to save the files
#' @param zip_file_name The name of the created zip file without file extension
#' @export sos_metadata
#'
#' @examples
#' \dontrun{
#' data_1 <- RCDBT::GetRegisterData(...) %>% filter(...)
#' data_2 <- RCDBT::GetRegisterData(...) %>% filter(...)
#' sos_metadata(
#'   dfs = list(data_1, data_2),
#'   file_names = list("dnr_xxxx_yyyyy_1", "dnr_xxxx_yyyyy_2")
#' )
#' }
sos_metadata <- function(dfs = list(),
                         file_names = list(),
                         separator = ",",
                         zip = TRUE,
                         output_dir = "Output",
                         zip_file_name = "output") {
  encoding <- "UTF-8"

  if (length(file_names) != length(dfs)) {
    stop("file_names and dfs is not the same length")
  }

  if (!("list" %in% class(dfs)) || !("list" %in% class(file_names))) {
    stop("Both file_names and dfs must be a list")
  }

  if (!all(stringi::stri_enc_isascii(unlist(file_names)))) {
    stop("file_names must only contain valid characters")
  }

  for (i in seq_along(file_names)) {
    df <- dfs[[i]] |>
      dplyr::mutate(dplyr::across(
        tidyselect::where(is.character),
        ~ enc2utf8(.x)
      ))

    data.table::fwrite(
      x = df,
      file = paste0("./", output_dir, "/", file_names[[i]], ".csv"),
      eol = "\r\n",
      row.names = FALSE,
      col.names = TRUE,
      sep = separator
    )
  }

  metadata <- data.frame(
    filename = paste0(unlist(file_names), ".csv"),
    encoding = encoding,
    separator = separator,
    end_of_line = "CRLF",
    nrows = purrr::map_int(dfs, nrow),
    ncols = purrr::map_int(dfs, ncol)
  )

  var_names <- lapply(seq_along(file_names), function(i) {
    tibble::tibble(
      filename = paste0(file_names[[i]], ".csv"),
      variable = names(dfs[[i]]),
      position_in_file = seq_along(dfs[[i]]),
      type = purrr::map_chr(dfs[[i]], \(x) utils::tail(class(x), 1)),
      length = purrr::map_int(
        dfs[[i]],
        function(x) {
          if (inherits(x, "logical")) {
            return(1)
          } else if (all(is.na(x))) {
            return(1)
          } else if (inherits(x, "Date")) {
            return(stringr::str_length(x[1]))
          } else if (inherits(x, "POSIXct")) {
            return(stringr::str_length(x[1]))
          } else {
            return(max(stringr::str_length(x), na.rm = TRUE))
          }
        }
      )
    )
  })

  var_names <- dplyr::bind_rows(var_names)

  writexl::write_xlsx(
    x = list(dataset = metadata, variabler = var_names),
    path = paste0("./", output_dir, "/metadata.xlsx")
  )

  if (zip) {
    zip_dir_with_pass(directory = output_dir, file = zip_file_name)

    if (!file.exists(paste0("./", zip_file_name, ".zip"))) {
      message("Zip failed, try (re-)installing RTools")
    }
  }
}

#' Pseudonymizes a data.frame
#'
#' @param df the data.frame to pseudonymize
#' @param pseudonimzed_var the name of the variable to store the key in
#' @param subject_key the name of the variable containing the subject identifier
#' @param remove_subject_key whether to remove the variable after adding the keys
#' @param save_key whether to save the key as an Excel-file
#' @param key_file_dir the directory in which to save the key
#'
#' @return the data.frame passed to `df` with a pseudonymized variable added
#' @export
pseudonymize_data <- function(df,
                              pseudonimzed_var = "lopnr",
                              subject_key = "SubjectKey",
                              remove_subject_key = TRUE,
                              save_key = TRUE,
                              key_file_dir) {

  checkmate::assert_data_frame(df)
  checkmate::assert_subset(subject_key, names(df))
  checkmate::assert_character(pseudonimzed_var, len = 1)
  checkmate::assert_logical(remove_subject_key, len = 1)
  checkmate::assert_logical(save_key, len = 1)

  if (save_key) {
    checkmate::assert(
      checkmate::check_character(key_file_dir),
      checkmate::check_directory_exists(key_file_dir),
      combine = "and"
    )
  }

  keys <- df |>
    dplyr::distinct(.data[[subject_key]]) |>
    dplyr::mutate(!!pseudonimzed_var := seq_len(dplyr::n()))

  if (save_key) {
    if (!grepl("/$", key_file_dir)) {
      key_file_dir <- paste0(key_file_dir, "/")
    }
    writexl::write_xlsx(keys, sprintf("%s%s.xlsx", key_file_dir, pseudonimzed_var))
  }

  df <- dplyr::left_join(df, keys, by = subject_key)
  if (remove_subject_key) {
    df <- dplyr::select(df, -tidyselect::all_of(subject_key))
  }
  return(df)
}
