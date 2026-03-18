#' Get data from function call in `register`
#'
#' Data from either a installed `register` package for testing locally
#' or from the API when in active use
#' (controlled by global option `get_data_fun`)
#'
#' @param register character. Register short name.
#' @param fun character. function name from a register package
#' @param arguments list. additional arguments passed to `register::fun()`
#'
#' @return tibble
get_data <- function(register, fun, arguments = list()) {
  message(
    "Calling function: ", register, "::",
    fun, " using: ", getOption("get_data_fun")
  ) # for debug
  if (length(arguments) > 0) arguments[sapply(arguments, is.null)] <- NULL
  do.call(
    getOption("get_data_fun"),
    list(register = register, fun = fun, arguments = arguments)
  )
}


# Try with data from locally installed {register} package
get_data_package <- function(register, fun, arguments = list()) {
  do.call(eval(parse(text = paste0(register, "::", fun))), arguments)
}

#' @describeIn api_statistics gets the url for the API call
api_url <- function(register,
                    fun,
                    arguments = list(),
                    api_key = "MpuYxfbtp5I=",
                    dev = FALSE,
                    api_url = "prod") {

  if (length(arguments) > 0) {
    arguments_str <- paste0(
      # Add all arguments
      paste0(
        "&", names(arguments), "=",
        # collapse if multiple inputs to one argument
        unname(lapply(arguments, paste0, collapse = ",")),
        collapse = ""
      )
    )
  } else {
    arguments_str <- ""
  }

  if (local_plumber(api_url)) {
    prefix <- "http://127.0.0.1:8530/"
  } else if (api_url == "prod") {
    prefix <- "https://stratum.registercentrum.se/stratum/api/statistics/"
  } else {
    prefix <- api_url
  }

  q <- sprintf(
    "%s%s/%s?apikey=%s%s",
    prefix,
    tolower(register),
    fun,
    api_key,
    arguments_str
  )

  if (dev) q <- paste0(q, "&forcenewr=true")

  q

}

local_plumber <- function(api_url) {
  api_url == "local" || getOption("rcstat.local_plumber", FALSE)
}

#' Get data from statistics API
#'
#' If you are calling a local plumber instance we will set the necessary
#' headers. By default, scope is 1, unit is 0, role is 900, and register is 100.
#' You can control these values with the options `rcstat.local_scope`,
#' `rcstat.local_unit`, `rcstat.local_role`, `rcstat.local_register`.
#'
#' @param register register short name
#' @param fun script name
#' @param arguments Arguments to include as named list
#' @param api_key api-key to use
#' @param dev if newr should be used
#' @param verbose print query sent
#' @param return_tibble whether or not to coerce data to a tibble using
#' tibble::as_tibble. Note that this does not work when the data is a list.
#' @param api_url Should be `"prod"` for calling the production Stratum API or
#' `"local"` to call a local instance of `rcplumber`. All other values will
#' interpreted as the prefix for the URL of the API call. Can also be controlled
#' by the option `rcstat.local_plumber`. See details for information about how
#' to control headers.
#'
#' @export
api_statistics <- function(
    register,
    fun,
    arguments = list(),
    api_key = "MpuYxfbtp5I=",
    dev = FALSE,
    verbose = FALSE,
    return_tibble = TRUE,
    api_url = "prod") {

  checkmate::assert_string(api_url)

  q <- api_url(
    register = register,
    fun = fun,
    arguments = arguments,
    api_key = api_key,
    dev = dev,
    api_url = api_url
  )

  if (verbose) message(q)

  args <- list(encoding = "UTF-8", url = q)

  if (local_plumber(api_url)) {
    args <- c(
      args,
      list(
        httr::add_headers(
          "x-data-scope" = getOption("rcstat.local_scope", 1),
          "x-unit" = getOption("rcstat.local_unit", 0),
          "x-role" = getOption("rcstat.local_role", 900),
          "x-registerid" = getOption("rcstat.local_register", 100)
        )
      )
    )
  }

  json_list <- do.call(
    what = httr::GET,
    args = args
  ) |>
    httr::content(
      as = "text",
      encoding = "UTF-8"
    ) |>
    jsonlite::fromJSON()

  if (json_list[["code"]] != 200 && !is.null(json_list[["message"]])) {
    stop(paste0(
      "\nError returned by server",
      "\nCode: ", json_list[["code"]],
      "\nMessage: ", json_list[["message"]]
    ))
  }


  if (!local_plumber(api_url)) {
    data <- json_list[["data"]]
  } else {
    data <- json_list
  }

  if (return_tibble) {
    data <- tibble::as_tibble(data)
  }

  data
}
