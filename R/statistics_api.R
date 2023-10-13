#' Get data from statistics API
#'
#' @param register register short name
#' @param script script name
#' @param arguments Arguments to include as named list
#' @param api_key api-key to use
#' @param dev if newr should be used
#' @param verbose print query sent
#'
#' @export statistics_api
statistics_api <- function(
    register,
    script,
    arguments = NULL,
    api_key = "MpuYxfbtp5I=",
    dev = FALSE,
    verbose = FALSE) {

  if (!is.null(arguments)) {
    # Remove NULL elements from list
    arguments[sapply(arguments, is.null)] <- NULL

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
    arguments_str <- NULL
  }

  q <-
    paste0(
      "https://stratum.registercentrum.se/",
      "stratum/api/statistics/",
      register, "/",
      script,
      "?apikey=", api_key,
      arguments_str
    )
  if (dev) q <- paste0(q, "&forcenewr=true")
  if (verbose) print(q)

  json_list <-
    q |>
    httr::GET(
      encoding = "UTF-8"
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

  data <- json_list[["data"]]

  as.data.frame(data, stringsAsFactors = FALSE)
}
