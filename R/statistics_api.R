#' Get data from statistics API
#'
#' @param register register short name
#' @param script script name
#' @param arguments Arguments to include as named list
#'
#' @export statistics_api
statistics_api <- function(
    register,
    script,
    arguments = NULL
    ) {

  if(!is.null(arguments)){
    # Remove NULL elements from list
    arguments[sapply(arguments, is.null)] <- NULL

    arguments_str = paste0(
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
      "?apikey=MpuYxfbtp5I=",
      arguments_str
    )

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

  print(json_list)

  if (json_list[["code"]] == 400 & !is.null(json_list[["message"]])) {
    stop(json_list[["message"]])
  }

  if (json_list[["code"]] == 401) {
    stop("401: Athentication error, invalid credentials!")
  }
  if (json_list[["code"]] == 403) {
    stop("403: Athentication error, permission denied!")
  }
  if (json_list[["code"]] == 404) {
    stop("404: not found")
  }
  if (json_list[["code"]] == 405) {
    stop("405: Disallowed operation!")
  }
  if (json_list[["code"]] == 500) {
    stop("500: Non-existing functionality!")
  }

  data <- json_list[["data"]]

  as.data.frame(data, stringsAsFactors = FALSE)
}
