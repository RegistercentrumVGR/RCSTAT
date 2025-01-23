#' Get current user info
#' @export
get_user_info <- function() {
  scope_register <- .GlobalEnv[["Scope.Register"]]
  scope_role <- .GlobalEnv[["Scope.Role"]]
  scope_user <- .GlobalEnv[["Scope.User"]]
  scope_unit <- .GlobalEnv[["Scope.Unit"]]

  list(
    "register_id" = scope_register,
    "user_id" = scope_user,
    "unit_id" = scope_unit,
    "role_id" = scope_role
  )
}

#' Extract r function call from RC-statisticts API-URL
#'
#' @param api_url The URL of a function call to a RC-statistics endpoint.
#'
#' @return a character vector of a r function call to a statistics API-endpoint
#'
#'@examples
#' extract_function_call(
#' api_url = "https://sep.registercentrum.se/stratum/api/statistics/sep/sep-jumbo-antalreg?apikey=bK3H9bwaG4o="
#' )
#' @export extract_function_call
extract_function_call <- function(api_url) {

  api_url <- stringr::str_split_1(
    string = stringr::str_split_1(
      api_url,
      pattern = "statistics/"
    )[2],
    pattern = "/"
  )[2]

  function_name <- stringr::str_split_i(
    api_url,
    pattern = "\\?",
    1
  )

  function_arguments <-
    stringr::str_split(
      stringr::str_split_i(
        api_url,
        pattern = "\\?",
        2
      ),
      pattern = "&"
    )[[1]]

  function_arguments <- function_arguments[-c(grep("apikey", function_arguments))]

  function_call <- paste0(function_name, "(")

  for (argument in function_arguments) {
    argument <- gsub("=", "='", argument)
    function_call <- paste0(function_call, argument, "', ")
  }
  function_call <- paste0(function_call, ")")
  function_call <- gsub(", )", ")", function_call)

  return(function_call)

}
