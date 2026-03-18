#' @section Package options:
#' - `rcstat.local_plumber`: logical, default `FALSE`.
#'   Controls whether [api_statistics()] should send API requests to a local
#'   plumber instance.
#'   Can be set globally with `options(rcstat.local_plumber = TRUE)`.
#' - `rcstat.local_scope`: integer, default 1.
#'    Controls the scope of the API call.
#' - `rcstat.local_unit`: integer, default 0.
#'    Controls what unit you are authenticated for.
#' - `rcstat.local_role`: integer, default 1.
#'    Controls the role you are authenticates as.
#' - `rcstat.local_register`: integer, default 1.
#'    Controls the register you are authenticated for.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom data.table .BY
#' @importFrom data.table .EACHI
#' @importFrom data.table .GRP
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table .NGRP
#' @importFrom data.table .SD
#' @importFrom data.table data.table
#' @importFrom Rcpp sourceCpp
#' @importFrom rlang %||%
#' @importFrom rlang .env
#' @useDynLib RCStat, .registration = TRUE
## usethis namespace: end
NULL

#' @noRd
dummy <- function() {
  readxl::read_excel
}
