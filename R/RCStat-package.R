#' @section Package options:
#' - `rcstat.local_plumber`: logical, default `FALSE`.
#'   Controls whether [api_statistics()] should send API requests to a local
#'   plumber instance.
#'   Can be set globally with `options(rcstat.local_plumber = TRUE)`.
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
