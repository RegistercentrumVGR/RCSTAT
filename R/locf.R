#' Performs memory efficient locf
#'
#' @param df data.frame or tibble with data
#' @param ... variables to group on
#' @param vars variables to perform locf on
#' @param order_by variable to sort each group on
#' @param slice keep only last row in each group
#'
#' @return data.frame or tibble with locf data
#'
#' @export locf
locf <- function(
    df,
    ...,
    vars,
    orderby = "date",
    slice = FALSE
){
  # Sort data by group and selected order
  df <- dplyr::arrange(df, ..., !!rlang::sym(orderby))

  # Make boolean vector for group change indikator
  idchg <- !duplicated(dplyr::select(df, ...))

  # locf all vars
  for(var in vars){
    index_vector <- as.integer(idchg | !is.na(df[[var]]))*seq_along(df[[var]])

    df[[var]] <- df[[var]][cummax(index_vector)]
  }
  # Select last observation in each group
  # if slice is TRUE
  if(slice){
    df <- dplyr::arrange(df, ..., -!!rlang::sym(orderby))
    df <- df[idchg,]
  }
  # rename rows in new order?
  # row.names(df) <- 1:nrow(df)
  return(df)
}
