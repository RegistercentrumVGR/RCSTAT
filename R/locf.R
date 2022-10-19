

#' Performs memory efficient locf
#'
#' @param df data.frame or tibble with data
#' @param vars variables to perform locf on
#' @param group_by variables to group on
#' @param order_by variable to sort each group on
#' @param slice keep only last row in each group
#'
#' @return data.frame or tibble with locf data
#'
#' @export locf
#'
locf <- function(
    df,
    vars,
    orderby = "date",
    groupby = "id",
    slice = FALSE
){
  data.table::setorderv(df, cols = c(groupby, orderby))

  # Get index vector which indicates each
  # row for which we have a new groupby ID
  # i.e. "first" in sorting
  idchg <- !duplicated(df[,groupby])
  row_numbers <- seq_along(idchg)
  #index_vector <- as.integer((grp_change | id_change) | !is.na(x))*seq_along(x)
  for(var in vars){
    index_vector <- as.integer(idchg | !is.na(df[[var]]))*row_numbers

    df[[var]] <- df[[var]][cummax(index_vector)]
  }
  if(slice){
    data.table::setorderv(df, cols = c(groupby, orderby),
              order = c(rep(1, length(groupby)), -1))
    df <- df[idchg,]
  }
  #row.names(df) <- 1:nrow(df)
  return(df)
}

#' DT Last Observation Carried Forward with
#'
#' Returns data.frame with LOCF imputation using
#' data.table. Faster than `RCStat::locf` for large
#' datasets, especially when imputing on multiple variables
#'
#' @name locfdt
#' @title locfdt
#' @param df data.frame to do LOCF imputation on
#' @param vars variables to impute with LOCF
#' @param orderby Variable to sort on
#' @param groupby Variable to group on
#' @param slice If `TRUE` will select the last row in each group of `groupby`
#' @return data.frame with LOCF imputation
#' @export locfdt
#' @examples
#' \dontrun{df <- locfdt(df, vars)}
#'
locfdt <- function(
    df,
    vars,
    groupby = "id",
    orderby = "date",
    slice = FALSE
){



  dt <- data.table::data.table(df)

  # Sort data
  data.table::setorderv(dt, c(groupby, orderby))

  # Get group-change indicator vector
  # This is TRUE for the first row in
  # each group.
  idchg <- !duplicated(dt[, groupby, with = FALSE])
  # locf all vars with parallel::mcapply
  dt <- dt[
    ,(vars) := parallel::mclapply(
      .SD, function(x) x[cummax(as.integer(!is.na(x)|idchg)* .I)]
    ),
    .SDcols = vars
  ]
  if(slice){
    data.table::setorderv(
      dt,
      cols = c(groupby, orderby),
      order = c(rep(1, length(groupby)), -1)
    )
    dt <- dt[idchg,]
  }
  df <- tibble::as_tibble(dt)

  attributes(df)$.internal.selfref <- NULL

  return(df)
}

