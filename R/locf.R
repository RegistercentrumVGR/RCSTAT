#' Performs memory efficient locf
#'
#' @param df data.frame or tibble with data
#' @param vars variables to perform locf on
#' @param groupby variables to group on
#' @param orderby variable to sort each group on
#' @param slice keep only last row in each group
#'
#' @return data.frame or tibble with locf data
#'
#' @export locf
#'
locf <- function(
    df,
    vars,
    groupby = "id",
    orderby = "date",
    slice = FALSE
) {
  # TODO: Assert that groupby and order by
  # are character vectors and names which
  # are present in df.

  # create order(groupby, orderby) expression
  sort_order <- paste0(
    "order(",
    paste(paste0("df$", groupby), collapse = ", "),
    ", df$", orderby, ")"
  )
  df <- df[eval(parse(text = sort_order)), ]

  # Get index vector which indicates each
  # row for which we have a new groupby ID
  # i.e. "first" in sorting
  first_row_of_each_group <- !duplicated(subset(df, select = groupby))
  row_numbers <- seq_along(first_row_of_each_group)

  df[vars] <- lapply(
    subset(df, select = vars),
    function(x) {
      x[cummax(
      as.integer(first_row_of_each_group | !is.na(x)) * row_numbers)]
    }
  )

  if (slice) {
    last_row_of_each_group <- !duplicated(df[, groupby], fromLast = TRUE)
    df <- df[last_row_of_each_group, ]
  }

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
#' @param dt data.frame or data.table to do LOCF imputation on
#' @param vars variables to impute with LOCF
#' @param orderby Variable to sort on
#' @param groupby Variable to group on
#' @param slice If `TRUE` will select the last row in each group of `groupby`
#' @param return_tibble whether to return a data.table or a tibble
#' @return data.frame with LOCF imputation
#' @export locfdt
#' @examples
#' \dontrun{df <- locfdt(df, vars)}
#'
locfdt <- function(
    dt,
    vars,
    groupby = "id",
    orderby = "date",
    slice = FALSE,
    return_tibble = FALSE
) {

  # Sort data
  data.table::setorderv(dt, c(groupby, orderby))

  # Get group-change indicator vector
  # This is TRUE for the first row in each group.
  idchg <- !duplicated(subset(dt, select = groupby))

  if (requireNamespace("parallel", quietly = TRUE)) {
    # locf all vars with parallel::mclapply
    dt[
      ,
      (vars) := parallel::mclapply(
        .SD, function(x) x[cummax(as.integer(!is.na(x) | idchg) * .I)]
      ),
      .SDcols = vars
    ]
  } else {
    # locf with lapply
    dt[
      ,
      (vars) := lapply(
        .SD, function(x) x[cummax(as.integer(!is.na(x) | idchg) * .I)]
      ),
      .SDcols = vars
    ]
  }
  # Keep only last observation in each
  # group if slice is TRUE
  if (slice) {
    last_row <- !duplicated(
      subset(dt, select = groupby),
      fromLast = TRUE
    )
    # Take last row of each group
    dt <- dt[last_row, ]
  }

  if (return_tibble) {
    dt <- tibble::as_tibble(dt)
    attributes(dt)$.internal.selfref <- NULL
  }

  return(dt)
}
