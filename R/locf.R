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
    slice = FALSE) {
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
      x[cummax(as.integer(first_row_of_each_group | !is.na(x)) * row_numbers)]
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
#' @param window_size The maximum time the last observation can be
#' carried forward. If `n_months` is `NULL` LOCF will be carried out
#' on all observations like normal.
#' @param window_type `years`, `months` or `days`, deafult is `days`.
#' With `months` and `years` the `window_size` is simply multiplied by a
#' factor of 31 or 365.25 respectively.
#'
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
    return_tibble = FALSE,
    window_size = NULL,
    window_type = NULL) {
  # Check inputs
  checkmate::assert_choice(
    x = window_type,
    choices = c("months", "years", "days"),
    null.ok = TRUE
  )
  if (!is.null(window_type)) {
    window_size <- switch(
      window_type,
      "months" = 31 * window_size,
      "years" = ceiling(365.25 * window_size),
      "days" = window_size
    )
  }
  checkmate::check_int(x = window_size, null.ok = TRUE)

  # Sort data
  data.table::setorderv(dt, c(groupby, orderby))

  # Get group-change indicator vector
  # This is TRUE for the first row in each group.
  idchg <- !duplicated(subset(dt, select = groupby))

  dt[
    ,
    (vars) := lapply(
      .SD, function(x) {
        i <- cummax(as.integer(!is.na(x) | idchg) * .I)

        if (!is.null(window_size) && !is.null(window_type)) {

          time_diff <- sapply(seq_along(i), function(j) {

            if (idchg[j] == 1) {
              return(TRUE)
            } else {
              return(
                dt[j, get(orderby)] - window_size > dt[i[j], get(orderby)]
              )
            }
          })

          i[time_diff] <- which(time_diff)

        }

        return(x[i])
      }
    ),
    .SDcols = vars
  ]

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
