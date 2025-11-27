#' Gets the estimated survival at a specific point in time
#'
#' @param df data.frame
#' @param time point in time (in days)
#' @param time_col the name of the time column
#' @param event_col the name of the event column
#' @param group_cols the columns to group by
#' @param marginal_cols the columns to add marginal groups for
#'
#' @return estimated survival at the requested point in time for each group
#' @export
get_surv_value <- function(df,
                           time,
                           time_col = "time",
                           event_col = "status",
                           group_cols = NULL,
                           marginal_cols) {

  checkmate::assert_data_frame(df)
  checkmate::assert_number(time, lower = 0)
  checkmate::assert_string(time_col)
  checkmate::assert_string(event_col)
  checkmate::assert_character(group_cols, null.ok = TRUE)
  checkmate::assert_names(
    names(df),
    must.include = c(time_col, event_col, group_cols)
  )

  if (rlang::is_missing(marginal_cols)) {
    marginal_cols <- group_cols
  } else {
    checkmate::assert_subset(marginal_cols, group_cols)
  }

  rhs <- ifelse(
    length(group_cols) > 0,
    paste(group_cols, collapse = " + "),
    "1"
  )

  fml <- stats::as.formula(
    paste0("survival::Surv(", time_col, ", ", event_col, ") ~ ", rhs)
  )

  df <- df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(marginal_cols),
        as.character
      )
    )

  marginal_name <- function(x) {
    if (stringr::str_detect(tolower(x), "(county|unit(code|id)?)$")) {
      "Riket"
    } else {
      "Alla"
    }
  }

  df <- purrr::reduce(
    marginal_cols,
    .init = df,
    .f = function(df, x) {
      dplyr::bind_rows(
        df,
        dplyr::mutate(df, !!x := marginal_name(x))
      )
    }
  )

  fit <- survival::survfit(fml, data = df)

  res <- broom::tidy(fit) |>
    dplyr::filter(.data$time <= .env$time) |>
    dplyr::group_by(dplyr::across(dplyr::any_of("strata"))) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup()

  if (!is.null(group_cols)) {
    res <- res |>
      tidyr::separate_wider_delim("strata", delim = ",", names_sep = "_") |>
      dplyr::mutate(
        dplyr::across(
          dplyr::matches("^strata_\\d+"),
          ~ stringr::str_extract(.x, "^.+=(.+)$", 1),
          .names = "
          {group_cols[as.numeric(stringr::str_extract(.col, '\\\\d+'))]}
          "
        ),
        dplyr::across(
          dplyr::all_of(group_cols),
          ~ convert(
            .x,
            class(df[[dplyr::cur_column()]])
          )
        )
      )
  }

  res |>
    dplyr::select(dplyr::all_of(group_cols), "estimate")
}

#' Convert an object to a specified class
#'
#' @param x object to convert
#' @param cls class to convert to
#'
#' @return x converted to specified class
convert <- function(x, cls) {
  if (!rlang::is_atomic(x)) {
    cli::cli_abort("{.arg x} has to be atomic")
  }
  if (cls == "numeric") {
    as.numeric(x)
  } else if (cls == "logical") {
    as.logical(x)
  } else if (cls == "integer") {
    as.integer(x)
  } else if (cls == "character") {
    as.character(x)
  } else if (cls == "factor") {
    as.factor(x)
  } else {
    cli::cli_alert_warning(
      "{.arg cls} was not matched, returning {.arg x} unmodified"
    )
    x
  }
}
