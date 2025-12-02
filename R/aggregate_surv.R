#' Gets the estimated survival at a specific point in time
#'
#' @param df data.frame
#' @param time point in time (in days)
#' @param time_col the name of the time column
#' @param event_col the name of the event column
#' @param group_cols the columns to group by
#' @param marginal_cols the columns to add marginal groups for
#' @param obfuscate_data whether or not to obfuscate data
#' @param add_reason_col whether or not to add the reason for data being
#' censored
#' @param censored_value the value to use when observations are censored
#' @param censored_limit lower limir for at risk before censoring
#'
#' @return estimated survival at the requested point in time for each group
#' @export
get_surv_value <- function(df,
                           time,
                           time_col = "time",
                           event_col = "status",
                           group_cols = NULL,
                           marginal_cols,
                           obfuscate_data = FALSE,
                           add_reason_col = TRUE,
                           censored_value = NA,
                           censored_limit = 15) {

  checkmate::assert_data_frame(df)
  checkmate::assert_number(time, lower = 0)
  checkmate::assert_string(time_col)
  checkmate::assert_string(event_col)
  checkmate::assert_character(group_cols, null.ok = TRUE)
  checkmate::assert_names(
    names(df),
    must.include = c(time_col, event_col, group_cols)
  )
  checkmate::assert_logical(obfuscate_data, len = 1)
  checkmate::assert_logical(add_reason_col, len = 1)
  checkmate::assert_integerish(censored_limit, lower = 0, len = 1)

  if (rlang::is_missing(marginal_cols)) {
    marginal_cols <- group_cols
  } else {
    checkmate::assert_subset(marginal_cols, group_cols)
  }

  original_df <- df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(marginal_cols),
        as.character
      )
    )

  df <- df |>
    dplyr::mutate(
      any_na = dplyr::if_any(
        dplyr::all_of(
          c(group_cols, time_col, event_col)
        ),
        is.na
      )
    )

  if (any(df$any_na)) {
    cli::cli_alert_warning(
      sprintf(
        paste0(
          "Removing ({.val {%d}} / {.val {%d}} [{.val {%.2f}}%%]) missing ",
          "observations in {.code group_cols}, {.code time_col},",
          " and {.code event_col}"
        ),
        sum(df$any_na),
        nrow(df),
        100 * sum(df$any_na) / nrow(df)
      )
    )
    df <- df |>
      dplyr::filter(!.data$any_na)
  }

  if (nrow(df) == 0) {
    return(
      tibble::tibble(
        estimate = NA,
        total = 0,
        cum_events = 0
      )
    )
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

  res <- broom::tidy(fit)

  if (obfuscate_data) {
    res <- obfuscate_surv(
      df = res,
      add_reason_col = add_reason_col,
      censored_value = censored_value,
      censored_limit = censored_limit
    )
  }

  res <- res |>
    dplyr::filter(.data$time <= .env$time) |>
    dplyr::group_by(dplyr::across(dplyr::any_of("strata"))) |>
    dplyr::mutate(total = .data$n.risk[1]) |>
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
          stringr::str_squish
        ),
        dplyr::across(
          dplyr::all_of(group_cols),
          ~ convert(
            .x,
            class(df[[dplyr::cur_column()]])
          )
        )
      ) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)))
  }

  res |>
    dplyr::mutate(
      cum_events = cumsum(.data$n.event)
    ) |>
    dplyr::ungroup() |>
    tidyr::complete(
      dplyr::select(original_df, dplyr::any_of(group_cols)),
      fill = list(total = 0, cum_events = 0)
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of("obfuscated_reason"),
        ~ dplyr::case_when(
          is.na(.x) & .data$total == 0 ~ "N < 15",
          .default = .x
        )
      )
    ) |>
    dplyr::select(
      dplyr::all_of(group_cols),
      "estimate",
      "cum_events",
      "total",
      dplyr::any_of("obfuscated_reason")
    )
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
  } else if (cls == "Date") {
    lubridate::ymd(x)
  } else {
    cli::cli_alert_warning(
      "{.arg cls} was not matched, returning {.arg x} unmodified"
    )
    x
  }
}

#' Obfuscate result from [get_surv_value()]
#'
#' @param df data.frame
#' @param add_reason_col add obfuscated reason
#' @param censored_value the value to use when censoring observations
#' @param censored_limit lower limir for at risk before censoring
#'
#' @return data.frame
obfuscate_surv <- function(df,
                           add_reason_col = TRUE,
                           censored_value = NA,
                           censored_limit = 15) {

  if (!all(c("estimate", "n.risk") %in% names(df))) {
    cli::cli_abort(
      paste0(
        "Variables: {.code estimate}, {.code n.risk} ",
        "are missing, returning {.arg df} unmodified"
      )
    )
  }

  reason_col <- function(df, add_reason_col, censored_limit) {
    if (!add_reason_col) {
      return(df)
    } else {
      df |>
        dplyr::mutate(
          obfuscated_reason = dplyr::case_when(
            .data$n.risk < censored_limit ~ sprintf("N < %s", censored_limit),
            .default = NA
          )
        )
    }
  }

  df |>
    reason_col(add_reason_col, censored_limit) |>
    dplyr::mutate(
      estimate = dplyr::case_when(
        .data$n.risk < censored_limit ~ censored_value,
        .default = .data$estimate
      )
    )
}
