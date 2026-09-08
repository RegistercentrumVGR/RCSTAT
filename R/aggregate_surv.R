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
#' @param estimate what the estimate should be, either "survival" or "event"
#' @param include_ci should confidence intervals be included in the final
#' data.frame
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
                           censored_limit = 15,
                           estimate = "survival",
                           include_ci = FALSE) {

  checkmate::assert_number(time, lower = 0)
  checkmate::assert_choice(estimate, c("survival", "event"))

  fit <- fit_surv(
    df = df,
    time_col = time_col,
    event_col = event_col,
    group_cols = group_cols,
    marginal_cols = marginal_cols,
    obfuscate_data = obfuscate_data,
    add_reason_col = add_reason_col,
    censored_value = censored_value,
    censored_limit = censored_limit
  )

  if (is.null(fit)) {
    return(
      tibble::tibble(
        estimate = NA,
        total = 0,
        cum_events = 0
      )
    )
  }

  res <- fit$res |>
    dplyr::filter(.data$time <= .env$time) |>
    dplyr::group_by(dplyr::across(dplyr::any_of("strata"))) |>
    dplyr::mutate(total = .data$n.risk[1]) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    unnest_strata(group_cols, fit$model_df)

  res <- if (is.null(group_cols)) {
    if (nrow(res) == 0) {
      tibble::tibble(estimate = NA, total = 0, cum_events = 0)
    } else {
      res
    }
  } else {
    res |>
      tidyr::complete(
        dplyr::select(fit$original_df, dplyr::any_of(group_cols)),
        fill = list(total = 0, cum_events = 0)
      )
  }

  if (include_ci) {
    ci <- c("conf.low", "conf.high")
  } else {
    ci <- NULL
  }

  res <- res |>
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
      dplyr::any_of(ci),
      "cum_events",
      "total",
      dplyr::any_of("obfuscated_reason")
    )

  if (estimate == "event") {
    res <- res |>
      dplyr::mutate(
        dplyr::across(dplyr::any_of("conf.low"), ~ 1 - .x),
        dplyr::across(dplyr::any_of("conf.high"), ~ 1 - .x),
        estimate = 1 - .data$estimate
      )

    if (all(c("conf.low", "conf.high") %in% names(res))) {
      res <- res |>
        dplyr::rename(conf.low = "conf.high", conf.high = "conf.low") |>
        dplyr::relocate("conf.low", .before = "conf.high")
    }
  }

  res
}

#' Gets the estimated survival curve, suitable for plotting
#'
#' Like [get_surv_value()], but returns the full survival curve (one row
#' per event/censoring time, per group) instead of the estimate at a
#' single point in time.
#'
#' @inheritParams get_surv_value
#'
#' @return estimated survival curve for each group
#' @export
get_surv_curve <- function(df,
                           time_col = "time",
                           event_col = "status",
                           group_cols = NULL,
                           marginal_cols,
                           obfuscate_data = FALSE,
                           add_reason_col = TRUE,
                           censored_value = NA,
                           censored_limit = 15,
                           estimate = "survival",
                           include_ci = FALSE) {

  checkmate::assert_choice(estimate, c("survival", "event"))

  fit <- fit_surv(
    df = df,
    time_col = time_col,
    event_col = event_col,
    group_cols = group_cols,
    marginal_cols = marginal_cols,
    obfuscate_data = obfuscate_data,
    add_reason_col = add_reason_col,
    censored_value = censored_value,
    censored_limit = censored_limit
  )

  if (is.null(fit)) {
    return(
      tibble::tibble(
        time = numeric(0),
        estimate = numeric(0),
        n.risk = numeric(0),
        cum_events = numeric(0)
      )
    )
  }

  if (include_ci) {
    ci <- c("conf.low", "conf.high")
  } else {
    ci <- NULL
  }

  res <- fit$res |>
    unnest_strata(group_cols, fit$model_df) |>
    dplyr::select(
      dplyr::all_of(group_cols),
      "time",
      "estimate",
      dplyr::any_of(ci),
      "n.risk",
      "cum_events",
      dplyr::any_of("obfuscated_reason")
    )

  if (estimate == "event") {
    res <- res |>
      dplyr::mutate(
        dplyr::across(dplyr::any_of("conf.low"), ~ 1 - .x),
        dplyr::across(dplyr::any_of("conf.high"), ~ 1 - .x),
        estimate = 1 - .data$estimate
      )

    if (all(c("conf.low", "conf.high") %in% names(res))) {
      res <- res |>
        dplyr::rename(conf.low = "conf.high", conf.high = "conf.low") |>
        dplyr::relocate("conf.low", .before = "conf.high")
    }
  }

  res
}

#' Fit a survival curve and tidy the result
#'
#' Shared implementation for [get_surv_value()] and [get_surv_curve()].
#' Fits `survival::survfit()`, tidies the result, optionally obfuscates it,
#' and computes cumulative events per group across all event/censoring
#' times (i.e. the full curve, unfiltered by time).
#'
#' @inheritParams get_surv_value
#'
#' @return a list with the tidied `res` (one row per group per
#' event/censoring time) and the `original_df` used to complete missing
#' groups, or `NULL` if `df` has no rows once missing observations are
#' removed
fit_surv <- function(df,
                     time_col = "time",
                     event_col = "status",
                     group_cols = NULL,
                     marginal_cols,
                     obfuscate_data = FALSE,
                     add_reason_col = TRUE,
                     censored_value = NA,
                     censored_limit = 15) {

  checkmate::assert_data_frame(df)
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
    return(NULL)
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

  surv_fit <- survival::survfit(fml, data = df)

  res <- broom::tidy(surv_fit)

  if (obfuscate_data) {
    res <- obfuscate_surv(
      df = res,
      add_reason_col = add_reason_col,
      censored_value = censored_value,
      censored_limit = censored_limit
    )
  }

  res <- res |>
    dplyr::arrange(.data$time) |>
    dplyr::group_by(dplyr::across(dplyr::any_of("strata"))) |>
    dplyr::mutate(
      cum_events = cumsum(.data$n.event)
    ) |>
    dplyr::ungroup()

  if ("state" %in% names(res)) {
    res <- res |>
      dplyr::filter(.data$state == "(s0)")
  }

  list(res = res, original_df = original_df, model_df = df)
}

#' Split the `strata` column of a tidied survfit into `group_cols`
#'
#' Shared implementation for [get_surv_value()] and [get_surv_curve()],
#' used after any grouping/filtering that depends on the raw `strata`
#' column has already happened.
#'
#' @param res tidied, filtered `survival::survfit()` result with a `strata`
#' column
#' @param group_cols the columns to group by
#' @param model_df the data used to fit the survival curve (used to recover
#' the original class of each group column)
#'
#' @return `res` with `strata` split into one column per entry of
#' `group_cols`
unnest_strata <- function(res, group_cols, model_df) {
  if (is.null(group_cols)) {
    return(res)
  }

  if (nrow(res) == 0) {
    for (col in group_cols) {
      res[[col]] <- convert(character(0), class(model_df[[col]]))
    }
    return(res)
  }

  res |>
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
          class(model_df[[dplyr::cur_column()]])
        )
      )
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
