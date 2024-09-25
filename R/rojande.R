#' Round number to nearest y
#'
#' @param x The number to round
#' @param y The number to round to
#'
#' @export
#'
#' @examples
#' round_to_y(x = 0.12, y = 0.05)
round_to_y <- function(x, y = 0.05) {
  return(roundc(x / y) * y)
}

#' Make data non-revealing
#'
#' @param data The data.frame to obfuscate
#' @param total_var The column containing the denominator
#' @param count_var The column containing the numerator
#' @param prop_var The column containing the proportion as determined by
#' total_var and count_var
#' @param group_var The variables to group by before obfuscating. If data is
#' already grouped the grouping will be overwritten
#' @param statistics_vars Other statistics to be obfuscated such as mean,
#' standard deviation, etc. These are hidden if total_var is < 15
#' @param other_count_vars Other count vars to obfuscate. These have not been
#' used to calculate proportions. These are simply rounded to nearest 10
#' @param round_statistics_vars Whether or not to round statistics_vars
#' @param round_statistics_digits The number of digits statistics_vars are
#' rounded to. Passed to RCStat::roundc. Should be an integer or a list.
#' If it is a list it should a named list where each name is a column in
#' statistics_vars. The corresponding value in the list is the number of digits
#' to round to.
#' @param add_reason_col Whether or not to add a column called obfuscated_reason
#' indicating why a row was obfuscated with either "n < 5" or "N < 15" if
#' all of `count_var`, `total_var`, and `prop_var` are supplied. If only
#' `total_var` is found the only obfuscated_reason can be "N < 5"
#' @param liberal_obfuscation Whether or not to use liberal obfuscation. If
#' this is true we round proportions to 5% when the numerator < 5 and
#' the denominator is >= 45. We also do nothing when the numerator is < 5 and
#' the denominator is >= 245.
#' @param censored_value What to replace prop_var with when it is censored.
#' When working with bars it is useful to censor with 0 to not
#' affect order of bars but when producing a line plot 0 does not make sense.
#' @param inform Whether or not to print information about the current group
#' vars when `group_var` is `NULL`
#' @param prop_scale Argument indicating what scale the prop_var is. Either 1
#' or 100
#'
#' @export
#'
#' @example man/examples/rojande.R
#'
obfuscate_data <- function(
    data,
    total_var = "total",
    count_var = "n",
    prop_var = "prop",
    group_var = NULL,
    statistics_vars = NULL,
    other_count_vars = NULL,
    round_statistics_vars = FALSE,
    round_statistics_digits = 2,
    add_reason_col = FALSE,
    liberal_obfuscation = TRUE,
    censored_value = 0,
    inform = TRUE,
    prop_scale = 1) {
  checkmate::assert_choice(prop_scale, c(1, 100))

  if (!is.null(group_var)) {
    data <- data |>
      dplyr::group_by(
        dplyr::across(
          tidyselect::all_of(group_var)
        )
      )
  } else if (inform && length(dplyr::group_vars(data)) > 0) {
    msg <- paste0(
      "obfuscate_data: data is grouped by ",
      paste0("'", dplyr::group_vars(data), "'", collapse = ", "),
      ", is this intentional?"
    )

    rlang::inform(msg)
  }

  if (prop_scale == 100) {
    data <- data |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::any_of(prop_var),
          ~ .x / 100
        )
      )
  }

  if (add_reason_col) {
    data <- reason_col(
      data = data,
      liberal_obfuscation = liberal_obfuscation,
      count_var = count_var,
      total_var = total_var,
      prop_var = prop_var,
      statistics_vars = statistics_vars
    )
  }


  if (length(dplyr::group_vars(data)) > 0) {
    if (liberal_obfuscation) {
      data <- data |>
        dplyr::mutate(
          dplyr::across(
            tidyselect::any_of(prop_var),
            ~ dplyr::case_when(
              .data[[total_var]] < 15 ~ 0,
              .data[[total_var]] < 45 ~ dplyr::if_else(
                rep(any(.data[[count_var]] < 5), dplyr::n()),
                rep(censored_value, dplyr::n()),
                roundc(.x, digits = 2)
              ),
              .data[[total_var]] < 245 ~ dplyr::if_else(
                .data[[count_var]] < 5 | .data[[total_var]] - .data[[count_var]] < 5,
                round_to_y(.x, y = 0.05),
                roundc(.x, digits = 2)
              ),
              .data[[total_var]] >= 245 ~ roundc(.x, digits = 2)
            )
          )
        )
    } else {
      data <- data |>
        dplyr::mutate(
          dplyr::across(
            tidyselect::any_of(prop_var),
            ~ dplyr::case_when(
              .data[[total_var]] < 15 ~ 0,
              .default = dplyr::if_else(
                rep(any(.data[[count_var]] < 5), dplyr::n()),
                rep(censored_value, dplyr::n()),
                roundc(.x, digits = 2)
              )
            )
          )
        )
    }
  } else {
    if (liberal_obfuscation) {
      data <- data |>
        dplyr::mutate(
          dplyr::across(
            tidyselect::any_of(prop_var),
            ~ dplyr::case_when(
              .data[[total_var]] < 15 ~ censored_value,
              .data[[total_var]] < 45 ~ dplyr::if_else(
                .data[[count_var]] < 5 | .data[[total_var]] - .data[[count_var]] < 5,
                censored_value,
                roundc(.x, digits = 2)
              ),
              .data[[total_var]] < 245 ~ dplyr::if_else(
                .data[[count_var]] < 5 | .data[[total_var]] - .data[[count_var]] < 5,
                round_to_y(.x, y = 0.05),
                roundc(.x, digits = 2)
              ),
              .data[[total_var]] >= 245 ~ roundc(.x, digits = 2)
            )
          )
        )
    } else {
      data <- data |>
        dplyr::mutate(
          dplyr::across(
            tidyselect::any_of(prop_var),
            ~ dplyr::case_when(
              .data[[total_var]] < 15 ~ censored_value,
              .data[[count_var]] < 5 ~ censored_value,
              .data[[total_var]] - .data[[count_var]] < 5 ~ censored_value,
              .default = roundc(.x, digits = 2)
            )
          )
        )
    }
  }

  data <- data |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::any_of(c(count_var, total_var, other_count_vars)),
        ~ roundc(.x, -1)
      )
    )

  # To allow using this function even without total_var, only used in rare
  # occasions

  if (total_var %in% colnames(data)) {
    data <- data |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::any_of(statistics_vars),
          ~ dplyr::if_else(
            .data[[total_var]] < 15,
            NA,
            .x
          )
        )
      )
  } else if (any(statistics_vars %in% colnames(data))) {
    warning(
      "total_var was not found among the columns, unable to censor statistics_vars"
    )
  }

  if (round_statistics_vars) {
    if (is.numeric(round_statistics_digits)) {
      data <- data |>
        dplyr::mutate(
          dplyr::across(
            tidyselect::any_of(statistics_vars),
            ~ roundc(.x, digits = round_statistics_digits)
          )
        )
    } else {
      for (stat_var in statistics_vars) {
        data <- data |>
          dplyr::mutate(
            !!stat_var := roundc(
              .data[[stat_var]],
              digits = round_statistics_digits[[stat_var]]
            )
          )
      }
    }
  }

  if (prop_scale == 100) {
    data <- data |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::any_of(prop_var),
          ~ .x * 100
        )
      )
  }

  return(data)
}


#' Internal function to add obfuscated reason column
#'
#' @param data Data to be obfuscated
#' @param liberal_obfuscation Whether or not to use liberal obfuscation
#' @param count_var The column containing the numerator
#' @param total_var The column containing the denominator
#' @param prop_var The column containing the proportion as determined by
#' `count_var` and `total_var`
#' @param statistics_vars Columns containing statistics such as mean, sd, etc.

reason_col <- function(
    data,
    liberal_obfuscation = TRUE,
    count_var = "n",
    total_var = "total",
    prop_var = "prop",
    statistics_vars = NULL) {
  n_group_vars <- length(dplyr::group_vars(data))

  all_vars <- all(c(count_var, total_var, prop_var) %in% colnames(data))

  if (all_vars && liberal_obfuscation && n_group_vars > 0) {
    data <- data |>
      dplyr::mutate(
        obfuscated_reason = dplyr::case_when(
          .data[[total_var]] < 15 ~ "N < 15",
          .data[[total_var]] < 45 ~ dplyr::if_else(
            rep(any(.data[[count_var]] < 5), dplyr::n()),
            rep("n < 5", dplyr::n()),
            NA
          ),
          dplyr::between(.data[[total_var]], 45, 244) &
            (.data[[count_var]] < 5 | .data[[total_var]] -
               .data[[count_var]] < 5) ~ "rounded to nearest 5%"
        )
      )
  }

  if (all_vars && liberal_obfuscation && n_group_vars == 0) {
    data <- data |>
      dplyr::mutate(
        obfuscated_reason = dplyr::case_when(
          .data[[total_var]] < 15 ~ "N < 15",
          .data[[total_var]] < 45 ~
            dplyr::case_when(
              .data[[count_var]] < 5 ~ "n < 5",
              .data[[total_var]] - .data[[count_var]] < 5 ~ "N - n < 5",
              .default = NA
            ),
          dplyr::between(.data[[total_var]], 45, 244) &
            (.data[[count_var]] < 5 | .data[[total_var]] -
               .data[[count_var]] < 5) ~ "rounded to nearest 5%"
        )
      )
  }

  if (all_vars && !liberal_obfuscation && n_group_vars > 0) {
    data <- data |>
      dplyr::mutate(
        obfuscated_reason = dplyr::case_when(
          .data[[total_var]] < 15 ~ "N < 15",
          .default = dplyr::if_else(
            rep(any(.data[[count_var]] < 5), dplyr::n()),
            rep("n < 5", dplyr::n()),
            NA
          )
        )
      )
  }

  if (all_vars && !liberal_obfuscation && n_group_vars == 0) {
    data <- data |>
      dplyr::mutate(
        obfuscated_reason = dplyr::case_when(
          .data[[total_var]] < 15 ~ "N < 15",
          .data[[count_var]] < 5 ~ "n < 5",
          .data[[total_var]] - .data[[count_var]] < 5 ~ "N - n < 5"
        )
      )
  }

  if (!all_vars && total_var %in% colnames(data)) {
    data <- data |>
      dplyr::mutate(
        obfuscated_reason = dplyr::if_else(
          .data[[total_var]] < 5,
          "N < 5",
          NA
        )
      )
  }

  if (!all_vars &&
        any(statistics_vars %in% colnames(data)) &&
        total_var %in% colnames(data)) {
    data <- data |>
      dplyr::mutate(
        obfuscated_reason = dplyr::if_else(
          .data[[total_var]] < 15,
          "N < 15",
          NA
        )
      )
  }

  return(data)
}

#' Get rounded ci for proportion
#'
#' @param p_hat estimate of proportion
#' @param n sample size
#' @param alpha alpha level
#'
#' @export
rounded_ci_p <- function(p_hat, n, alpha = 0.05) {
  z <- stats::qnorm(1 - alpha / 2)
  width <- roundc(z * sqrt((p_hat * (1 - p_hat)) / n), digits = 2)
  p_hat <- roundc(p_hat, digits = 2)
  list(lower = pmax(0, p_hat - width), upper = pmin(1, p_hat + width))
}
