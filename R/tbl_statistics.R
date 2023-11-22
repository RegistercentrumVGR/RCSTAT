# Contains functions group_proportions, group_means
#

#' Proportions by group
#'
#' Counts number of observations in each group.
#' drops last grouping variable and counts total.
#' Calculates proportion for each grouping
#'
#'
#' @param data A data.frame or tibble
#' @param ... variables to group on
#'
#' @export group_proportions
group_proportions <- function(data, ...) {

  data |>
    dplyr::group_by(...) |>
    dplyr::summarise(
      .groups = "drop_last",
      n = dplyr::n()
    ) |>
    dplyr::mutate(
      Nt = sum(.data[["n"]], na.rm = TRUE),
      p = .data[["n"]] / .data[["Nt"]]
    ) |>
    dplyr::rename(
      Count = "n",
      Total = "Nt",
      Proportion = "p"
    ) |>
    dplyr::ungroup()
}

#' Calculate n, means and sd by group
#'
#' @param data A data.frame or tibble
#' @param ... Variables to group on by
#' @param vars Variables to calculate means and sd on.
#' Defaults to all vars in data.
#'
#' @export group_means
group_means <- function(data, ..., vars = names(data)) {
  # Remove grouping-vars from vars if present
  vars <- setdiff(vars, names(dplyr::select(data, ...)))

  data |>
    dplyr::group_by(...) |>
    dplyr::summarise(
      .groups = "drop",
      n = dplyr::n(),
      dplyr::across(
        .cols = tidyselect::all_of(vars),
        .fns = list(
          mean = \(x) mean(x, na.rm = TRUE),
          sd = \(x) stats::sd(x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      )
    )
}
#' Calculates proportion of missing data
#'
#' Calculates n missing and proportion missing across
#' columns and by groups specified.
#'
#' @param data data.frame or tibble
#' @param ... grouping to apply before calculation
#' @param vars vars to calculate proportion of missing
#' data on. Defaults to all except grouping vars.
#'
#' @export proportion_missing
proportion_missing <- function(data, ..., vars = names(data)) {
  # Remove grouping-vars from vars if present
  vars <- setdiff(vars, names(dplyr::select(data, ...)))

  data |>
    dplyr::group_by(...) |>
    dplyr::summarise(
      .groups = "drop",
      N = dplyr::n(),
      dplyr::across(
        .cols = tidyselect::all_of(vars),
        .fns = \(x) sum(is.na(x), na.rm = TRUE)
      ),
      dplyr::across(
        .cols = tidyselect::all_of(vars),
        .fns = \(x) x / .data[["N"]],
        .names = "Proportion_missing_{.col}"
      )
    )
}
