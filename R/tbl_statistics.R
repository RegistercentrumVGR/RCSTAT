#' Proportions by group
#'
#' Counts number of observations in each group.
#' drops last grouping variable and counts total.
#' Calculates proportion for each grouping
#'
#' @param data A data.frame or tibble
#' @param group_by variables to group on
#' @param obfuscate If data should be non-revealing
#'
#' @export group_proportions
group_proportions <- function(
    data,
    group_by,
    obfuscate = TRUE) {

  res <- data |>
    dplyr::group_by(dplyr::pick(tidyselect::all_of(group_by))) |>
    dplyr::summarise(
      .groups = "drop_last",
      n = dplyr::n()
    ) |>
    dplyr::mutate(
      Nt = sum(.data[["n"]], na.rm = TRUE),
      p = .data[["n"]] / .data[["Nt"]]
    ) |>
    dplyr::ungroup()

  # Make data non-revealing
  if (obfuscate) {
    res <- obfuscate_data(
      data = res,
      prop_var = "p",
      count_var = "n",
      total_var = "Nt"
    )
  }
  dplyr::rename(
    res,
    "Count" = "n", "Total" = "Nt", "Proportion" = "p",
  )
}

#' Calculate n, means and sd by group
#'
#' @param data A data.frame or tibble
#' @param group_by Variables to group on by
#' @param vars Variables to calculate means and sd on.
#' Defaults to all vars in data.
#' @param obfuscate If data should be non-revealing
#' @param ... Arguments passed to obfuscate
#' @export group_means
group_means <- function(
    data,
    group_by,
    vars = NULL,
    obfuscate = TRUE,
    ...) {
  if (is.null(vars)) {
    vars <- setdiff(names(data), group_by)
  }

  res <- data |>
    dplyr::group_by(dplyr::pick(tidyselect::all_of(group_by))) |>
    dplyr::summarise(
      .groups = "drop",
      n = dplyr::n(),
      dplyr::across(
        .cols = tidyselect::all_of(vars),
        .fns = list(
          non_missing = \(x) sum(!is.na(x))
        ),
        .names = "{.col}_{.fn}"
      ),
      dplyr::across(
        .cols = tidyselect::all_of(vars),
        .fns = list(
          mean = \(x) mean(x, na.rm = TRUE),
          sd = \(x) stats::sd(x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      )
    )

  if (obfuscate) {
    res <- obfuscate_data(
      data = res,
      total_var = "n",
      other_count_vars = paste0(vars, "_non_missing"),
      statistics_vars = c(paste0(vars, "_sd"), paste0(vars, "_mean")),
      ...
    )
  }
  res
}
#' Calculates proportion of missing data
#'
#' Calculates n missing and proportion missing across
#' columns and by groups specified.
#'
#' @param data data.frame or tibble
#' @param group_by grouping to apply before calculation
#' @param vars vars to calculate proportion of missing
#' data on. Defaults to all except grouping vars.
#' @param obfuscate If data should be non-revealing
#' @param ... Arguments passed to obfuscate
#' @export proportion_missing
proportion_missing <- function(
    data,
    group_by,
    vars = NULL,
    obfuscate = TRUE,
    ...) {

  if (is.null(vars)) {
    vars <- setdiff(names(data), group_by)
  }

  res <- data |>
    dplyr::group_by(dplyr::pick(tidyselect::all_of(group_by))) |>
    dplyr::summarise(
      .groups = "drop",
      N = dplyr::n(),
      dplyr::across(
        .cols = tidyselect::all_of(vars),
        .fns = \(x) sum(is.na(x))
      ),
      dplyr::across(
        .cols = tidyselect::all_of(vars),
        .fns = \(x) x / .data[["N"]],
        .names = "proportion_missing_{.col}"
      )
    )

  if (obfuscate) {
    res <- obfuscate_data(
      data = res,
      statistics_vars = paste0("proportion_missing_", vars),
      total_var = "N",
      other_count_vars = c("x", "c"),
      ...
    )
  }
  res
}
