#' Proportions by group
#'
#' Counts number of observations in each group.
#' drops last grouping variable and counts total.
#' Calculates proportion for each grouping
#'
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
      proportion_vars = "p",
      freq_vars = c("n", "Nt")
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
#' @param ... Variables to group on by
#' @param vars Variables to calculate means and sd on.
#' Defaults to all vars in data.
#' @param obfuscate If data should be non-revealing
#'
#' @export group_means
group_means <- function(
    data,
    group_by,
    vars = NULL,
    obfuscate = TRUE) {
  if (is.null(vars)) {
    vars <- setdiff(group_by, names(data))
  }

  res <- data |>
    dplyr::group_by(dplyr::pick(tidyselect::all_of(group_by))) |>
    dplyr::summarise(
      .groups = "drop",
      n = dplyr::n(),
      dplyr::across(
        .cols = tidyselect::all_of(vars),
        .fns = list(
          mean = \(x) if (.data[["n"]] > 14) mean(x, na.rm = TRUE) else NA,
          sd = \(x) if (.data[["n"]] > 14) stats::sd(x, na.rm = TRUE) else NA
        ),
        .names = "{.col}_{.fn}"
      )
    )

  if (obfuscate) {
    res <- obfuscate_data(data = res, freq_vars = "n")
  }
  res
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
#' @param obfuscate If data should be non-revealing
#'
#' @export proportion_missing
proportion_missing <- function(
    data,
    group_by,
    vars = NULL,
    obfuscate = TRUE) {

  if (is.null(vars)) {
    vars <- setdiff(group_by, names(data))
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
        .names = "Proportion_missing_{.col}"
      )
    )

  if (obfuscate) {
    res <-
      obfuscate_data(
        data = res,
        proportion_vars = paste0("Proportion_missing_", vars),
        freq_vars = c(vars, "N")
      )
  }
  res
}
