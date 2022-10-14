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
group_proportions <- function(data, ...){

  result <- data %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(
      .groups = "drop_last",
      n = dplyr::n()
    ) %>%
    dplyr::mutate(
      N = sum(n),
      p = n/N
    ) %>%
    dplyr::rename(
      Count = n,
      Total = N,
      Proportion = p
    ) %>%
    ungroup()

  return(result)
}

#' Calculate n, means and sd by group
#'
#' @param data A data.frame or tibble
#' @param ... Variables to group on by
#' @param vars Variables to calculate means and sd on.
#' Defaults to all vars in data.
#'
#' @export group_means
group_means <- function(data, ..., vars = names(data)){
  # Remove grouping-vars from vars if present
  vars <- setdiff(vars, names(select(data, ...)))

  result <- data %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(
      .groups = "drop",
      n = dplyr::n(),
      dplyr::across(
        .cols = all_of(vars),
        .fns = list(mean = mean, sd = sd),
        na.rm = TRUE
      )
    )

  return(result)
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
proportion_missing <- function(data, ..., vars = names(data)){
  # Remove grouping-vars from vars if present
  vars <- setdiff(vars, names(dplyr::select(data, ...)))

  result <- data %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(
      .groups = "drop",
      N = dplyr::n(),
      dplyr::across(
        .cols = all_of(vars),
        .fns = ~ sum(is.na(.x)),
        .names = "n_missing_{.col}"
      ),
      dplyr::across(
        .cols = all_of(paste0("n_missing_", vars)),
        .fns = ~ .x/N,
        .names = "Proportion_missing_{.col}"
      )
    )

  return(result)
}
