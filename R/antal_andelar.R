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
    )

  return(result)
}

#' Calculate n, means and sd by group
#'
#' @param data A data.frame or tibble
#' @param vars Variables to calculate means and sd on
#' @param ... Variables to group on by
#'
#' @export group_means
group_means <- function(data, vars, ...){

  result <- data %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(
      .groups = "drop",
      Count = dplyr::n(),
      dplyr::across(
         .cols = all_of(vars),
         .fns = list(mean = mean, sd = sd),
         na.rm = TRUE
      )
    )

  return(result)
}
