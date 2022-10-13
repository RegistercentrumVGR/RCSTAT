#' Derives birthdate from Swedish social security number
#'
#' @param x Swedish social security number
#'
#' @export birthdate
andel_missing <- function(data, vars = names(data), ...){

  result <- data %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(
      .groups = "keep",
      N = dplyr::n(),
      dplyr::across(
        .cols = all_of(vars),
        .fns = ~ sum(is.na(.x)),
        .names = "n_missing_{.col}"
      ),
      dplyr::across(
        .cols = all_of(vars),
        .fns = ~ sum(is.na(.x))/N,
        .names = "Proportion_missing_{.col}"
      )
    )

  return(result)
}
