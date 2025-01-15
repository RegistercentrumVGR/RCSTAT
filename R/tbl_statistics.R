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


#' Create aggregate data across all combinations, including totals.
#' Can specify if proportion or average should be used.
#'
#' @param df A data frame.
#' @param group_cols The columns to group by.
#' @param vars Variables to be used to calculate the proportion from.
#' @param include_missing If missing values should be included in the total
#' @param obfuscate_data If data should be obfuscated
#' @param censored_value What value to replace censored values, used as argument in obfuscate_data
#' @export get_aggregate_value

get_aggregate_value <- function(
    df,
    group_cols = NULL,
    vars = NULL,
    include_missing = TRUE,
    obfuscate_data = FALSE,
    censored_value = 0) {
  #### Warnings ####

  checkmate::assert_data_frame(df, min.rows = 1)
  checkmate::assert_list(vars, min.len = 1, any.missing = FALSE)
  checkmate::assert_logical(include_missing, len = 1, any.missing = FALSE)
  checkmate::assert_logical(obfuscate_data, len = 1, any.missing = FALSE)
  checkmate::assert_subset(group_cols, names(df), empty.ok = FALSE)
  checkmate::assert_subset(unlist(vars), names(df), empty.ok = FALSE)
  checkmate::assert_subset(
    names(vars),
    c("prop", "mean", "median", "prop_count"),
    empty.ok = FALSE
  )

  prop_var <- vars[["prop"]]
  mean_var <- vars[["mean"]]
  median_var <- vars[["median"]]
  prop_count_var <- vars[["prop_count"]]

  numeric_vars <- c(prop_var, mean_var, median_var)

  # Check if vars variables are numeric
  checkmate::assert_data_frame(
    dplyr::select(df, tidyselect::all_of(numeric_vars)),
    types = c("numeric", "logical")
  )

  #### Create the Groups ####
  # Make all the grouping variables characters
  df <- df |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(group_cols), as.character
      )
    )

  # Return data frame
  out <- data.frame()

  # Get all the combinations of the variables
  group_var_combinations <- do.call(
    c,
    lapply(
      seq_along(group_cols) - 1,
      utils::combn,
      x = group_cols,
      simplify = FALSE
    )
  )

  group_var_combinations[[length(group_var_combinations) + 1]] <- group_cols


  #### Calculation ####
  # Get the variables and the corresponding statistic that is to be reported

  # Create list to be used as argument .fns in across
  prop_missing_list <- list(
    total_non_missing = function(x) sum(!is.na(x)),
    total_missing = function(x) sum(is.na(x)),
    n = function(x) sum(x, na.rm = TRUE),
    prop = function(x) sum(x, na.rm = TRUE) / sum(!is.na(x))
  )

  prop_list <- list(
    n = function(x) sum(x, na.rm = TRUE),
    prop = function(x) sum(x, na.rm = TRUE) / dplyr::n()
  )


  mean_list <- list(
    mean = function(x) mean(x, na.rm = TRUE),
    std = function(x) stats::sd(x, na.rm = TRUE),
    total_non_missing = function(x) sum(!is.na(x))
  )


  median_list <- list(
    median = function(x) stats::median(x, na.rm = TRUE),
    quant_5 = function(x) stats::quantile(x, probs = 0.05, na.rm = TRUE),
    quant_25 = function(x) stats::quantile(x, probs = 0.25, na.rm = TRUE),
    quant_75 = function(x) stats::quantile(x, probs = 0.75, na.rm = TRUE),
    quant_95 = function(x) stats::quantile(x, probs = 0.95, na.rm = TRUE),
    total_non_missing = function(x) sum(!is.na(x))
  )


  if (include_missing) {
    prop_fns <- prop_list
  } else {
    prop_fns <- prop_missing_list
  }



  # Perform the calculation
  for (comb in group_var_combinations) {
    all_cols <- setdiff(group_cols, comb)

    temp <- df |>
      dplyr::group_by(
        dplyr::across(
          tidyselect::all_of(comb)
        )
      ) |>
      dplyr::summarise(
        dplyr::across(
          .cols = tidyselect::all_of(prop_var),
          .fns = prop_fns
        ),
        dplyr::across(
          .cols = tidyselect::all_of(mean_var),
          .fns = mean_list
        ),
        dplyr::across(
          .cols = tidyselect::all_of(median_var),
          .fns = median_list
        ),
        dplyr::across(
          .cols = tidyselect::all_of(prop_count_var),
          .fns = ~ count_prop_wide(
            .x,
            include_missing = include_missing,
            obfuscate_data = obfuscate_data,
            censored_value = censored_value
          ),
          .unpack = TRUE
        ),
        total = dplyr::n(),
        .groups = "drop"
      )


    for (cols in all_cols) {
      # Change County variable to contain Riket not Alla
      if (grepl("County", cols)) {
        temp <- temp |>
          dplyr::ungroup() |>
          dplyr::mutate(!!dplyr::sym(cols) := "Riket")
      } else {
        temp <- temp |>
          dplyr::ungroup() |>
          dplyr::mutate(!!dplyr::sym(cols) := "Alla")
      }
    }


    out <- dplyr::bind_rows(out, temp)
  }

  # Obfuscate the data with missing included.
  if (obfuscate_data && include_missing) {
    for (var in prop_var) {
      out <- out |> RCStat::obfuscate_data(
        total_var = "total",
        count_var = paste0(var, "_n"),
        prop_var = paste0(var, "_prop"),
        censored_value = censored_value
      )
    }
  } else if (obfuscate_data && !include_missing) {
    # Obfuscate the data with missing not included.
    for (var in prop_var) {
      out <- out |> RCStat::obfuscate_data(
        total_var = paste0(var, "_total_non_missing"),
        count_var = paste0(var, "_n"),
        prop_var = paste0(var, "_prop"),
        censored_value = censored_value
      )
    }
  }

  if (obfuscate_data) {
    out <- out |> RCStat::obfuscate_data(
      total_var = paste0(mean_var, "_total_non_missing"),
      statistics_vars = c(
        paste0(mean_var, "_mean"),
        paste0(mean_var, "_std")
      ),
      censored_value = censored_value,
      other_count_vars = "total"
    ) |>
      obfuscate_data(
        total_var = paste0(median_var, "_median"),
        statistics_vars = paste0(median_var, "_median"),
        censored_value = censored_value
      )
  }

  return(out)
}

#' Summarise count into wide format
#'
#' Summarise a vector into columns of counts for each occurrence and each counts
#' associated proportion
#'
#' @param x A vector, numeric or character
#' @param include_missing logical indicating whether or not to include
#' NA values
#' @param obfuscate_data logical indicating whether or not to obfuscate data
#' @param censored_value the value to replace censored proportions with

count_prop_wide <- function(x, include_missing = FALSE, obfuscate_data, censored_value) {
  checkmate::assert_logical(include_missing, len = 1, any.missing = FALSE)
  checkmate::assert_vector(x)
  checkmate::assert(
    checkmate::check_numeric(x),
    checkmate::check_character(x),
    combine = "or"
  )
  checkmate::assert_logical(obfuscate_data, len = 1, any.missing = FALSE)

  use_na <- ifelse(include_missing, "ifany", "no")

  tbl <- table(x, useNA = use_na)

  if (nrow(tbl) == 0) {
    return(NA)
  }

  res <- data.frame(tbl) |>
    dplyr::rename(
      n = "Freq"
    ) |>
    dplyr::mutate(
      total = sum(.data[["n"]]),
      prop = .data[["n"]] / .data[["total"]]
    )

  if (obfuscate_data) {
    res <- RCStat::obfuscate_data(
      data = res,
      total_var = "total",
      count_var = "n",
      prop_var = "prop",
      censored_value = censored_value,
      liberal_obfuscation = TRUE
    )
  }

  res <- res |>
    tidyr::pivot_wider(
      names_from = "x",
      values_from = c("n", "prop")
    )

  if (include_missing) {
    res <- dplyr::select(res, -tidyselect::all_of("total"))
  }


  return(res)
}
