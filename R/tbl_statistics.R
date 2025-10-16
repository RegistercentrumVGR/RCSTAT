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
#' @param pivot_prop_count whether to pivot the resulting data.frame into long format
#' @param distinct_cols a set of columns that are used in
#' [dplyr::distinct_by()]. Should be disjoint from `group_cols` to prevent
#' misinterpretation of results.
#' @param arrange_by a column used in [dplyr::arrange_by()] before calling
#' [dplyr::distinct()] with the variables in `distinct_cols`. The variable is
#' arranged in descending order.
#' @param marginal_cols a subset of `group_cols` for which to marginally
#' summarize. If not supplied all grouping columns will be used. Use `NULL`
#' to not add any marginals.
#' @param add_reason_col whether or not to add a variable describing why an
#' observation was obfuscate, passed to [obfuscate_data()]
#'
#' @export get_aggregate_value

get_aggregate_value <- function(
    df,
    group_cols = NULL,
    vars = NULL,
    include_missing = TRUE,
    obfuscate_data = FALSE,
    censored_value = 0,
    pivot_prop_count = FALSE,
    distinct_cols = NULL,
    arrange_by = NULL,
    marginal_cols,
    add_reason_col = FALSE) {
  #### Warnings ####

  checkmate::assert_list(vars, min.len = 1, any.missing = FALSE)
  checkmate::assert_logical(include_missing, len = 1, any.missing = FALSE)
  checkmate::assert_logical(obfuscate_data, len = 1, any.missing = FALSE)
  checkmate::assert_subset(group_cols, names(df))
  checkmate::assert_subset(unlist(vars), names(df), empty.ok = FALSE)
  checkmate::assert_subset(
    names(vars),
    c("prop", "mean", "median", "prop_count", "count"),
    empty.ok = FALSE
  )
  checkmate::assert_logical(pivot_prop_count, len = 1, any.missing = FALSE)
  checkmate::assert(
    checkmate::check_null(distinct_cols),
    checkmate::check_true(
      checkmate::test_disjunct(
        distinct_cols,
        group_cols
      ) && checkmate::test_subset(
        distinct_cols,
        names(df)
      )
    )
  )
  checkmate::assert_logical(add_reason_col, len = 1, any.missing = FALSE)


  if (!is.null(distinct_cols)) {
    checkmate::assert_choice(arrange_by, names(df))
  }

  id_vars <- c(
    "SubjectKey",
    "SubjectID",
    "lopnr",
    "LopNr"
  )

  if (!is.null(distinct_cols)) {
    if (!any(id_vars %in% distinct_cols | grepl("^EventID", distinct_cols))) {
      rlang::warn("No unique subject identifier supplied,
                  this is probably a mistake")
    }
  }

  if (nrow(df) == 0) {
    rlang::warn("You are trying to aggregate a data.frame that contains 0 rows")
  }

  prop_var <- vars[["prop"]]
  mean_var <- vars[["mean"]]
  median_var <- vars[["median"]]
  prop_count_var <- vars[["prop_count"]]
  count_var <- vars[["count"]]

  if (!is.null(count_var) && !setequal(count_var, vars)) {
    stop("count can not be supplied to vars with any other summarizing function")
  }

  group_cols <- c(count_var, group_cols)

  if (rlang::is_missing(marginal_cols)) {
    marginal_cols <- group_cols
  } else {
    checkmate::assert_subset(marginal_cols, group_cols)
  }

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
        tidyselect::all_of(marginal_cols), as.character
      ),
      dplyr::across(
        dplyr::all_of(prop_count_var) & dplyr::where(~ !is.factor(.x)),
        as.factor
      )
    )

  # Return data frame
  out <- data.frame()

  # Get all the combinations of the variables
  group_var_combinations <- get_group_combinations(group_cols, marginal_cols)


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
    std = function(x) stats::sd(x, na.rm = TRUE)
  )


  median_list <- list(
    median = function(x) stats::median(x, na.rm = TRUE),
    quant_5 = function(x) stats::quantile(x, probs = 0.05, na.rm = TRUE),
    quant_25 = function(x) stats::quantile(x, probs = 0.25, na.rm = TRUE),
    quant_75 = function(x) stats::quantile(x, probs = 0.75, na.rm = TRUE),
    quant_95 = function(x) stats::quantile(x, probs = 0.95, na.rm = TRUE)
  )


  if (include_missing) {
    prop_fns <- prop_list
  } else {
    mean_list$total_non_missing <- function(x) sum(!is.na(x))
    median_list$total_non_missing <- function(x) sum(!is.na(x))
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
      )

    if (!is.null(distinct_cols)) {
      temp <- temp |>
        dplyr::arrange(dplyr::desc(.data[[arrange_by]])) |>
        dplyr::distinct(
          dplyr::across(dplyr::all_of(distinct_cols)),
          .keep_all = TRUE
        )
    }

    temp <- temp |>
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
            censored_value = censored_value,
            add_reason_col = add_reason_col
          ),
          .unpack = TRUE
        ),
        total = dplyr::n(),
        .groups = "drop"
      )


    for (cols in all_cols) {
      # Change County and Unit variables to contain Riket not Alla
      if (grepl("County|Unit", cols)) {
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
  if (obfuscate_data) {
    if (include_missing) {
      for (var in prop_var) {
        out <- out |>
          obfuscate_data(
            total_var = "total",
            count_var = paste0(var, "_n"),
            prop_var = paste0(var, "_prop"),
            censored_value = censored_value,
            add_reason_col = add_reason_col
          )
      }

      for (var in median_var) {
        out <- obfuscate_data(
          data = out,
          total_var = "total",
          statistics_vars = paste0(
            var,
            c(
              "_median",
              "_quant_5",
              "_quant_25",
              "_quant_75",
              "_quant_95"
            )
          ),
          censored_value = censored_value,
          add_reason_col = add_reason_col
        )
      }

      for (var in mean_var) {
        out <- obfuscate_data(
          data = out,
          total_var = "total",
          statistics_vars = c(
            paste0(var, "_mean"),
            paste0(var, "_std")
          ),
          censored_value = censored_value,
          add_reason_col = add_reason_col
        )
      }

      for (var in prop_count_var) {
        out <- obfuscate_data(
          data = out,
          total_var = "total"
        )
      }

    } else if (!include_missing) {
      # Obfuscate the data with missing not included.
      for (var in prop_var) {
        out <- out |>
          obfuscate_data(
            total_var = paste0(var, "_total_non_missing"),
            count_var = paste0(var, "_n"),
            prop_var = paste0(var, "_prop"),
            censored_value = censored_value,
            other_count_vars = c("total", paste0(var, "_total_missing")),
            add_reason_col = add_reason_col
          )
      }

      for (var in median_var) {
        out <- obfuscate_data(
          data = out,
          total_var = paste0(var, "_total_non_missing"),
          statistics_vars = paste0(
            var,
            c(
              "_median",
              "_quant_5",
              "_quant_25",
              "_quant_75",
              "_quant_95"
            )
          ),
          censored_value = censored_value,
          other_count_vars = "total",
          add_reason_col = add_reason_col
        )
      }

      for (var in mean_var) {
        out <- obfuscate_data(
          data = out,
          total_var = paste0(var, "_total_non_missing"),
          statistics_vars = c(
            paste0(var, "_mean"),
            paste0(var, "_std")
          ),
          censored_value = censored_value,
          other_count_vars = "total",
          add_reason_col = add_reason_col
        )
      }

      for (var in prop_count_var) {
        out <- obfuscate_data(
          data = out,
          total_var = "total_non_missing",
          other_count_vars = "total"
        )
      }

    }
    out <- out |>
      obfuscate_data(total_var = "total")
  }

  if (pivot_prop_count) {

    if (length(vars) > 1) {

      warning(
        paste0("pivot_prop_count is not supported when multiple",
               " aggregation variables are specified")
      )

    } else if ("prop_count" %in% names(vars) && length(vars[["prop_count"]]) > 1) {

      warning(
        paste0("pivot_prop_count is not supported when multiple",
               " variables are specified for prop_count")
      )

    } else {

      out <- pivot_prop_count(out, category_name = vars[["prop_count"]])

    }

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
#' @param add_reason_col whether or not to add a variable describing why an
#' observation was obfuscate, passed to [obfuscate_data()]
count_prop_wide <- function(x,
                            include_missing = FALSE,
                            obfuscate_data,
                            censored_value,
                            add_reason_col) {
  checkmate::assert_logical(include_missing, len = 1, any.missing = FALSE)
  checkmate::assert_vector(x)
  checkmate::assert(
    checkmate::check_numeric(x),
    checkmate::check_character(x),
    checkmate::check_factor(x),
    combine = "or"
  )
  checkmate::assert_logical(obfuscate_data, len = 1, any.missing = FALSE)
  checkmate::assert_logical(add_reason_col, len = 1, any.missing = FALSE)

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
    res <- res |>
      dplyr::mutate(dummy = "a") |>
      obfuscate_data(
        total_var = "total",
        count_var = "n",
        prop_var = "prop",
        censored_value = censored_value,
        liberal_obfuscation = TRUE,
        group_var = "dummy",
        add_reason_col = add_reason_col
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-"dummy")
  }

  res <- res |>
    tidyr::pivot_wider(
      names_from = "x",
      values_from = dplyr::any_of(c("n", "prop", "obfuscated_reason"))
    )

  if (include_missing) {
    res <- dplyr::select(res, -tidyselect::all_of("total"))
  } else {
    res <- dplyr::rename(res, "total_non_missing" = "total")
  }


  return(res)
}

#' Pivots the result of a single prop_count from [get_aggregate_value()]
#'
#' @param df the data.frame to plot
#' @param category_name the name of the variable to store the outcomes of the
#' variable in
#'
#' @return a data.frame pivoted into long format
pivot_prop_count <- function(df, category_name = "kategori") {

  if (!any(grepl("_(n|prop|obfuscated_reason)_", names(df)))) {
    return(df)
  }

  df |>
    tidyr::pivot_longer(
      cols = dplyr::matches(".+_(n|prop|obfuscated_reason)_"),
      names_pattern = "(.+_(?:n|prop|obfuscated_reason))_(.+)",
      names_to = c(".value", category_name)
    )
}

#' Get a list of all combinations of columns defined in `group_cols`
#'
#' Also finds which columns to always group by and adds these to the
#' combinations.
#'
#' @param group_cols the columns to group by
#' @param marginal_cols the columns for which to create marginal summaries
#'
#' @return a list of combinations
get_group_combinations <- function(group_cols, marginal_cols) {

  if (!is.null(group_cols)) {
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
  } else {
    group_var_combinations <- list(character(0))
  }

  always_group_by <- setdiff(group_cols, marginal_cols)
  group_var_combinations <- unique(
    lapply(
      group_var_combinations,
      \(x) unique(c(always_group_by, x))
    )
  )

  return(group_var_combinations)

}
