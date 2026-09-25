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
group_proportions <- function(data,
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
group_means <- function(data,
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
proportion_missing <- function(data,
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
#' @param censored_value What value to replace censored values, used as
#' argument in obfuscate_data
#' @param pivot_prop_count whether to pivot the resulting data.frame into
#' long format
#' @param distinct_cols a set of columns that are used in
#' [dplyr::distinct()]. Should be disjoint from `group_cols` to prevent
#' misinterpretation of results.
#' @param arrange_by a column used in [dplyr::arrange()] before calling
#' [dplyr::distinct()] with the variables in `distinct_cols`. The variable is
#' arranged in descending order.
#' @param marginal_cols a subset of `group_cols` for which to marginally
#' summarize. If not supplied all grouping columns will be used. Use `NULL`
#' to not add any marginals.
#' @param add_reason_col whether or not to add a variable describing why an
#' observation was obfuscate, passed to [obfuscate_data()]
#' @param ci whether to add a Wilson score confidence interval
#' (`{var}_ci_lower_{level}`/`{var}_ci_upper_{level}`, where `{level}` is
#' the confidence level as a percentage, e.g. `alpha = 0.05` produces
#' `_ci_lower_95`/`_ci_upper_95`) for `prop` variables in `vars`. The level
#' is baked into the column name so consumers (e.g. [prettify_table()])
#' can read it back out without `alpha` being threaded through separately.
#' Not supported for `mean`, `median`, `prop_count`, or `count`.
#' @param alpha alpha level used for the confidence interval when `ci` is
#' `TRUE`
#'
#' @export get_aggregate_value
get_aggregate_value <- function(df,
                                group_cols = NULL,
                                vars = NULL,
                                include_missing = TRUE,
                                obfuscate_data = FALSE,
                                censored_value = 0,
                                pivot_prop_count = FALSE,
                                distinct_cols = NULL,
                                arrange_by = NULL,
                                marginal_cols,
                                add_reason_col = FALSE,
                                ci = FALSE,
                                alpha = 0.05) {
  #### Warnings ####

  checkmate::assert_list(vars, min.len = 1)
  checkmate::assert_logical(include_missing, len = 1, any.missing = FALSE)
  checkmate::assert_logical(obfuscate_data, len = 1, any.missing = FALSE)
  checkmate::assert_logical(ci, len = 1, any.missing = FALSE)
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  checkmate::assert_subset(group_cols, names(df))

  checkmate::assert_subset(
    names(vars),
    c("prop", "mean", "median", "prop_count", "count"),
    empty.ok = FALSE
  )

  if (!setequal("count", names(vars))) {
    checkmate::assert_subset(
      vars |>
        purrr::discard_at("count") |>
        unlist(),
      names(df),
      empty.ok = FALSE
    )
  }

  checkmate::assert_subset(
    purrr::pluck(vars, "count"),
    names(df),
    empty.ok = TRUE
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
    rlang::warn(
      "You are trying to aggregate a data.frame that contains 0 rows"
    )
  }

  prop_var <- vars[["prop"]]
  mean_var <- vars[["mean"]]
  median_var <- vars[["median"]]
  prop_count_var <- vars[["prop_count"]]
  count_var <- vars[["count"]]

  if ("count" %in% names(vars) && length(vars) > 1) {
    stop(
      "count can not be supplied to vars with any other summarizing function"
    )
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

  # Compute prop_count aggregates for every combination up front, from a
  # single finest-grain count rolled up by summation, instead of
  # re-tabulating raw data once per combination (see rollup_prop_count()).
  prop_count_results <- NULL
  if (!is.null(prop_count_var)) {
    checkmate::assert_disjunct(
      group_cols,
      c("n", "category", "total", "prop"),
      .var.name = "group_cols"
    )
    prop_count_results <- rollup_prop_count(
      df = df,
      group_cols = group_cols,
      group_var_combinations = group_var_combinations,
      prop_count_var = prop_count_var,
      include_missing = include_missing,
      obfuscate_data = obfuscate_data,
      censored_value = censored_value,
      add_reason_col = add_reason_col
    )
  }


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

  if (ci) {
    ci_level <- ci_level_suffix(alpha)
    ci_lower_name <- paste0("ci_lower_", ci_level)
    ci_upper_name <- paste0("ci_upper_", ci_level)

    prop_list[[ci_lower_name]] <- function(x) {
      wilson_ci_p(
        sum(x, na.rm = TRUE) / dplyr::n(), dplyr::n(), alpha = alpha
      )$lower
    }
    prop_list[[ci_upper_name]] <- function(x) {
      wilson_ci_p(
        sum(x, na.rm = TRUE) / dplyr::n(), dplyr::n(), alpha = alpha
      )$upper
    }

    prop_missing_list[[ci_lower_name]] <- function(x) {
      n_non_missing <- sum(!is.na(x))
      wilson_ci_p(
        sum(x, na.rm = TRUE) / n_non_missing, n_non_missing, alpha = alpha
      )$lower
    }
    prop_missing_list[[ci_upper_name]] <- function(x) {
      n_non_missing <- sum(!is.na(x))
      wilson_ci_p(
        sum(x, na.rm = TRUE) / n_non_missing, n_non_missing, alpha = alpha
      )$upper
    }
  }

  mean_list <- list(
    mean = function(x) mean(x, na.rm = TRUE),
    std = function(x) stats::sd(x, na.rm = TRUE)
  )


  median_list <- list(
    median = function(x) stats::median(x, na.rm = TRUE),
    quant_5 = function(x) {
      stats::quantile(
        x,
        probs = 0.05,
        na.rm = TRUE,
        names = FALSE
      )
    },
    quant_25 = function(x) {
      stats::quantile(
        x,
        probs = 0.25,
        na.rm = TRUE,
        names = FALSE
      )
    },
    quant_75 = function(x) {
      stats::quantile(
        x,
        probs = 0.75,
        na.rm = TRUE,
        names = FALSE
      )
    },
    quant_95 = function(x) {
      stats::quantile(
        x,
        probs = 0.95,
        na.rm = TRUE,
        names = FALSE
      )
    }
  )


  if (include_missing) {
    prop_fns <- prop_list
  } else {
    mean_list$total_non_missing <- function(x) sum(!is.na(x))
    median_list$total_non_missing <- function(x) sum(!is.na(x))
    prop_fns <- prop_missing_list
  }



  # Perform the calculation
  for (i in seq_along(group_var_combinations)) {
    comb <- group_var_combinations[[i]]
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
        total = dplyr::n(),
        .groups = "drop"
      )

    if (!is.null(prop_count_results)) {
      temp <- if (length(comb) == 0) {
        dplyr::bind_cols(temp, prop_count_results[[i]])
      } else {
        dplyr::left_join(temp, prop_count_results[[i]], by = comb)
      }
      # Match the old count_prop_wide-in-summarise() column order, where
      # `total = dplyr::n()` was always the last computed column.
      temp <- dplyr::relocate(
        temp, dplyr::all_of("total"), .after = dplyr::last_col()
      )
    }


    for (cols in all_cols) {
      # Change County and Unit variables to contain Riket not Alla
      if (grepl("(County|Unit(Code)?)$", cols)) {
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
            statistics_vars = if (ci) {
              paste0(var, "_", c(ci_lower_name, ci_upper_name))
            } else {
              NULL
            },
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
            statistics_vars = if (ci) {
              paste0(var, "_", c(ci_lower_name, ci_upper_name))
            } else {
              NULL
            },
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

    } else if (
      "prop_count" %in% names(vars) && length(vars[["prop_count"]]) > 1
    ) {

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

#' Build a finest-grain, long-format count table for one prop_count variable
#'
#' @param df the data.frame, already coerced so `var` is a factor
#' @param group_cols all columns get_aggregate_value() groups by
#' @param var the single prop_count column to tabulate
#' @param include_missing whether NA values of `var` count as a category
#'
#' @return a long-format tibble with `group_cols`, `category`, and `n`
build_finest_prop_count <- function(df, group_cols, var, include_missing) {
  category_levels <- levels(df[[var]])
  is_na_val <- is.na(df[[var]])
  add_na_category <- include_missing && any(is_na_val)

  raw <- df |>
    dplyr::mutate(
      category = if (add_na_category) {
        dplyr::if_else(is_na_val, "NA", as.character(.data[[var]]))
      } else {
        as.character(.data[[var]])
      }
    ) |>
    dplyr::select(dplyr::all_of(group_cols), "category")

  if (!include_missing) {
    raw <- raw[!is_na_val, , drop = FALSE]
  }

  full_category_levels <- if (add_na_category) {
    c(category_levels, "NA")
  } else {
    category_levels
  }

  group_tuples <- df |>
    dplyr::distinct(dplyr::across(dplyr::all_of(group_cols)))

  counts <- raw |>
    dplyr::count(
      dplyr::across(dplyr::all_of(group_cols)), .data[["category"]], name = "n"
    )

  finest <- group_tuples |>
    tidyr::crossing(category = full_category_levels) |>
    dplyr::left_join(counts, by = c(group_cols, "category")) |>
    dplyr::mutate(
      n = dplyr::coalesce(.data[["n"]], 0L),
      # Factor keeps category in full_category_levels order (NA last) through
      # group_by()/summarise() downstream, instead of locale-dependent sorting.
      category = factor(.data[["category"]], levels = full_category_levels)
    )

  if (!add_na_category) {
    return(finest)
  }

  # A tuple has NA iff counts (already aggregated from raw) has an "NA"
  # category row for it, so this doesn't need its own pass over df.
  has_na_by_tuple <- counts |>
    dplyr::filter(.data[["category"]] == "NA") |>
    dplyr::select(dplyr::all_of(group_cols)) |>
    dplyr::mutate(.has_na = TRUE)

  finest |>
    dplyr::left_join(has_na_by_tuple, by = group_cols) |>
    dplyr::mutate(
      .has_na = dplyr::coalesce(.data[[".has_na"]], FALSE),
      n = dplyr::if_else(
        .data[["category"]] == "NA" & !.data[[".has_na"]],
        NA,
        .data[["n"]]
      )
    ) |>
    dplyr::select(-".has_na")
}

#' Roll finest-grain prop_count counts up to one group_cols combination
#'
#' @param finest_counts output of [build_finest_prop_count()]
#' @param comb the subset of columns to group by for this combination
#' @param var_name the prop_count column name, used to prefix output columns
#' @param include_missing whether NA values of the tabulated var count as
#' a category
#' @param obfuscate_data whether counts should be obfuscated
#' @param censored_value the value to replace censored counts with
#' @param add_reason_col whether to add an obfuscation-reason column
#'
#' @return a wide tibble keyed by `comb`'s columns
rollup_prop_count_one <- function(finest_counts,
                                  comb,
                                  var_name,
                                  include_missing,
                                  obfuscate_data,
                                  censored_value,
                                  add_reason_col) {

  comb_counts <- finest_counts |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(comb, "category")))) |>
    dplyr::summarise(
      n = if (any(!is.na(.data[["n"]]))) {
        sum(.data[["n"]], na.rm = TRUE)
      } else {
        NA
      },
      .groups = "drop_last"
    ) |>
    dplyr::mutate(
      total = sum(.data[["n"]], na.rm = TRUE),
      prop = .data[["n"]] / .data[["total"]]
    ) |>
    dplyr::ungroup()

  if (length(comb) == 0 && nrow(comb_counts) == 0) {
    category_levels <- levels(finest_counts[["category"]])
    comb_counts <- tibble::tibble(
      category = factor(category_levels, levels = category_levels),
      n = 0,
      total = 0,
      prop = NaN
    )
  }

  if (obfuscate_data) {
    comb_counts <- comb_counts |>
      obfuscate_data(
        total_var = "total",
        count_var = "n",
        prop_var = "prop",
        censored_value = censored_value,
        liberal_obfuscation = TRUE,
        group_var = comb,
        add_reason_col = add_reason_col
      ) |>
      dplyr::ungroup()
  }

  if (include_missing) {
    comb_counts <- dplyr::select(comb_counts, -dplyr::all_of("total"))
    extra_id_col <- NULL
  } else {
    comb_counts <- dplyr::rename(comb_counts, total_non_missing = "total")
    extra_id_col <- "total_non_missing"
  }

  wide <- comb_counts |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(c(comb, extra_id_col)),
      names_from = "category",
      values_from = dplyr::any_of(c("n", "prop", "obfuscated_reason"))
    )

  dplyr::rename_with(
    wide,
    \(x) if (length(x) == 0) x else paste0(var_name, "_", x),
    .cols = -dplyr::all_of(comb)
  )
}

#' Produce a proper tabulation when the `var_name` of interest is degenerate,
#' i.e. contains no relevant rows, either truly containing 0 rows or having
#' only NA values when `include_missing` is `FALSE`
#'
#' @param df the data.frame
#' @param comb the subset of columns to key rows by for this combination
#' @param var_name the prop_count column name, used as-is (no prefix)
#'
#' @return a tibble with one column named `var_name` (all `NA`), one row
#' per distinct `comb` tuple present in `df` (or exactly one row if `comb`
#' is empty, matching [dplyr::group_by()]'s zero-columns convention)
degenerate_prop_count_wide <- function(df, comb, var_name) {
  if (length(comb) == 0) {
    tibble::tibble(!!var_name := NA)
  } else {
    df |>
      dplyr::distinct(dplyr::across(dplyr::all_of(comb))) |>
      dplyr::mutate(!!var_name := NA)
  }
}

#' Compute prop_count aggregates for every group_cols combination
#'
#' @param df the data.frame, already coerced so `prop_count_var` columns
#' are factors
#' @param group_cols all columns get_aggregate_value() groups by
#' @param group_var_combinations list of column-subsets, as returned by
#' [get_group_combinations()]
#' @param prop_count_var one or more column names to tabulate
#' @param include_missing whether NA values of `prop_count_var` count as
#' a category
#' @param obfuscate_data whether counts should be obfuscated
#' @param censored_value the value to replace censored counts with
#' @param add_reason_col whether to add an obfuscation-reason column
#' @return a list, same length/order as `group_var_combinations`, of wide
#' tibbles keyed by that combination's columns
rollup_prop_count <- function(df,
                              group_cols,
                              group_var_combinations,
                              prop_count_var,
                              include_missing = TRUE,
                              obfuscate_data = FALSE,
                              censored_value = 0,
                              add_reason_col = FALSE) {

  is_degenerate <- purrr::map_lgl(
    prop_count_var,
    \(var) {
      no_levels <- length(levels(df[[var]])) == 0
      no_na_fallback <- !(include_missing && any(is.na(df[[var]])))
      no_levels && no_na_fallback
    }
  )
  names(is_degenerate) <- prop_count_var

  finest_by_var <- purrr::map(
    prop_count_var[!is_degenerate],
    \(var) build_finest_prop_count(df, group_cols, var, include_missing)
  )
  names(finest_by_var) <- prop_count_var[!is_degenerate]

  purrr::map(
    group_var_combinations,
    \(comb) {
      wide_by_var <- purrr::map(
        prop_count_var,
        \(var) {
          if (is_degenerate[[var]]) {
            degenerate_prop_count_wide(df, comb, var)
          } else {
            rollup_prop_count_one(
              finest_by_var[[var]],
              comb = comb,
              var_name = var,
              include_missing = include_missing,
              obfuscate_data = obfuscate_data,
              censored_value = censored_value,
              add_reason_col = add_reason_col
            )
          }
        }
      )
      purrr::reduce(
        wide_by_var,
        \(a, b) {
          if (length(comb) == 0) {
            dplyr::bind_cols(a, b)
          } else {
            dplyr::full_join(a, b, by = comb)
          }
        }
      )
    }
  )
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
    group_var_combinations <- purrr::list_flatten(
      purrr::map(
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
    purrr::map(
      group_var_combinations,
      \(x) unique(c(always_group_by, x))
    )
  )

  return(group_var_combinations)

}
