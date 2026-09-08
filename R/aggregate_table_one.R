#' Create a "table one" style summary table
#'
#' Summarizes a set of variables by type (binary, categorical, or numeric)
#' and pivots the result into a wide, printable table with one column per
#' `group_cols` combination (mirroring [get_aggregate_value()], including
#' its marginal "Alla"/"Riket" totals).
#' Variables are classified automatically:
#' * `logical` columns, and numeric columns only containing `0`/`1` (and
#' `NA`), are treated as binary and summarized as N (%) on a single row.
#' * Remaining `character`/`factor` columns are treated as categorical and
#' summarized as N (%) with one row per category. Categories are lumped
#' with [forcats::fct_lump_n()] when there are more than `max_categories`
#' of them.
#' * Remaining numeric columns are treated as continuous and summarized as
#' mean (SD), unless listed in `median_vars`, in which case they are
#' summarized as median (5%-95%).
#'
#' @param df a data.frame
#' @param vars the variables to summarize
#' @param median_vars a subset of the numeric variables in `vars` that
#' should be summarized as median (5%-95%) instead of mean (SD)
#' @param include_missing whether or not missing values should be included
#' when calculating proportions, passed to [get_aggregate_value()]
#' @param max_categories the maximum number of categories a categorical
#' variable can have before the smallest categories are lumped into
#' `"Other"`
#' @param digits the number of decimals used when formatting statistics
#' @param obfuscate_data whether or not to obfuscate data
#' @param group_cols the columns to group by
#' @param marginal_cols a subset of `group_cols` for which to marginally
#' summarize, passed to [get_aggregate_value()]
#' @param value_labels an optional data.frame with `ColumnName`, `ValueCode`,
#' and `ValueName` columns (the same shape used by [decode_data()]), used to
#' decode categorical variables in `vars` before summarizing. Classification
#' of variables into binary/categorical/numeric always runs on the raw,
#' undecoded data first, so decoding a binary variable's value labels never
#' changes its classification. If `NULL` (the default), categorical values
#' are left undecoded
#'
#' @return a data.frame with one row per variable/category and one column
#' per `group_cols` combination. `category` is `" "` (a single whitespace)
#' for rows that are not categorical, so the table renders without `NA`
#' @export
table_one <- function(df,
                      vars,
                      median_vars = NULL,
                      include_missing,
                      max_categories = 15,
                      digits = 1,
                      obfuscate_data = TRUE,
                      group_cols = NULL,
                      marginal_cols,
                      value_labels = NULL) {

  checkmate::assert_data_frame(df)
  checkmate::assert_character(vars, min.len = 1)
  checkmate::assert_subset(vars, names(df))
  checkmate::assert_character(median_vars, null.ok = TRUE)
  checkmate::assert_subset(median_vars, vars)
  checkmate::assert_logical(include_missing, len = 1, any.missing = FALSE)
  checkmate::assert_int(max_categories, lower = 2)
  checkmate::assert_int(digits, lower = 0)
  checkmate::assert_logical(obfuscate_data, len = 1, any.missing = FALSE)
  checkmate::assert_character(group_cols, null.ok = TRUE)
  checkmate::assert_subset(group_cols, names(df))
  checkmate::assert_disjunct(vars, group_cols)
  checkmate::assert_data_frame(value_labels, null.ok = TRUE)
  if (!is.null(value_labels)) {
    checkmate::assert_subset(c("ColumnName", "ValueCode", "ValueName"), names(value_labels))
  }

  var_types <- classify_table_one_vars(df, vars)

  binary_vars <- vars[var_types == "binary"]
  categorical_vars <- vars[var_types == "categorical"]
  numeric_vars <- vars[var_types == "numeric"]

  checkmate::assert_subset(median_vars, numeric_vars)
  mean_vars <- setdiff(numeric_vars, median_vars)

  if (!is.null(value_labels) && length(categorical_vars) > 0) {
    df <- df |>
      decode_data(
        labels = value_labels[value_labels$ColumnName %in% categorical_vars, ],
        missing_labels_na = FALSE
      )
  }

  df <- df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(categorical_vars),
        ~ lump_table_one_categories(.x, max_categories, dplyr::cur_column())
      )
    )

  vars_list <- list(
    prop = binary_vars,
    prop_count = categorical_vars,
    mean = mean_vars,
    median = median_vars
  )
  vars_list <- vars_list[lengths(vars_list) > 0]

  res <- get_aggregate_value(
    df = df,
    group_cols = group_cols,
    vars = vars_list,
    include_missing = include_missing,
    obfuscate_data = obfuscate_data,
    marginal_cols = marginal_cols
  )

  out <- dplyr::bind_rows(
    extract_table_one_n(res, group_cols),
    extract_table_one_prop(res, binary_vars, group_cols, digits),
    extract_table_one_prop_count(res, categorical_vars, group_cols, digits),
    extract_table_one_mean(res, mean_vars, group_cols, digits),
    extract_table_one_median(res, median_vars, group_cols, digits)
  )

  if (length(group_cols) > 0) {
    out <- tidyr::pivot_wider(
      out,
      names_from = dplyr::all_of(group_cols),
      values_from = "value"
    )
  }

  out
}

#' Prettify the result of [table_one()]
#'
#' Renames `variable`/`category`/`value` to `"Variabel"`/`"Utfall"`/
#' `"Värde"`, optionally recodes variable identifiers into
#' human-readable labels, and blanks `"Variabel"` on rows where it repeats
#' the value directly above it, so a categorical variable's name is only
#' printed once instead of on every category row.
#'
#' @param df the result of [table_one()]
#' @param vars a data.frame with `ColumnName` and `Description` columns
#' (the same shape used by [decode_names()]) used to recode `variable` into
#' human-readable labels with [dplyr::case_match()]. Variables not found in
#' `vars` are left unchanged
#' @param blank_repeated whether or not to blank `"Variabel"` on rows
#' where it repeats the value directly above it
#'
#' @return a data.frame
#' @export
prettify_table_one <- function(df, vars = NULL, blank_repeated = TRUE) {

  checkmate::assert_data_frame(df)
  checkmate::assert_subset(c("variable", "category"), names(df))
  checkmate::assert_data_frame(vars, null.ok = TRUE)
  checkmate::assert_logical(blank_repeated, len = 1, any.missing = FALSE)

  if (!is.null(vars)) {
    checkmate::assert_subset(c("ColumnName", "Description"), names(vars))
    var_labels <- stats::setNames(vars$Description, vars$ColumnName)

    df <- df |>
      dplyr::mutate(
        variable = dplyr::case_match(
          .data$variable,
          !!!purrr::imap(var_labels, ~ rlang::expr(!!.y ~ !!.x)),
          .default = .data$variable
        )
      )
  }

  if (blank_repeated) {
    df <- df |>
      dplyr::mutate(
        variable = dplyr::if_else(
          .data$variable == dplyr::lag(.data$variable) &
            !is.na(dplyr::lag(.data$variable)),
          "",
          .data$variable
        )
      )
  }

  df |>
    dplyr::rename(
      Variabel = "variable",
      Utfall = "category",
      !!"V\u00e4rde" := dplyr::any_of("value")
    )
}

#' Classify `vars` into binary, categorical, or numeric
#'
#' @param df a data.frame
#' @param vars the variables to classify
#'
#' @return a character vector of the same length as `vars`, with one of
#' `"binary"`, `"categorical"`, or `"numeric"` for each entry
classify_table_one_vars <- function(df, vars) {
  purrr::map_chr(
    vars,
    function(v) {
      x <- df[[v]]
      if (is_table_one_binary(x)) {
        "binary"
      } else if (is.numeric(x)) {
        "numeric"
      } else if (is.character(x) || is.factor(x)) {
        "categorical"
      } else {
        cli::cli_abort(
          "{.field {v}} has unsupported class {.cls {class(x)}}"
        )
      }
    }
  )
}

#' Check if a vector should be treated as binary
#'
#' `logical` vectors, and numeric vectors only containing `0`/`1` (and
#' `NA`), are treated as binary.
#'
#' @param x a vector
#'
#' @return a single logical
is_table_one_binary <- function(x) {
  if (is.logical(x)) {
    return(TRUE)
  }
  if (!is.numeric(x)) {
    return(FALSE)
  }
  x_obs <- x[!is.na(x)]
  length(x_obs) > 0 && all(x_obs %in% c(0, 1))
}

#' Lump the smallest categories of a categorical variable into `"Other"`
#'
#' @param x a vector
#' @param max_categories the maximum number of categories to keep
#' @param name the name of the variable, used in the warning message
#' @param other_level value of the level used for "other" values, passed to
#' [forcats::fct_lump_n()]
#'
#' @return `x` as a factor, with the smallest categories lumped into
#' `"Other"` if `x` has more than `max_categories` categories
lump_table_one_categories <- function(x,
                                      max_categories,
                                      name,
                                      other_level = "Other") {

  x <- if (is.factor(x)) x else factor(x)

  if (nlevels(x) > max_categories) {
    cli::cli_alert_warning(
      paste0(
        "{.field {name}} has {.val {nlevels(x)}} categories, lumping the ",
        "smallest into {.val {'Other'}} to keep at most ",
        "{.val {max_categories}}"
      )
    )
    x <- forcats::fct_lump_n(
      x,
      n = max_categories - 1,
      other_level = other_level
    )
  }

  x
}

#' Extract the group sizes from the result of [get_aggregate_value()]
#'
#' @param res the result of [get_aggregate_value()]
#' @param group_cols the columns to group by
#'
#' @return a long data.frame with one row (`variable = "N"`) per
#' `group_cols` combination
extract_table_one_n <- function(res, group_cols) {
  res |>
    dplyr::select(dplyr::all_of(group_cols), value = "total") |>
    dplyr::distinct() |>
    dplyr::mutate(
      variable = "N",
      category = " ",
      value = as.character(.data$value)
    ) |>
    dplyr::select(dplyr::all_of(group_cols), "variable", "category", "value")
}

#' Extract binary variables from the result of [get_aggregate_value()]
#'
#' @param res the result of [get_aggregate_value()]
#' @param binary_vars the binary variables to extract
#' @param group_cols the columns to group by
#' @param digits the number of decimals used when formatting statistics
#'
#' @return a long data.frame with one row per variable
extract_table_one_prop <- function(res, binary_vars, group_cols, digits) {
  purrr::map(
    binary_vars,
    function(v) {
      res |>
        dplyr::select(
          dplyr::all_of(group_cols),
          n = paste0(v, "_n"),
          prop = paste0(v, "_prop")
        ) |>
        dplyr::mutate(
          variable = v,
          category = " ",
          value = format_table_one_prop(.data$n, .data$prop, digits)
        ) |>
        dplyr::select(
          dplyr::all_of(group_cols), "variable", "category", "value"
        )
    }
  ) |>
    dplyr::bind_rows()
}

#' Extract categorical variables from the result of [get_aggregate_value()]
#'
#' @param res the result of [get_aggregate_value()]
#' @param categorical_vars the categorical variables to extract
#' @param group_cols the columns to group by
#' @param digits the number of decimals used when formatting statistics
#'
#' @return a long data.frame with one row per variable/category
extract_table_one_prop_count <- function(res,
                                         categorical_vars,
                                         group_cols,
                                         digits) {
  purrr::map(
    categorical_vars,
    function(v) {
      res |>
        dplyr::select(
          dplyr::all_of(group_cols),
          dplyr::matches(paste0("^", v, "_(n|prop)_"))
        ) |>
        tidyr::pivot_longer(
          cols = -dplyr::all_of(group_cols),
          names_to = c(".value", "category"),
          names_pattern = paste0("^", v, "_(n|prop)_(.+)$")
        ) |>
        dplyr::mutate(
          variable = v,
          value = format_table_one_prop(.data$n, .data$prop, digits)
        ) |>
        dplyr::select(
          dplyr::all_of(group_cols), "variable", "category", "value"
        )
    }
  ) |>
    dplyr::bind_rows()
}

#' Extract mean variables from the result of [get_aggregate_value()]
#'
#' @param res the result of [get_aggregate_value()]
#' @param mean_vars the numeric variables to summarize as mean (SD)
#' @param group_cols the columns to group by
#' @param digits the number of decimals used when formatting statistics
#'
#' @return a long data.frame with one row per variable
extract_table_one_mean <- function(res, mean_vars, group_cols, digits) {
  purrr::map(
    mean_vars,
    function(v) {
      res |>
        dplyr::select(
          dplyr::all_of(group_cols),
          mean = paste0(v, "_mean"),
          sd = paste0(v, "_std")
        ) |>
        dplyr::mutate(
          variable = v,
          category = " ",
          value = format_table_one_mean(.data$mean, .data$sd, digits)
        ) |>
        dplyr::select(
          dplyr::all_of(group_cols), "variable", "category", "value"
        )
    }
  ) |>
    dplyr::bind_rows()
}

#' Extract median variables from the result of [get_aggregate_value()]
#'
#' @param res the result of [get_aggregate_value()]
#' @param median_vars the numeric variables to summarize as median (5%-95%)
#' @param group_cols the columns to group by
#' @param digits the number of decimals used when formatting statistics
#'
#' @return a long data.frame with one row per variable
extract_table_one_median <- function(res, median_vars, group_cols, digits) {
  purrr::map(
    median_vars,
    function(v) {
      res |>
        dplyr::select(
          dplyr::all_of(group_cols),
          median = paste0(v, "_median"),
          quant_5 = paste0(v, "_quant_5"),
          quant_95 = paste0(v, "_quant_95")
        ) |>
        dplyr::mutate(
          variable = v,
          category = " ",
          value = format_table_one_median(
            .data$median, .data$quant_5, .data$quant_95, digits
          )
        ) |>
        dplyr::select(
          dplyr::all_of(group_cols), "variable", "category", "value"
        )
    }
  ) |>
    dplyr::bind_rows()
}

#' Format a count/proportion pair as `"N (P%)"`
#'
#' @param n a numeric vector of counts
#' @param prop a numeric vector of proportions
#' @param digits the number of decimals used when formatting `prop`
#'
#' @return a character vector
format_table_one_prop <- function(n, prop, digits) {
  dplyr::if_else(
    is.na(n) | is.na(prop),
    NA,
    sprintf(paste0("%s (%.", digits, "f%%)"), n, 100 * prop)
  )
}

#' Format a mean/SD pair as `"mean (SD)"`
#'
#' @param mean a numeric vector of means
#' @param sd a numeric vector of standard deviations
#' @param digits the number of decimals used when formatting `mean`/`sd`
#'
#' @return a character vector
format_table_one_mean <- function(mean, sd, digits) {
  dplyr::if_else(
    is.na(mean),
    NA,
    sprintf(paste0("%.", digits, "f (%.", digits, "f)"), mean, sd)
  )
}

#' Format a median/range triple as `"median (5%-95%)"`
#'
#' @param median a numeric vector of medians
#' @param quant_5 a numeric vector of 5th percentiles
#' @param quant_95 a numeric vector of 95th percentiles
#' @param digits the number of decimals used when formatting the statistics
#'
#' @return a character vector
format_table_one_median <- function(median, quant_5, quant_95, digits) {
  dplyr::if_else(
    is.na(median),
    NA,
    sprintf(
      paste0("%.", digits, "f (%.", digits, "f-%.", digits, "f)"),
      median, quant_5, quant_95
    )
  )
}
