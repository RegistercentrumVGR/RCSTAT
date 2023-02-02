
#' Assign each value with its corresponding label
#'
#' Returns data frame with descriptive labels for data points
#'
#' @name set_value_labels
#' @title set_value_labels
#' @param df Data frame with Stratum register data
#' @param labels Value labels for the same register/form
#'
#' @return data.frame with map attribute labels on values.
#' @export
#' @examples
#' \dontrun{
#' df <- set_value_labels(df, lab_val)
#' }
#' @importFrom rlang .data
#'
set_value_labels <- function(df, labels) {

  # Add value labels as separate key/value map for each factor variable.
  for (cn in unique(labels[["ColumnName"]])) {
    if (cn %in% colnames(df)) {
      # return vector with value codes for current variable
      vc <- labels[["ValueCode"]][labels[["ColumnName"]] == cn]
      # return vector with value names for current variable
      vn <- labels[["ValueName"]][labels[["ColumnName"]] == cn]
      # Attach key/value map for domain value lookup
      # (in UTF-8 to be serializable with rjson).
      attr(df[[cn]], "map") <- data.frame(
        levels = vc,
        labels = enc2utf8(as.character(vn))
      )
    }
  }

  return(df)

}

#' Convert object with attributes in "map" to factor
#'
#' Used to extract texts from a variable containing numeric values.
#' @param x Object with "map" attributes, where levels and labels exist
#' @param droplevels If `TRUE`, factor levels not present in data will be
#'                   excluded.
#' @param name name of input variable (optional)
#' @return Factor
#' @examples
#' df <- data.frame(cars = 1:3)
#' attr(df$cars, "map") <- data.frame(
#'     levels = 1:3,
#'     labels = c("Volvo", "Saab", "Opel")
#'  )
#' attr_to_factor(df$cars)
#' @export
attr_to_factor <- function(x, droplevels = TRUE, name = NULL) {

  unique_levels <- as.character(unique(x))
  unique_levels <- unique_levels[!is.na(unique_levels)]

  if (droplevels) {
    # Remove factor levels that are not used
    attr(x, "map") <- attr(x, "map")[attr(x, "map")$levels %in% unique_levels, ]
  }
  # Check if there are data that does not
  # have a label and warn.
  x_no_label <- unique_levels[!unique_levels %in% attr(x, "map")$levels]

  if (!is.null(x_no_label) && !all(is.na(x_no_label))) {

    n_obs <- sum(x %in% x_no_label)

    rlang::warn(paste0(
      "\nSome levels in variable does not have a label",
      "\n Number of observations implicitly set to NA: ", n_obs,
      "\n Variable name: ", name,
      "\n Values: ", paste(x_no_label, collapse = ", "), "\n"
    )
    )
  }

  x <- factor(
    x,
    levels = attr(x, "map")$levels,
    labels = attr(x, "map")$labels
  )

  return(x)
}

#' Convert object with attributes in "map" to factor
#'
#' Used to extract texts from a variable containing numeric values.
#' @param x Object with "map" attributes, where levels and labels exist
#' @param droplevels If `TRUE`, factor levels not present in data will be
#'                   excluded.
#' @param name name of input variable (optional)
#' @return Factor
#' @examples
#' df <- data.frame(cars = 1:3)
#' attr(df$cars, "map") <- data.frame(
#'     levels = 1:3,
#'     labels = c("Volvo", "Saab", "Opel")
#'  )
#' attr_to_factor(df$cars)
#' @export
var_to_factor <- function(x, labels, name, droplevels) {

  unique_levels <- as.character(unique(x))
  unique_levels <- unique_levels[!is.na(unique_levels)]

  if (droplevels) {
    # Remove factor levels that are not used
    labels <- labels[labels[["ValueCode"]] %in% unique_levels, ]
  }

  # Check if there are data that does not have a label and warn.
  x_no_label <- unique_levels[!unique_levels %in% labels[["ValueCode"]]]

  if (!is.null(x_no_label) && !all(is.na(x_no_label))) {

    n_obs <- sum(x %in% x_no_label)

    rlang::warn(paste0(
      "\nSome levels in variable does not have a label",
      "\n Number of observations implicitly set to NA: ", n_obs,
      "\n Variable name: ", name,
      "\n Values: ", paste(x_no_label, collapse = ", "), "\n"
    )
    )
  }

  x <- factor(
    x,
    levels = labels[["ValueCode"]],
    labels = labels[["ValueName"]]
  )

  return(x)
}
#' Convert variable to character
#'
#' Used to extract texts from a variable containing numeric values.
#' @param x Object with "map" attributes, where levels and labels exist
#' @param droplevels If `TRUE`, factor levels not present in data will be
#'                   excluded.
#' @param name name of input variable (optional)
#' @return Factor
#' @examples
#' df <- data.frame(cars = 1:3)
#' labels <- data.frame(
#'     levels = 1:3,
#'     labels = c("Volvo", "Saab", "Opel")
#'  )
#' var_to_char(df[["cars"]])
#' @export
var_to_char <- function(x, labels, name = NULL) {
  as.character(var_to_factor(x, labels, name = name, droplevels = TRUE))
}

#' Replace each value with its corresponding label
#'
#' Returns data with descriptive data points
#' instead of codes.
#'
#' @name decode_vars
#' @title decode_vars
#'
#' @param df Data frame with Stratum register data
#' @param labels Value labels for the same register
#' @param droplevels If `TRUE`, factor levels not present in data will be
#' excluded. If `FALSE` no levels are dropped. If character
#' then the specified variables will have its levels dropped.
#' @param add_cols Add additional columns with the labels
#' @param suffix suffix to add to added columns
#' @param as_character If `TRUE` variables
#' will be set to characters. If `FALSE` variables will be factors.
#'
#' @param
#' @return data.frame with character values instead
#'         of numerical values.
#' @export
#' @examples
#' \dontrun{
#' factor_data <- decode_vars(data, factor_levels)
#' }
#'
decode_data <- function(
    data,
    labels,
    droplevels = TRUE,
    add_cols = FALSE,
    suffix = "_label",
    as_character = FALSE) {

  var_names <- colnames(data)
  label_names <- unique(labels[["ColumnName"]])

  vars_to_decode <- var_names[var_names %in% label_names]

  if (is.logical(droplevels)) {
    droplevels <- rep(droplevels, length(vars_to_decode))
  } else {
    droplevels <- vars_to_decode %in% droplevels
  }

  labels_list <- list()
  for (var in vars_to_decode) {
    labels_list[[var]] <- labels[labels[["ColumnName"]] == var,]
  }

  if (as_character) {
    if (add_cols) {
      for (var in vars_to_decode) {
        # Rename colnames and att suffix
        # remove variable ColumnName
        var_labels <- labels_list[[var]]
        var_labels[["ColumnName"]] <- NULL
        names(var_labels) <- c(var, paste0(var, suffix))
        labels_list[[var]] <- var_labels
      }
      for (var in vars_to_decode) {
        # Save column index for variable and variable names
        col_order <- colnames(data)
        idx_of_var <- which(col_order == var)
        # Handle off-by-one indexing
        if (idx_of_var == 1) {
          # If var is the first
          col_order <- c(var, paste0(var, suffix),
                         col_order[2:length(col_order)])

        } else if (idx_of_var == length(col_order)) {
          # if var is the last one
          col_order <- c(col_order[1:(length(col_order)-1)],
                         var, paste0(var, suffix))
        } else {
          # put label var in correct position next to codes
          col_order <- c(
            # All vars up to var
            col_order[1:idx_of_var-1],
            # Var and var_suffix
            var, paste0(var, suffix),
            # All vars after var
            col_order[(idx_of_var+1):length(col_order)]
          )
        }
        data <- merge(data, labels_list[[var]], all.x = TRUE, sort = FALSE)

        # Re-arrange columns to original order
        data <- data[, col_order]
      }

    } else {
      data[,vars_to_decode] <- Map(
        var_to_char,
        subset(data, select = vars_to_decode),
        labels_list,
        vars_to_decode
      )
    }
  } else {
    data[,vars_to_decode] <- Map(
      var_to_factor,
      subset(data, select = vars_to_decode),
      labels_list,
      vars_to_decode,
      droplevels
    )
  }
  return(data)
}

#' Replace each value with its corresponding label
#'
#' Returns data frame with descriptive data points
#' instead of numerical codes.
#'
#' @name set_factors
#' @title set_factors
#' @param df Data frame with Stratum register data
#' @param labels Value labels for the same register
#' @param droplevels If `TRUE`, factor levels not present in data will be
#'                   excluded. If `FALSE` no levels are dropped. If character
#'                   then the specified variables will have its levels dropped.
#' @return data.frame with character values instead
#'         of numerical values.
#' @export
#' @examples
#' \dontrun{
#' factor_data <- set_factors(data, factor_levels)
#' }
#'
set_factors <- function(
    df,
    labels,
    droplevels = TRUE) {

  df <- set_value_labels(df = df, labels = labels)
  if (!data.table::is.data.table(df)) {
    if (is.character(droplevels)) {
      droplevels <- stats::setNames(
        as.list(names(df) %in% droplevels), names(df)
      )
    }else {
      droplevels <-  stats::setNames(
        as.list(rep(droplevels, length(names(df)))), names(df)
      )
    }
  }

  has_map <- function(var) {
    !is.null(attr(var, "map"))
  }
  # Get vars that have "map" attribute
  map_vars <- names(df)[unname(unlist(lapply(df, has_map)))]


  if (length(map_vars) > 0) {
    # for data.tables
    if (data.table::is.data.table(df)) {

      # Split map_vars into two character vectors.
      # One with variables where levels are to be
      # dropped and one where levels are to be kept.
      if (is.logical(droplevels)) {
        if (isTRUE(droplevels)) {
          map_w_drop <- map_vars
          map_wo_drop <- NULL
        }else {
          map_w_drop <- NULL
          map_wo_drop <- map_vars
        }
      } else {
        map_w_drop <- map_vars[map_vars %in% droplevels]
        map_wo_drop <- map_vars[!map_vars %in% droplevels]
      }
      # Do lapply on both with and without droplevels
      # columns separately.
      if (length(map_w_drop) > 0) {
        df <- df[
          ,
          (map_w_drop) := lapply(
            .SD,
            function(x) attr_to_factor(x = x, droplevels = TRUE)
          ),
          .SDcols = map_w_drop
        ]
      }
      if (length(map_wo_drop) > 0) {
        df <- df[
          ,
          (map_wo_drop) := lapply(
            .SD,
            function(x) attr_to_factor(x = x, droplevels = FALSE)
          ),
          .SDcols = map_wo_drop
        ]
      }
      # dplyr for data.frames/tibbles
    }else {
      df <-
        df %>%
        dplyr::mutate(
          dplyr::across(
            .cols = dplyr::all_of(map_vars),
            .fns = ~ attr_to_factor(
              x = .x,
              droplevels = droplevels[[dplyr::cur_column()]],
              name = dplyr::cur_column()
            )
          )
        )
    }
  }
  return(df)

}
