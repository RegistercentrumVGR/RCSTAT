#' Convert vector to factor using supplied labels
#'
#' @param x Vector with codes to decode
#' @param labels data.frame with columns ValueCode and ValueName to use
#' when decoding.
#' @param name name of input variable (optional). Used in the warning message
#' only to print the name of variable with missing labels.
#' @param droplevels If `TRUE`, factor levels not present in data will be
#' excluded.
#'
#' @return Factor
#' @examples
#'
#' x <- sample(1:3, size = 10, replace = TRUE)
#'
#' labels <- data.frame(
#'   ValueCode = 1:3,
#'   ValueName = c("Volvo", "Saab", "Opel")
#' )
#' var_to_factor(x, labels, name = "cars")
#' @export
var_to_factor <- function(x, labels, name = NULL, droplevels = TRUE) {

  unique_levels <- as.character(unique(x))
  unique_levels <- unique_levels[!is.na(unique_levels)]

  if (droplevels) {
    # Remove factor levels that are not used
    labels <- labels[labels[["ValueCode"]] %in% unique_levels, ]
  }

  # Check if there are data that does not have a label and warn.
  x_no_label <- unique_levels[!unique_levels %in% labels[["ValueCode"]]]

  if (!is.null(x_no_label) && length(x_no_label) > 0 && !all(is.na(x_no_label))) {

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
#' @param x Vector with codes to decode
#' @param labels data.frame with columns ValueCode and ValueName to use when
#' decoding/translating.
#' @param name name of input variable (optional)
#'
#' @return character vector
#' @examples
#'
#' x <- sample(1:3, size = 10, replace = TRUE)
#'
#' labels <- data.frame(
#'   ValueCode = 1:3,
#'   ValueName = c("Volvo", "Saab", "Opel")
#' )
#' var_to_char(x, labels, name = "cars")
#'
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
#' @param data Data frame with Stratum register data
#' @param labels Value labels for the same register
#' @param droplevels If `TRUE`, factor levels not present in data will be
#' excluded. If `FALSE` no levels are dropped. If character
#' then the specified variables will have its levels dropped.
#' @param add_cols Add additional columns with the labels
#' @param suffix suffix to add to added columns
#' @param as_character If `TRUE` variables
#' will be set to characters. If `FALSE` variables will be factors.
#'
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

    labels_list <- list()
  for (var in vars_to_decode) {
    labels_list[[var]] <- labels[labels[["ColumnName"]] == var,]
  }

  # Exclude variables which already have only values from their labels (See issue #6)
  already_decoded <-
    vapply(
      vars_to_decode,
      \(x) all(data[[x]] %in% c(NA, labels_list[[x]]$ValueName)),
      logical(1)
    )
  vars_to_decode <- vars_to_decode[!already_decoded]
  labels_list[already_decoded] <- NULL

  if (is.logical(droplevels)) {
    droplevels <- rep(droplevels, length(vars_to_decode))
  } else {
    droplevels <- vars_to_decode %in% droplevels
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
        data.table::setcolorder(data, col_order)

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

