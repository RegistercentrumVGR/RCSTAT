
#' Assign each value with its corresponding label
#'
#' Returns data frame with descriptive labels for data points
#'
#' @name SetValueLabels
#' @title SetValueLabels
#' @param df Data frame with Stratum register data
#' @param labels Value labels for the same register/form
#'
#' @return data.frame with map attribute labels on values.
#' @export
#' @examples
#' \dontrun{
#' df <- GetRegisterData(1012, RunDeep = TRUE)
#' lab_val <- GetValueLabels(1012)
#' df <- SetValueLabels(df, lab_val)
#' }
#' @importFrom rlang .data
#'
SetValueLabels <- function(df, labels)
{

  # Add value labels as separate key/value map for each factor variable.
  for (cn in unique(labels[["ColumnName"]])) {
    if (cn %in% colnames(df)) {
      # return vector with value codes for current variable
      vc <- labels[["ValueCode"]][labels[["ColumnName"]] == cn]
      # return vector with value names for current variable
      vn <- labels[["ValueName"]][labels[["ColumnName"]] == cn]
      # Attach key/value map for domain value lookup (in UTF-8 to be serializable with rjson).
      attr(df[[cn]], "map") <- data.frame(levels = vc, labels = enc2utf8(as.character(vn)))
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
#' attr(df$cars, "map") <- data.frame(levels = 1:3, labels = c("Volvo", "Saab", "Opel"))
#' attr_to_factor(df$cars)
#' @export
attr_to_factor <- function(x, droplevels = TRUE, name = NULL){

  unique_levels <- as.character(unique(x))
  unique_levels <- unique_levels[!is.na(unique_levels)]

  if(droplevels){
    # Remove factor levels that are not used
    attr(x, "map") <- attr(x, "map")[attr(x, "map")$levels %in% unique_levels,]
  }
  # Check if there are data that does not
  # have a label and warn.
  x_no_label <- unique_levels[!unique_levels %in% attr(x, "map")$levels]

  if(!is.null(x_no_label) & !all(is.na(x_no_label))){

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

#' Replace each value with its corresponding label
#'
#' Returns data frame with descriptive data points
#' instead of numerical codes.
#'
#' @name SetFactors
#' @title SetFactors
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
#' factor_data <- SetFactors(data, factor_levels)
#' }
#'
SetFactors <- function(
    df,
    labels,
    droplevels = TRUE
){

  df <- SetValueLabels(df = df, labels = labels )
  if(!data.table::is.data.table(df)){
    if(is.character(droplevels)){
      droplevels <- stats::setNames(as.list(names(df) %in% droplevels), names(df))
    }else{
      droplevels <-  stats::setNames(as.list(rep(droplevels, length(names(df)))), names(df))
    }
  }

  has_map   <- function(var){!is.null(attr(var, "map"))}
  # Get vars that have "map" attribute
  map_vars <- names(df)[unname(unlist(lapply(df, has_map)))]

  if(length(map_vars) > 0){
    # for data.tables
    if(data.table::is.data.table(df)){

      # Split map_vars into two character vectors.
      # One with variables where levels are to be
      # dropped and one where levels are to be kept.
      if(is.logical(droplevels)){
        if(isTRUE(droplevels)){
          map_w_drop <- map_vars
          map_wo_drop <- NULL
        }else{
          map_w_drop <- NULL
          map_wo_drop <- map_vars
        }
      }else{
        map_w_drop <- map_vars[map_vars %in% droplevels]
        map_wo_drop <- map_vars[!map_vars %in% droplevels]
      }
      # Do lapply on both with and without droplevels
      # columns separately.
      if(length(map_w_drop)>0){
        df <- df[
          ,
          (map_w_drop) := lapply(.SD,
                                 function(x) attr_to_factor(x = x, droplevels = TRUE)),
          .SDcols = map_w_drop
        ]
      }
      if(length(map_wo_drop)>0){
        df <- df[
          ,
          (map_wo_drop) := lapply(.SD,
                                  function(x) attr_to_factor(x = x, droplevels = FALSE)),
          .SDcols = map_wo_drop
        ]
      }
    # dplyr for data.frames/tibbles
    }else{
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
