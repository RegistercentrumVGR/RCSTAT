# Small internal helpers
any_na <- function(...) apply(cbind(...), 1, anyNA)

# Check that only valid numbers are used as EQ5D-levels
valid <- function(..., ok) {
  check <- function(x) {
    if (!is.numeric(x))
      stop("EQ5D-values must be numeric!")
    if (any(!x %in% c(NA, ok)))
      stop("EQ5D-values must be: ", paste(ok, collapse = ", "))
  }
  lapply(data.frame(...), check)
}


#' Calculate Swedish EQ5D-indices based on TTO
#'
#' The general purpose eq5d-package (available on CRAN) might be used for most
#' calculations of EQ5D-index values. Some combinations based on Swedish value
#' sets are, however, not (yet) available there.
#'
#' @param ... arguments passed to underlying methods.
#'   Either `mobility,selfcare,usual,pain,anxiety`
#'   to [eq5d_3l_index_tto()] or [eq5d_5l_index_tto()]
#'   or `x` to [eq5d_3l_index_tto_old()]
#' @param levels numeric: 3 or 5 for the number of levels used
#' @param type character: "TTO" or "VAS"
#' @param old logic: should [eq5d_3l_index_tto_old()] be used?
#' @inheritParams eq5d_3l_index_tto_old
#'
#' @return numeric vector with calculated indices
#' @export
#' @seealso https://github.com/fragla/eq5d
#' @family eq5d
#'
#' @examples
#' eq5d_index(1, 1, 1, 1, 1, levels = 3, type = "TTO")
#' eq5d_index(c("1,1,1,1,1"), levels = 3, type = "TTO", old = TRUE)
#' eq5d_index(1, 1, 1, 1, 1, levels = 5, type = "TTO")
#' eq5d_index(3,4,5,4,3, levels = 5, type = "VAS")
eq5d_index <- function(..., levels, type, old = FALSE, split = ",") {
  stopifnot(levels %in% c(3, 5), type %in% c("VAS", "TTO"))
  if (type == "TTO") {
    if (levels == 3) {
      if (old) {
        eq5d_3l_index_tto_old(..., split = split)
      } else {
        eq5d_3l_index_tto(...)
      }
    } else if (levels == 5) {
      if (old) {
        warning(paste0(
          "Argument `old` ignored ",
          "(not implemented for `levels = 5`)!"
        ))
      }
      eq5d_5l_index_tto(...)
    }
  } else if (type == "VAS") {
    if (levels == 5) eq5d_5l_index_vas(...)
    else stop("VAS is only available for `levels = 5`!")
  }
}


#' Calculate Swedish EQ5D-index based on TTO from EQ5D-3-level data
#'
#' @param mobility,selfcare,usual,pain,anxiety numeric
#'        scale values for each dimension
#' @return numeric vector with calulated index-values
#' @export
#' @references
#'   [Burström et al. 2014, TTO Model 4, Table 3](https://dx.doi.org/10.1007%2Fs11136-013-0496-4)
#' @family eq5d
#' @examples
#' eq5d_3l_index_tto(1, 1, 1, 1, 1) # 0.969
#' eq5d_3l_index_tto(1, 2, 2, 3, 3) # 0.499
#' eq5d_3l_index_tto(2, 1, 3, 1, 1) # 0.724
#'
#' # Works with vectors as well
#' eq5d_3l_index_tto(rep(1, 3), rep(1, 3), rep(1, 3), rep(1,3), rep(1, 3))
eq5d_3l_index_tto <- function(mobility, selfcare, usual, pain, anxiety) {
  valid(mobility, selfcare, usual, pain, anxiety, ok = 1:3)
  ifelse(
    any_na(mobility, selfcare, usual, pain, anxiety), NA,
    0.9694 -
      c(0, 0.0666, 0.1247)[mobility] -
      c(0, 0.0276, 0.0276)[selfcare] -
      c(0, 0.1012, 0.1355)[usual]    -
      c(0, 0.0345, 0.0904)[pain]     -
      c(0, 0.0552, 0.2077)[anxiety]  -
      ifelse(pmax(mobility, selfcare, usual, pain, anxiety) == 3, 0.0433, 0)
  )
}


#' Convert EQ5D-3L-values stored in combined character vectors to Swedish
#' EQ5D-index based on TTO
#'
#' @param x character vector with comma-separated vaslues for each dimension.
#' @inheritParams base::strsplit
#' @return numeric vector with EQ5D-indices based on TTO
#' @export
#' @family eq5d
#' @seealso [eq5d_3l_index_tto()]
#'
#' @examples
#' eq5d_3l_index_tto_old(c("2,2,2,2,3", NA, "2,2,2,3,3", "2,2,2,2,2"))
#' eq5d_3l_index_tto_old(c("22223", NA, "22233", "22222"), split = "")
eq5d_3l_index_tto_old <- function(x, split = ",") {
  xx <-
    x |>
    strsplit(split) |>
    lapply(as.numeric) |>
    data.frame() |>
    t() |>
    data.frame()
  colnames(xx) <- c("mobility", "selfcare", "usual", "pain", "anxiety")
  do.call(eq5d_3l_index_tto, xx)
}


#' Calculate Swedish EQ5D-index based on TTO from EQ5D-5-level data
#'
#' @inheritParams eq5d_3l_index_tto
#'
#' @return numeric vector with EQ5D-indices
#' @export
#' @references
#'   [Burström, model 5 TTO](https://link.springer.com/article/10.1007/s40273-020-00905-7/tables/9)
#' @family eq5d
#' @examples
#' eq5d_5l_index_tto(3,4,5,4,3) # 0.503
#' eq5d_5l_index_tto(rep(3, 10), rep(4, 10),rep(5, 10), rep(4, 10), rep(3, 10))
eq5d_5l_index_tto <- function(mobility, selfcare, usual, pain, anxiety) {
  valid(mobility, selfcare, usual, pain, anxiety, ok = 1:5)
  ifelse(
    any_na(mobility, selfcare, usual, pain, anxiety), NA,
    0.9755 -
      c(0, 0.0287, 0.0346, 0.0523, 0.0523)[mobility] -
      c(0, 0.0254, 0.0817, 0.0824, 0.0824)[selfcare] -
      c(0, 0.0549, 0.1143, 0.1639, 0.1639)[usual]    -
      c(0, 0.0108, 0.0428, 0.1024, 0.1974)[pain]     -
      c(0, 0.0325, 0.0868, 0.2002, 0.2339)[anxiety]  -
      ifelse(pmax(mobility, selfcare, usual, pain, anxiety) == 5, 0.0023, 0)
  )
}


#' Calculate Swedish EQ5D-index based on VAS from EQ5D-5-level data
#'
#' @inheritParams eq5d_3l_index_tto
#'
#' @return numeric vector with EQ5D-indices
#' @export
#' @family eq5d
#' @references
#'   [Burström, model 5 VAS](https://link.springer.com/article/10.1007/s40273-020-00905-7/tables/9)
#'
#' @examples
#' eq5d_5l_index_vas(3,4,5,4,3) # 30.5
#' eq5d_5l_index_vas(rep(3, 10), rep(4, 10),rep(5, 10), rep(4, 10), rep(3, 10))
eq5d_5l_index_vas <- function(mobility, selfcare, usual, pain, anxiety) {
  maxp <- pmax(mobility, selfcare, usual, pain, anxiety)
  ifelse(
    any_na(mobility, selfcare, usual, pain, anxiety), NA,
    88.85 -
      c(0, 3.37,  5.53,  9.05,  9.05)[mobility] -
      c(0, 2.25,  2.82,  6.07,  7.83)[selfcare] -
      c(0, 5.23, 10.12, 14.07, 17.05)[usual]    -
      c(0, 1.63,  4.43, 10.14, 17.05)[pain]     -
      c(0, 4.97, 10.75, 16.52, 27.30)[anxiety]  -
      ifelse(maxp >= 2, 2.75, 0) -
      ifelse(maxp >= 3, 4.19, 0) -
      ifelse(maxp >= 4, 1.85, 0)
  )
}
