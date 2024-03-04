#' Make data non-revealing
#'
#' @param data Aggregated data with frequencies and proportions
#' @param freq_vars variables/columns containing frequencies
#' @param proportion_vars variables/columns containing proportions
#'
#' @export
obfuscate_data <- function(
    data,
    freq_vars,
    proportion_vars = NULL) {


  # Clear rows with less than 5 for proportions
  if (!is.null(proportion_vars)) {
    for (i in seq_len(nrow(data))) {
      if (any(data[i, freq_vars] < 5)) {
        data[i, c(freq_vars, proportion_vars)] <- NA
      }
    }
    # Round proportions
    data[, proportion_vars] <- lapply(
      data[, proportion_vars], function(x) {
        round(x, digits = 2)
      }
    )
  }
  # Round frequencies
  data[, freq_vars] <- lapply(data[, freq_vars], \(x) round(x, -1L))

  data
}

#' Get rounded ci for proportion
#'
#' @param p_hat estimate of proportion
#' @param n sample size
#' @param alpha alpha level
#'
#' @export
rounded_ci_p <- function(p_hat, n, alpha = 0.05) {
  z <- stats::qnorm(1 - alpha / 2)
  width <- round(z * sqrt((p_hat * (1 - p_hat)) / n), digits = 2)
  p_hat <- round(p_hat, digits = 2)
  list(lower = pmax(0, p_hat - width), upper = pmin(1, p_hat + width))
}



#' Round 5's upwards instead of `round`'s behavior which is to round towards
#' the even digit
#'
#' @param x The number you wish to round
#' @param digits the number of decimal places to be used
#'
#' @return
#' @export
#'
#' @examples
#' round(1.5)
#' round_up(1.5)
round_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x)*10^digits
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z/10^digits
  return(z*posneg)
}
