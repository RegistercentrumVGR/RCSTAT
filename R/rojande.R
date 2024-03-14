#' Make data non-revealing
#'
#' @param data Aggregated data with frequencies and proportions
#' @param freq_vars variables/columns containing frequencies
#' @param tot_freq_var variable/column with total frequency for group
#' @param statistics_vars variables/columns containing proportions
#'
#' @example man/examples/rojande.R
#'
#' @export
obfuscate_data <- function(
    data,
    freq_vars = NULL,
    tot_freq_var = NULL,
    statistics_vars = NULL) {


  # Clear rows with less than 5 for proportions
  # and where the denominator (tot_freq_var)
  # is less than 15
  if (!is.null(statistics_vars)) {
    for (i in seq_len(nrow(data))) {
      if (any(data[i, freq_vars] < 5) || any(data[i, tot_freq_var] < 15)) {
        data[i, c(freq_vars, tot_freq_var, statistics_vars)] <- NA
      }
    }
    # Round proportions
    data[, statistics_vars] <- lapply(
      data[, statistics_vars], function(x) {
        roundc(x, digits = 2)
      }
    )
  }

  if (!is.null(tot_freq_var)) {
    # Round total frequencies
    data[, tot_freq_var] <- lapply(
      data[, tot_freq_var],
      \(x) data.table::fifelse(x < 5, NA_integer_, roundc(x, -1L))
    )
  }
  if (!is.null(freq_vars)) {
    # Round frequencies
    data[, freq_vars] <- lapply(
      data[, freq_vars],
      \(x) data.table::fifelse(x < 5, NA_integer_, roundc(x, -1L))
    )
  }

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
  width <- roundc(z * sqrt((p_hat * (1 - p_hat)) / n), digits = 2)
  p_hat <- roundc(p_hat, digits = 2)
  list(lower = pmax(0, p_hat - width), upper = pmin(1, p_hat + width))
}
