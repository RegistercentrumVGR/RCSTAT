#' Rolling average over vector
#'
#' @param x numeric vector to compute rolling average on
#' @param n size of window
#' @param align start window at the left, right or middle
#' @param na.rm remove NA when computing mean (consider them to be 0)
#'
#' @export rolling_mean
rolling_mean <- function(
    x,
    n,
    align = c("left", "center", "right")[3],
    na.rm = FALSE
){
  if(na.rm == TRUE){
    missing_ind <- which(is.na(x))
    x[is.na(x)] <- 0
  }
  if(align == "left"){
    x <- rev(as.vector(stats::filter(rev(x), rep(1 / n, n), sides = 1)))
  } else {
    sides <- which(c("right", "center") == align)
    x <- as.vector(stats::filter(x, rep(1 / n, n), sides = sides))
  }
  if(na.rm == TRUE){
    x[missing_ind] <- NA
  }

  x
}
