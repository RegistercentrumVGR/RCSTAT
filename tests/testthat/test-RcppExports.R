test_that("rounding works", {
  x <- c(2.5, 1.5, -1.5, -2.5, 1.5 - .Machine$double.eps^0.5)
  x_rounded <- c(3, 2, -2, -3, 1)

  testthat::expect_equal(roundc(x), x_rounded)
})
