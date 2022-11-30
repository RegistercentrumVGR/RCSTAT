test_that("Rolling average works", {

  x <- 10:20
  n <- 4

  res_right <- c(NA, NA, NA, 11.5, 12.5, 13.5, 14.5, 15.5, 16.5, 17.5, 18.5)
  res_left <- c(11.5, 12.5, 13.5, 14.5, 15.5, 16.5, 17.5, 18.5, NA, NA, NA)
  res_center <- c(NA, 11.5, 12.5, 13.5, 14.5, 15.5, 16.5, 17.5, 18.5, NA, NA)

  testthat::expect_equal(rolling_mean(x, n = n, align = "left"), res_left)
  testthat::expect_equal(rolling_mean(x, n = n, align = "right"), res_right)
  testthat::expect_equal(rolling_mean(x, n = n, align = "center"), res_center)

  x <- c(NA, 10:15, NA, 16:20)
  n <- 4

  res_right <- c(NA, NA, NA, NA, 11.5, 12.5, 13.5, NA, NA, NA, NA, 17.5, 18.5)
  res_left <- c(NA, 11.5, 12.5, 13.5, NA, NA, NA, NA, 17.5, 18.5, NA, NA, NA)
  res_center <- c(NA, NA, 11.5, 12.5, 13.5, NA, NA, NA, NA, 17.5, 18.5, NA, NA)

  testthat::expect_equal(rolling_mean(x, n = n, align = "left"), res_left)
  testthat::expect_equal(rolling_mean(x, n = n, align = "right"), res_right)
  testthat::expect_equal(rolling_mean(x, n = n, align = "center"), res_center)

})
