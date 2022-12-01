test_that("Age can be calculated ", {

  testthat::expect_equal(
    age(as.Date("2000-02-01"), as.Date("2001-01-01"), unit = "whole_years"),
    0L
  )
  testthat::expect_equal(
    age(as.Date("2000-02-01"), as.Date("2001-02-01"), unit = "whole_years"),
    1L
  )
})
