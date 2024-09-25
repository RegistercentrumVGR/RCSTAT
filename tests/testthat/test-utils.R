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

test_that("Age can be calculated from pnr ", {
  dummy_pnr <- c(
    "19110908-4531",
    "19001219-8750",
    "19390524-1224",
    "19160404-7850",
    "19300430-3099",
    "193503207452",
    "193909141131",
    "192204052449",
    "194811178104",
    "196808238551",
    "201901011111",
    "202001011111"
  )

  ages_2020 <- c(108, 119, 80, 103, 89, 84, 80, 97, 71, 51, 1, 0)
  testthat::expect_equal(
    age(birthdate(dummy_pnr), as.Date("2020-01-01"), unit = "whole_years"),
    ages_2020
  )

  dates_plus_10_years <- as.Date(birthdate(dummy_pnr) + lubridate::dyears(10))
  ages_10 <- c(9, 10, 9, 10, 9, 9, 9, 9, 10, 10, 9, 9)

  testthat::expect_equal(
    age(birthdate(dummy_pnr), dates_plus_10_years, unit = "whole_years"),
    ages_10
  )
})

test_that("Gender can be calculated from pnr ", {
  dummy_pnr <- c(
    "19110908-4531",
    "19001219-8750",
    "19390524-1224",
    "19160404-7850",
    "19300430-3099",
    "193503207452",
    "193909141131",
    "192204052449",
    "194811178104",
    "196808238551",
    "201901011111",
    "202001011111"
  )

  calculated_genders <-
    c(
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      FALSE
    )

  testthat::expect_equal(
    is_female(dummy_pnr),
    calculated_genders
  )
})
