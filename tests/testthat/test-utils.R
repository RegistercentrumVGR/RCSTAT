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

test_that("format of pnr can be validated", {
  dummy_pnr <- c(
    7,
    "19110908-453Z",
    "19001219_8750",
    "19390524-1224",
    "160404-7850",
    "193004303099",
    "19350320-7452",
    "19390914-1131",
    "Hemliga meddelanden till flitiga reviewers"
  )

  expected <-
    c(
      FALSE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      TRUE,
      FALSE
    )
  testthat::expect_equal(
    valid_pnr_format(dummy_pnr),
    expected
  )
})

test_that("the control number can be validated", {
  dummy_pnr <- c(
    "193004303099",
    "19350320-7452",
    "19390914-1131",
    "19591912-1331",
    "Hemliga meddelanden till flitiga reviewers"
  )

  expected <-
    c(
      FALSE,
      TRUE,
      TRUE,
      FALSE,
      FALSE
    )
  testthat::expect_equal(
    valid_pnr(dummy_pnr),
    expected
  )
})

test_that("the control number can be validated", {
  dummy_pnr <- c(
    "193004303099",
    "19350320-7452",
    "19390914-1131",
    "19591912-1331",
    "Hemliga meddelanden till flitiga reviewers"
  )

  expected <-
    c(
      FALSE,
      TRUE,
      TRUE,
      FALSE,
      FALSE
    )

  testthat::expect_equal(
    valid_pnr(dummy_pnr),
    expected
  )

  expected <-
    c(
      NA,
      TRUE,
      TRUE,
      FALSE,
      NA
    )

  testthat::expect_equal(
    valid_pnr(dummy_pnr, handle_invalid = FALSE),
    expected
  )
})

test_that("pseudonymizing data works", {
  res <- pseudonymize_data(
    df = dummy_data,
    pseudonimzed_var = "lopnr",
    subject_key = "id",
    remove_subject_key = FALSE,
    save_key = FALSE
  )

  expect_true("lopnr" %in% names(res))
  expect_true("id" %in% names(res))

  res <- pseudonymize_data(
    df = dummy_data,
    pseudonimzed_var = "abc",
    subject_key = "id",
    remove_subject_key = TRUE,
    save_key = FALSE
  )

  expect_true("abc" %in% names(res))
  expect_true(!"id" %in% names(res))

  res <- pseudonymize_data(
    df = dummy_data,
    pseudonimzed_var = "abc",
    subject_key = "id",
    remove_subject_key = TRUE,
    save_key = TRUE,
    key_file_dir = "."
  )

  expect_true(file.exists("abc.xlsx"))
  expect_named(readxl::read_xlsx("abc.xlsx"), c("id", "abc"))
})

withr::defer(unlink("abc.xlsx"), teardown_env())

test_that("random_password works", {
  # Test errors
  expect_no_error(
    random_password()
  )

  # Test erroneous char_types
  expect_error(
    random_password(
      char_types = c("U")
    )
  )

  # Test password_length
  random_password(
    password_length = 10
  ) |>
    expect_type("character") |>
    stringr::str_length() |>
    expect_equal(10)
})
