test_that("methods give same result on dt and df ", {

  # Grab some data for tests
  df <- dplyr::filter(
    dummy_data,
    id %in% c(1, 2),
    a %in% c(1, 2),
    b %in% c(1, 2)
  ) |>
    dplyr::select(id, a, b, c, x, y, z)



  # Test group_means on data.frame and data.table
  res <- group_means(df, vars = c("x", "y"), group_by = c("a", "b"))

  expected_res <- tibble::tribble(
    ~a, ~b, ~n, ~x_mean, ~x_sd, ~y_mean, ~y_sd,
    1,   1, 10,  NA,      NA,    NA,      NA,
    1,   2, 10,  NA,      NA,    NA,      NA,
    2,   1, 10,  NA,      NA,    NA,      NA,
    2,   2, 10,  NA,      NA,    NA,      NA,
  )

  expect_equal(res, expected_res)

  # Test group_proportions on data.frame and data.table
  res <- group_proportions(df, group_by = c("a", "b"))

  expected_res <- tibble::tribble(
    ~a, ~b, ~Count, ~Total, ~Proportion,
    1,   1, 10,      20,     0.47,
    1,   2, 10,      20,     0.53,
    2,   1, 10,      20,     0.35,
    2,   2, 10,      20,     0.65,
  )

  expect_equal(res, expected_res)

  # Test proportion_missing on data.frame and data.table
  res <- proportion_missing(df, vars = c("c", "x"), group_by = "b")

  expected_res <- tibble::tribble(
    ~b, ~N,       ~c,       ~x,        ~Proportion_missing_c , ~Proportion_missing_x,
    1,  NA_real_, NA_real_, NA_real_,  NA_real_,                NA_real_,
    2,  NA_real_, NA_real_, NA_real_,  NA_real_,                NA_real_
  )

  expect_equal(res, expected_res)

})
