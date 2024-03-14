test_that("methods give same result on dt and df ", {

  # Grab some data for tests
  df <- dplyr::filter(
    dummy_data,
    id %in% c(1, 2),
    a %in% c(1, 2),
    b %in% c(1, 2)
  ) |>
    dplyr::select(id, a, b, c, x, y, z)

  # Give me "more" data!
  df_big <- dplyr::bind_rows(df, df, df, df, df, df)

  # Test group_means on data.frame and data.table
  res <- group_means(df_big, vars = c("x", "y"), group_by = c("a", "b"))

  expected_res <- tibble::tribble(
    ~a, ~b, ~n, ~x_non_missing, ~y_non_missing, ~x_mean, ~x_sd, ~y_mean, ~y_sd,
    1,   1, 50, 40,             40,             -0.59,   1.05,  0.27,    0.21,
    1,   2, 50, 40,             30,             -0.31,   0.67,  0.18,    0.12,
    2,   1, 40, 30,             30,              0.86,   0.5,   0.46,    0.22,
    2,   2, 70, 60,             50,              0.4,    1.13,  0.56,    0.37
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
    ~b, ~N,       ~c,       ~x,        ~proportion_missing_c, ~proportion_missing_x,
    1,  NA_real_, NA_real_, NA_real_,  NA_real_,                NA_real_,
    2,  NA_real_, NA_real_, NA_real_,  NA_real_,                NA_real_
  )

  expect_equal(res, expected_res)

})
