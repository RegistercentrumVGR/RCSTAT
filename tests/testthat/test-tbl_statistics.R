test_that("methods give same result on dt and df ", {

  # Grab some data for tests
  df <- dplyr::filter(
    dummy_data,
    id %in% c(1, 2),
    a %in% c(1, 2),
    b %in% c(1, 2)
  ) |>
    dplyr::select(id, a, b, c, x, y, z)

  dt <- data.table::setDT(data.table::copy(df))

  # Test group_means on data.frame and data.table
  res <- group_means(df, vars = c("x", "y"), a, b)
  dt_res <- group_means(dt, vars = c("x", "y"), a, b)
  expect_equal(res, dt_res)

  # Test group_proportions on data.frame and data.table
  res <- group_proportions(df, a, b)
  dt_res <- group_proportions(dt, a, b)
  expect_equal(res, dt_res)

  # Test proportion_missing on data.frame and data.table
  res <- proportion_missing(df, vars = c("c", "x", "y", "z"), a, b)
  dt_res <- proportion_missing(dt, vars = c("c", "x", "y", "z"), a, b)
  expect_equal(res, dt_res)

})
