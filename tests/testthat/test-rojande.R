test_that("rounded_ci_p works", {
  p_hat <- 0.5
  n <- 20
  expected_res <- list(lower = 0.28, upper = 0.72)
  expect_equal(rounded_ci_p(p_hat, n), expected_res)

  p_hat <- c(0.5, 0.5)
  n <- c(20, 30)
  expected_res <- list(lower = c(0.28, 0.32), upper = c(0.72, 0.67))
})

test_that("group_proportions censors as expected", {
  # Make some data for tests
  df <- tibble::tibble(
    a = c(rep(1, 25), rep(2, 11), rep(3, 4)),
    b = c(rep(1:2, 20))
  )
  expected_res <- tibble::tribble(
    ~a, ~Count, ~Total, ~Proportion,
    1,  30,     40,     0.63,
    2,  10,     40,     0.28,
    3,  0,      40,     0
  )
  # Test group_means on data.frame and data.table
  res <- group_proportions(df, group_by = "a", obfuscate = TRUE)

  expect_equal(res, expected_res)

  res <- group_proportions(df, group_by = c("a", "b"), obfuscate = TRUE)
  expected_res <- tibble::tribble(
    ~a, ~b, ~Count, ~Total, ~Proportion,
    1,  1,  10,     30,     0.52,
    1,  2,  10,     30,     0.48,
    2,  1,  10,     10,     0,
    2,  2,  10,     10,     0,
    3,  1,  0,      0,      0,
    3,  2,  0,      0,      0
  )
  expect_equal(res, expected_res)
})

test_that("reason col works", {
  res <- data.frame(
    group = c(rep("a", 3), rep("b", 2), rep("c", 2)),
    n = c(1, 5, 10, 1, 9, 1, 10),
    total = c(rep(16, 3), rep(10, 2), rep(50, 2))
  ) |>
    dplyr::mutate(prop = n / total) |>
    dplyr::group_by(group) |>
    obfuscate_data(add_reason_col = TRUE) |>
    dplyr::ungroup()

  expected_res <- tibble::tribble(
    ~group, ~n, ~total, ~prop, ~obfuscated_reason,
    "a",    0,  20,     0,     "n < 5",
    "a",    10, 20,     0,     "n < 5",
    "a",    10, 20,     0,     "n < 5",
    "b",    0,  10,     0,     "N < 15",
    "b",    10, 10,     0,     "N < 15",
    "c",    0,  50,     0,     "rounded to nearest 5%",
    "c",    10, 50,     0.2,   NA
  )

  expect_equal(res, expected_res)

  res <- data.frame(
    group = c(rep("a", 3), rep("b", 2), rep("c", 2)),
    n = c(1, 5, 10, 1, 11, 1, 10),
    total = c(rep(16, 3), rep(10, 2), rep(50, 2))
  ) |>
    dplyr::mutate(prop = n / total) |>
    dplyr::group_by(group) |>
    obfuscate_data(add_reason_col = TRUE, liberal_obfuscation = FALSE) |>
    dplyr::ungroup()

  expected_res <- tibble::tribble(
    ~group, ~n, ~total, ~prop, ~obfuscated_reason,
    "a",    0,  20,     0,     "n < 5",
    "a",    10, 20,     0,     "n < 5",
    "a",    10, 20,     0,     "n < 5",
    "b",    0,  10,     0,     "N < 15",
    "b",    10, 10,     0,     "N < 15",
    "c",    0,  50,     0,     "n < 5",
    "c",    10, 50,     0,     "n < 5"
  )

  expect_equal(res, expected_res)

  res <- data.frame(
    n = c(1, 11, 1, 11),
    total = c(15, 15, 12, 12)
  ) |>
    dplyr::mutate(prop = n / total) |>
    obfuscate_data(add_reason_col = TRUE, liberal_obfuscation = FALSE) |>
    tibble::as_tibble()

  expected_res <- tibble::tribble(
    ~n, ~total, ~prop, ~obfuscated_reason,
    0,  20,     0,     "n < 5",
    10, 20,     0,     "N - n < 5",
    0,  10,     0,     "N < 15",
    10, 10,     0,     "N < 15",
  )

  expect_equal(res, expected_res)

  res <- data.frame(
    n = c(2, 43, 249, 1),
    total = c(46, 46, 250, 250)
  ) |>
    dplyr::mutate(prop = n / total) |>
    obfuscate_data(add_reason_col = TRUE) |>
    tibble::as_tibble()

  expected_res <- tibble::tribble(
    ~n, ~total, ~prop, ~obfuscated_reason,
    0, 50, 0.05, "rounded to nearest 5%",
    40, 50, 0.95, "rounded to nearest 5%",
    250, 250, 1, NA,
    0, 250, 0, NA
  )

  expect_equal(res, expected_res)

  res <- data.frame(
    group = rep(c("a", "b"), each = 3),
    n = c(23, 23, 4, 20, 20, 4),
    total = c(rep(50, 3), rep(44, 3))
  ) |>
    dplyr::mutate(prop = n / total) |>
    dplyr::group_by(group) |>
    obfuscate_data(add_reason_col = TRUE) |>
    dplyr::ungroup() |>
    tibble::as_tibble()

  expected_res <- tibble::tribble(
    ~group, ~n, ~total, ~prop, ~obfuscated_reason,
    "a", 20, 50, 0.46, NA,
    "a", 20, 50, 0.46, NA,
    "a", 0, 50, 0.1, "rounded to nearest 5%",
    "b", 20, 40, 0, "n < 5",
    "b", 20, 40, 0, "n < 5",
    "b", 0, 40, 0, "n < 5"
  )

  expect_equal(res, expected_res)

  res <- data.frame(
    n = c(1, 5, 10)
  ) |>
    obfuscate_data(
      add_reason_col = TRUE,
      total_var = "n"
    ) |>
    tibble::as_tibble()

  expected_res <- tibble::tribble(
    ~n, ~obfuscated_reason,
    0,  "N < 5",
    10, NA,
    10, NA
  )

  expect_equal(res, expected_res)

  res <- data.frame(
    n = c(5, 20),
    x_mean = c(1.7, 2.1)
  ) |>
    obfuscate_data(
      add_reason_col = TRUE,
      total_var = "n",
      statistics_vars = "x_mean"
    ) |>
    tibble::as_tibble()

  expected_res <- tibble::tribble(
    ~n, ~x_mean, ~obfuscated_reason,
    10, NA, "N < 15",
    20, 2.1, NA
  )

  expect_equal(res, expected_res)

  res <- data.frame(
    n = 4, total = 16, prop = 4 / 16, mean = 0.125
  ) |>
    obfuscate_data(
      add_reason_col = TRUE,
      statistics_vars = "mean",
      round_statistics_vars = TRUE,
      censored_value = NA
    )

  expected_res <- data.frame(
    n = 0, total = 20, prop = as.double(NA), mean = 0.13,
    obfuscated_reason = "n < 5"
  )

  expect_equal(res, expected_res)
})

test_that("censored_value works", {
  res <- data.frame(
    n = 4,
    total = 20
  ) |>
    dplyr::mutate(prop = n / total) |>
    obfuscate_data(censored_value = 0) |>
    tibble::as_tibble()

  expected_res <- tibble::tribble(
    ~n, ~total, ~prop,
    0,  20,     0
  )

  expect_equal(res, expected_res)

  res <- data.frame(
    n = 4,
    total = 20
  ) |>
    dplyr::mutate(prop = n / total) |>
    obfuscate_data(censored_value = NA) |>
    tibble::as_tibble()

  expected_res <- tibble::tribble(
    ~n, ~total, ~prop,
    0,  20,     NA_real_
  )

  expect_equal(res, expected_res)

  res <- data.frame(
    unit = "a",
    n = c(3, 4, 10),
    total = 17
  ) |>
    dplyr::mutate(prop = n / total) |>
    dplyr::group_by(unit) |>
    obfuscate_data(censored_value = -1) |>
    tibble::as_tibble()

  expected_res <- tibble::tribble(
    ~unit, ~n, ~total, ~prop,
    "a",  0,     20,    -1,
    "a",  0,     20,    -1,
    "a", 10,     20,    -1
  )

  expect_equal(res, expected_res)

})

test_that("round_statistics_digits list works", {
  data_aggregated <- tibble::tribble(
    ~unit, ~group, ~x_mean, ~x_sd, ~total,
    1, "a", 1.0503, 0.26956, 51,
    1, "b", 0.6725, 0.17612, 45,
    2, "a", 0.5021, 0.46552, 32,
    2, "b", 1.1357, 0.54783, 78
  )

  res <- obfuscate_data(
    data = data_aggregated,
    statistics_vars = c("x_mean", "x_sd"),
    round_statistics_digits = list(x_mean = 2, x_sd = 1),
    round_statistics_vars = TRUE,
    total_var = "total"
  )

  expected_res <- tibble::tribble(
    ~unit, ~group, ~x_mean, ~x_sd, ~total,
    1, "a", 1.05, 0.3, 50,
    1, "b", 0.67, 0.2, 50,
    2, "a", 0.5, 0.5, 30,
    2, "b", 1.14, 0.5, 80
  )

  expect_equal(res, expected_res)
})

test_that("obfuscate_data works", {
  res <- tibble::tribble(
    ~unit, ~n,  ~total, ~prop,
    "a",   4,   200,    4 / 200,
    "a",   196, 200,    196 / 200,
    "b",   4,   44,     4 / 44,
    "b",   40,  44,     40 / 44
  ) |>
    obfuscate_data(
      liberal_obfuscation = TRUE,
      group_var = "unit",
      add_reason_col = TRUE
    ) |>
    dplyr::ungroup()

  expected_res <- tibble::tribble(
    ~unit, ~n,  ~total, ~prop, ~obfuscated_reason,
    "a",   0,   200,    0,     "rounded to nearest 5%",
    "a",   200, 200,    1,     "rounded to nearest 5%",
    "b",   0,   40,     0,     "n < 5",
    "b",   40,  40,     0,     "n < 5"
  )

  expect_equal(res, expected_res)
})


test_that("prop_scale works", {
  res <- tibble::tribble(
    ~n, ~total, ~prop,
    6,  23,     100 * (6 / 23),
    17, 23,     100 * (17 / 23)
  ) |>
    obfuscate_data(
      prop_scale = 100
    )

  expected_res <- tibble::tribble(
    ~n, ~total, ~prop,
    10, 20,     26,
    20, 20,     74
  )

  expect_equal(res, expected_res)

  res <- tibble::tribble(
    ~n, ~total, ~prop,
    6,  23,     6 / 23,
    17, 23,     17 / 23
  ) |>
    obfuscate_data(
      prop_scale = 1
    )

  expected_res <- tibble::tribble(
    ~n, ~total, ~prop,
    10, 20,     0.26,
    20, 20,     0.74
  )

  expect_equal(res, expected_res)

  expect_error(
    tibble::tribble(
      ~n, ~total, ~prop,
      6,  23,     6 / 23,
      17, 23,     17 / 23
    ) |>
      obfuscate_data(
        prop_scale = -1
      )
  )
})
