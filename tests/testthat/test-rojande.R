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
    obfuscate_data(add_reason_col = T) |>
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
    obfuscate_data(add_reason_col = T, liberal_obfuscation = F) |>
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
    obfuscate_data(add_reason_col = T, liberal_obfuscation = F) |>
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
    obfuscate_data(add_reason_col = T) |>
    tibble::as_tibble()

  expected_res <- tibble::tribble(
    ~n, ~total, ~prop, ~obfuscated_reason,
    0,   50,     0.05,     "rounded to nearest 5%",
    40,  50,     0.95,     "rounded to nearest 5%",
    250, 250,    1,        NA,
    0,   250,    0,        NA
  )

  expect_equal(res, expected_res)

  res <- data.frame(
    group = rep(c("a", "b"), each = 3),
    n = c(23, 23, 4, 20, 20, 4),
    total = c(rep(50, 3), rep(44, 3))
  ) |>
    dplyr::mutate(prop = n / total) |>
    dplyr::group_by(group) |>
    obfuscate_data(add_reason_col = T) |>
    dplyr::ungroup() |>
    tibble::as_tibble()

  expected_res <- tibble::tribble(
    ~group, ~n, ~total, ~prop, ~obfuscated_reason,
    "a",    20,  50,     0.46,     NA,
    "a",    20,  50,     0.46,     NA,
    "a",    0,   50,     0.1,      "rounded to nearest 5%",
    "b",    20,  40,     0,        "n < 5",
    "b",    20,  40,     0,        "n < 5",
    "b",    0,   40,     0,        "n < 5"
  )

  expect_equal(res, expected_res)

})
