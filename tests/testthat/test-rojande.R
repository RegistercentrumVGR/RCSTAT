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
    3,  NA,     NA,     NA
  )
  # Test group_means on data.frame and data.table
  res <- group_proportions(df, group_by = "a", obfuscate = TRUE)

  expect_equal(res, expected_res)

  res <- group_proportions(df, group_by = c("a", "b"), obfuscate = TRUE)
  expected_res <- tibble::tribble(
    ~a, ~b, ~Count, ~Total, ~Proportion,
    1,  1,  10,     30,     0.52,
    1,  2,  10,     30,     0.48,
    2,  1,  NA,     NA,     NA,
    2,  2,  NA,     NA,     NA,
    3,  1,  NA,     NA,     NA,
    3,  2,  NA,     NA,     NA
  )
  expect_equal(res, expected_res)
})
