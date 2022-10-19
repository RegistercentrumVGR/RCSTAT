test_that("locf works", {
  # Make test data
  df <- tibble::tibble(
    id = rep(1:2, each = 4),
    a = rep(1:4, each = 2),
    t = 1:8,
    x = c(NA, 1, NA, NA, 2, NA, 3, 3),
    z = c(2, NA, NA, NA, NA, 1, 3, 3)
  )
  # Make intended result
  dfres <- tibble::tibble(
    id = rep(1:2, each = 4),
    a = rep(1:4, each = 2),
    t = 1:8,
    x = c(NA, 1, NA, NA, 2, 2, 3, 3),
    z = c(2, 2, NA, NA, NA, 1, 3, 3)
  )
  dfsres <- dfres[seq(2, 8, by = 2),]
  # Scramble
  df <- df[sample(1:nrow(df), size = nrow(df)),]

  dfl <- locf(df, vars = c("x", "z"), groupby = c("id", "a"), orderby = "t")
  testthat::expect_equal(dfl, dfres)

  dfl <- locfdt(df, vars = c("x", "z"), groupby = c("id", "a"), orderby = "t")
  testthat::expect_equal(dfl, dfres)

  dfl <- locf(df, vars = c("x", "z"), groupby = c("id", "a"), orderby = "t", slice = TRUE)
  testthat::expect_equal(dfl, dfsres)

  dfl <- locfdt(df, vars = c("x", "z"), groupby = c("id", "a"), orderby = "t", slice = TRUE)
  testthat::expect_equal(dfl, dfsres)
})
