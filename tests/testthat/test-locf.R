test_that("locf and locfdt works", {
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
  # Need to make explicit copy of df
  # since it will be modified in locfdt
  dt <- tibble::as_tibble(df)
  # Scramble
  df <- df[sample(1:nrow(df), size = nrow(df)),]

  dfl <- locf(df = df, vars = c("x", "z"), groupby = c("id", "a"), orderby = "t")
  testthat::expect_identical(dfl, dfres)

  dfldt <- locfdt(dt = dt, vars = c("x", "z"), groupby = c("id", "a"), orderby = "t")

  testthat::expect_identical(dfldt, dfres)
  testthat::expect_identical(dfldt, dfl)

  dfl <- locf(df, vars = c("x", "z"), groupby = c("id", "a"), orderby = "t", slice = TRUE)

  testthat::expect_identical(dfl, dfsres)

  dfldt <- locfdt(dt, vars = c("x", "z"), groupby = c("id", "a"), orderby = "t", slice = TRUE)

  testthat::expect_identical(dfldt, dfsres)
  testthat::expect_identical(dfldt, dfl)


})
