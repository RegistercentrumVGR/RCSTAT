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
  dfsres <- dfres[seq(2, 8, by = 2), ]

  # Need to make explicit copy of df
  # since it will be modified in locfdt
  set.seed(1L)
  # Scramble
  df <- df[sample.int(n = 8), ]
  dt <- tibble::as_tibble(df)

  # locf, group by 2 variables, order by 1
  dfl <- locf(df = df, vars = c("x", "z"),
              groupby = c("id", "a"), orderby = "t")
  testthat::expect_identical(dfl, dfres)

  # data.table locf, group by 2 variables, order by 1
  dfldt <- locfdt(dt = dt, vars = c("x", "z"),
                  groupby = c("id", "a"), orderby = "t")
  testthat::expect_identical(dfldt, dfres)

  # locf, keep 1 row per group, group by 2 variables, order by 1
  dfl <- locf(df, vars = c("x", "z"),
              groupby = c("id", "a"),
              orderby = "t", slice = TRUE)
  testthat::expect_identical(dfl, dfsres)

  # data.table locf, keep 1 row per group, group by 2 variables, order by 1
  dfldt <- locfdt(dt, vars = c("x", "z"),
                  groupby = c("id", "a"),
                  orderby = "t", slice = TRUE)
  testthat::expect_identical(dfldt, dfsres)

  #---- TEST WITH ONE GROUPING VARIABLE ----#
  # Something is going on here that I do not
  # understand - have to re-define the test-data
  df <- tibble::tibble(
    id = rep(1:2, each = 4),
    a = rep(1:4, each = 2),
    t = 1:8,
    x = c(NA, 1, NA, NA, 2, NA, 3, 3),
    z = c(2, NA, NA, NA, NA, 1, 3, 3)
  )
  df <- df[sample.int(n = 8), ]
  dt <- tibble::as_tibble(df)
  # New result expected
  dfres <- tibble::tibble(
    id = rep(1:2, each = 4),
    a = rep(1:4, each = 2),
    t = 1:8,
    x = c(NA, 1, 1, 1, 2, 2, 3, 3),
    z = c(2, 2, 2, 2, NA, 1, 3, 3)
  )
  # locf, group by 1 variable, order by 1
  dfl <- locf(df, vars = c("x", "z"),
              groupby = "id",
              orderby = "t",
              slice = FALSE)
  testthat::expect_identical(dfl, dfres)

  # data.table locf, group by 1 variable, order by 1
  dfldt <- locfdt(dt, vars = c("x", "z"),
                  groupby = "id",
                  orderby = "t",
                  slice = FALSE)
  testthat::expect_identical(dfldt, dfres)

})
