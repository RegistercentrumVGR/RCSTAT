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
  dfl <- locf(
    df = df, vars = c("x", "z"),
    groupby = c("id", "a"), orderby = "t"
  )
  testthat::expect_identical(dfl, dfres)

  data.table::setDT(dt)
  # data.table locf, group by 2 variables, order by 1
  dfldt <- locfdt(
    dt = dt, vars = c("x", "z"),
    groupby = c("id", "a"), orderby = "t"
  )
  testthat::expect_identical(dfldt, data.table::setDT(dfres))


  # locf, keep 1 row per group, group by 2 variables, order by 1
  dfl <- locf(df,
    vars = c("x", "z"),
    groupby = c("id", "a"),
    orderby = "t", slice = TRUE
  )
  testthat::expect_identical(dfl, dfsres)

  dt <- tibble::as_tibble(df)
  data.table::setDT(dt)
  # data.table locf, keep 1 row per group, group by 2 variables, order by 1
  dfldt <- locfdt(
    dt,
    vars = c("x", "z"),
    groupby = c("id", "a"),
    orderby = "t", slice = TRUE,
    return_tibble = FALSE
  )
  testthat::expect_identical(dfldt, data.table::setDT(dfsres))

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
  dfl <- locf(df,
    vars = c("x", "z"),
    groupby = "id",
    orderby = "t",
    slice = FALSE
  )
  testthat::expect_identical(dfl, dfres)

  data.table::setDT(dt)
  # data.table locf, group by 1 variable, order by 1
  dfldt <-
    locfdt(
      dt,
      vars = c("x", "z"),
      groupby = "id",
      orderby = "t",
      slice = FALSE,
      return_tibble = FALSE
    )
  testthat::expect_identical(dfldt, data.table::setDT(dfres))

  df <-
    dplyr::bind_rows(
      data.frame(
        id = rep(1:3, each = 3),
        date = rep(data.table::as.IDate(paste0("2023-0", 6:8, "-25")),
          times = 3
        ),
        val = c(
          NA, 1, NA,
          1, NA, NA,
          NA, 1, NA
        )
      ),
      data.frame(
        id = c(3, 3),
        val = c(NA, NA),
        date = c(
          data.table::as.IDate("2023-08-24"),
          data.table::as.IDate("2023-09-26")
        )
      )
    ) |>
    data.table::setorderv(cols = c("id", "date"))

  df_res <- data.table::copy(df)
  df_res[["val"]] <- c(
    NA, 1, 1,
    1, 1, NA,
    NA, 1, 1,
    1, NA
  )

  data.table::setDT(df)
  data.table::setDT(df_res)

  dfldt <- locfdt(data.table::copy(df),
    vars = "val",
    groupby = "id",
    orderby = "date",
    window_type = "months",
    window_size = 1
  )

  testthat::expect_identical(dfldt, df_res)

  dfldt <- locfdt(data.table::copy(df),
    vars = "val",
    groupby = "id",
    orderby = "date",
    window_type = "days",
    window_size = 31
  )

  testthat::expect_identical(dfldt, df_res)
  df <-
    dplyr::bind_rows(
      data.frame(
        id = rep(1:3, each = 3),
        date = rep(data.table::as.IDate(paste0("202", 1:3, "-01-01")),
          times = 3
        ),
        val = c(
          NA, 1, NA,
          1, NA, NA,
          NA, 1, NA
        )
      ),
      data.frame(
        id = c(3, 3),
        val = c(NA, NA),
        date = c(
          data.table::as.IDate("2024-01-01"),
          data.table::as.IDate("2025-01-10")
        )
      )
    ) |>
    data.table::setorderv(cols = c("id", "date"))

  df_res <- data.table::copy(df)
  df_res[["val"]] <- c(
    NA, 1, 1,
    1, 1, NA,
    NA, 1, 1,
    1, NA
  )

  data.table::setDT(df)
  data.table::setDT(df_res)

  dfldt <- locfdt(data.table::copy(df),
    vars = "val",
    groupby = "id",
    orderby = "date",
    window_type = "years",
    window_size = 1
  )
})
