test_that("get_surv_value works", {
  df <- data.frame(
    time = c(1, 100),
    event = c(1, 0)
  )

  get_surv_value(
    df = df,
    time = 1,
    time_col = "time",
    event = "event"
  ) |>
    expect_equal(
      tibble::tibble(estimate = 0.5)
    )

  df <- data.frame(
    time = rep(c(1, 100), each = 50),
    event = c(1, 0),
    county = c("01", "02"),
    gender = rep(1:2, each = 2)
  )

  get_surv_value(
    df = df,
    time = 1,
    time_col = "time",
    event = "event",
    group_cols = c("county", "gender")
  ) |>
    expect_equal(
      tibble::tribble(
        ~county, ~gender, ~estimate,
        "01", "1   ", 0.48,
        "01", "2   ", 0.52,
        "01", "Alla", 0.5,
        "02", "1   ", 1,
        "02", "2   ", 1,
        "02", "Alla", 1,
        "Riket", "1   ", 0.74,
        "Riket", "2   ", 0.76,
        "Riket", "Alla", 0.75
      )
    )

  get_surv_value(
    df = df,
    time = 1,
    time_col = "time",
    event = "event",
    group_cols = c("county", "gender"),
    marginal_cols = "county"
  ) |>
    expect_equal(
      tibble::tribble(
        ~county, ~gender, ~estimate,
        "01", 1L, 0.48,
        "01", 2L, 0.52,
        "02", 1L, 1,
        "02", 2L, 1,
        "Riket", 1L, 0.74,
        "Riket", 2L, 0.76
      )
    )

  get_surv_value(
    df = df,
    time = 1,
    time_col = "time",
    event = "event",
    group_cols = c("county", "gender"),
    marginal_cols = NULL
  ) |>
    expect_equal(
      tibble::tribble(
        ~county, ~gender, ~estimate,
        "01", 1L, 0.48,
        "01", 2L, 0.52,
        "02", 1L, 1,
        "02", 2L, 1
      )
    )


})
