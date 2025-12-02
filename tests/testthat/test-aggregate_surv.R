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
      tibble::tibble(estimate = 0.5, cum_events = 1, total = 2)
    )

  get_surv_value(
    df = df,
    time = 1,
    time_col = "time",
    event = "event",
    censored_value = -1,
    obfuscate_data = TRUE
  ) |>
    expect_equal(
      tibble::tibble(
        estimate = -1,
        cum_events = 1,
        total = 2,
        obfuscated_reason = "N < 15"
      )
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
        ~county, ~gender, ~estimate, ~cum_events, ~total,
        "01", "1", 0.48, 13, 25,
        "01", "2", 0.52, 12, 25,
        "02", "1", 1, 0, 25,
        "02", "2", 1, 0, 25,
        "01", "Alla", 0.5, 25, 50,
        "02", "Alla", 1, 0, 50,
        "Riket", "1", 0.74, 13, 50,
        "Riket", "2", 0.76, 12, 50,
        "Riket", "Alla", 0.75, 25, 100
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
        ~county, ~gender, ~estimate, ~cum_events, ~total,
        "01", 1L, 0.48, 13, 25,
        "01", 2L, 0.52, 12, 25,
        "02", 1L, 1, 0, 25,
        "02", 2L, 1, 0, 25,
        "Riket", 1L, 0.74, 13, 50,
        "Riket", 2L, 0.76, 12, 50
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
        ~county, ~gender, ~estimate, ~cum_events, ~total,
        "01", 1L, 0.48, 13, 25,
        "01", 2L, 0.52, 12, 25,
        "02", 1L, 1, 0, 25,
        "02", 2L, 1, 0, 25
      )
    )

  get_surv_value(
    df = df,
    time = 1,
    time_col = "time",
    event_col = "event",
    obfuscate_data = TRUE,
    censored_limit = 200
  ) |>
    expect_equal(
      tibble::tibble(
        estimate = NA_real_,
        cum_events = 25,
        total = 100,
        obfuscated_reason = "N < 200"
      )
    )

  df <- tibble::tibble(
    time = 100,
    status = rep(NA, 100)
  )

  expect_message(
    get_surv_value(
      df = df,
      time = 100,
      time_col = "time",
      event_col = "status"
    ) |>
      expect_equal(
        tibble::tibble(
          estimate = NA,
          total = 0,
          cum_events = 0
        )
      ),
    regexp = paste0(
      "Removing .+ missing observations"
    )
  )

  df <- tibble::tibble(
    time = 100,
    status = rep(c(2, NA), each = 50),
    county = rep(1:2, each = 50)
  )

  get_surv_value(
    df = df,
    time = 100,
    time_col = "time",
    event_col = "status",
    group_cols = "county"
  ) |>
    expect_equal(
      tibble::tribble(
        ~county, ~estimate, ~cum_events, ~total,
        "1", 0, 50, 50,
        "2", NA, 0, 0,
        "Riket", 0, 50, 50
      )
    )
})
