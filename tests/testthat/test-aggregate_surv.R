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
    include_ci = TRUE
  ) |>
    expect_equal(
      tibble::tibble(
        estimate = 0.5,
        conf.low = 0.125,
        conf.high = 1,
        cum_events = 1,
        total = 2
      ),
      tolerance = 0.001
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

  df <- data.frame(
    id = 1:10,
    time = c(5, 6, 7, 4, 10, 12, 3, 8, 9, 11),
    status = factor(
      c(1, 0, 1, 2, 1, 0, 0, 1, 0, 1),
      levels = 0:2,
      labels = c("cens", "rev", "death")
    )
  )

  get_surv_value(
    df = df,
    time = 10,
    time_col = "time",
    event_col = "status",
    obfuscate_data = FALSE
  ) |>
    expect_equal(
      tibble::tribble(
        ~estimate, ~cum_events, ~total,
        0.3457, 4, 10
      ),
      tolerance = 0.001
    )

  get_surv_value(
    df = df,
    time = 10,
    time_col = "time",
    event_col = "status",
    estimate = "event",
    obfuscate_data = FALSE
  ) |>
    expect_equal(
      tibble::tribble(
        ~estimate, ~cum_events, ~total,
        1 - 0.3457, 4, 10
      ),
      tolerance = 0.001
    )

  # CIs should also be flipped (inverted and swapped) when estimate = "event"
  surv_ci <- get_surv_value(
    df = df,
    time = 10,
    time_col = "time",
    event_col = "status",
    estimate = "survival",
    include_ci = TRUE,
    obfuscate_data = FALSE
  )

  event_ci <- get_surv_value(
    df = df,
    time = 10,
    time_col = "time",
    event_col = "status",
    estimate = "event",
    include_ci = TRUE,
    obfuscate_data = FALSE
  )

  event_ci |>
    expect_equal(
      tibble::tibble(
        estimate = 1 - surv_ci$estimate,
        conf.low = 1 - surv_ci$conf.high,
        conf.high = 1 - surv_ci$conf.low,
        cum_events = surv_ci$cum_events,
        total = surv_ci$total
      )
    )


  # time before the first event/censoring time, no group_cols
  df <- data.frame(time = c(1, 100), event = c(1, 0))

  get_surv_value(
    df = df,
    time = 0.5,
    time_col = "time",
    event_col = "event"
  ) |>
    expect_equal(
      tibble::tibble(estimate = NA, cum_events = 0, total = 0)
    )

  # time before the first event/censoring time in every stratum
  df <- data.frame(
    time = c(5, 6, 7, 4, 10, 12, 3, 8, 9, 11),
    status = c(1, 0, 1, 1, 1, 0, 0, 1, 0, 1),
    grp = rep(c("a", "b"), each = 5)
  )

  get_surv_value(
    df = df,
    time = 1,
    time_col = "time",
    event_col = "status",
    group_cols = "grp"
  ) |>
    expect_equal(
      tibble::tribble(
        ~grp, ~estimate, ~cum_events, ~total,
        "a", NA_real_, 0, 0,
        "b", NA_real_, 0, 0
      )
    )
})

test_that("fit_surv works", {
  df <- data.frame(time = c(1, 2, 3, 4), status = c(1, 0, 1, 0))

  fit <- fit_surv(df, time_col = "time", event_col = "status")

  expect_false("state" %in% names(fit$res))
  expect_gt(nrow(fit$res), 0)

  df <- data.frame(
    time = c(5, 6, 7, 4, 10, 12, 3, 8, 9, 11),
    status = factor(
      c(1, 0, 1, 2, 1, 0, 0, 1, 0, 1),
      levels = 0:2,
      labels = c("cens", "rev", "death")
    )
  )

  fit <- fit_surv(df, time_col = "time", event_col = "status")

  expect_gt(nrow(fit$res), 0)
  expect_true(all(fit$res$state == "(s0)"))
})

test_that("get_surv_curve works", {
  df <- data.frame(
    id = 1:10,
    time = c(5, 6, 7, 4, 10, 12, 3, 8, 9, 11),
    status = factor(
      c(1, 0, 1, 2, 1, 0, 0, 1, 0, 1),
      levels = 0:2,
      labels = c("cens", "rev", "death")
    )
  )

  get_surv_curve(
    df = df,
    time_col = "time",
    event_col = "status"
  ) |>
    dplyr::mutate(dplyr::across(c("n.risk", "cum_events"), as.numeric)) |>
    expect_equal(
      tibble::tribble(
        ~time, ~estimate, ~n.risk, ~cum_events,
        3, 1.000, 10, 0,
        4, 0.889, 9, 0,
        5, 0.778, 8, 1,
        6, 0.778, 7, 2,
        7, 0.648, 6, 2,
        8, 0.519, 5, 3,
        9, 0.519, 4, 4,
        10, 0.346, 3, 4,
        11, 0.173, 2, 5,
        12, 0.173, 1, 6
      ),
      tolerance = 0.001
    )

  get_surv_curve(
    df = data.frame(time = numeric(0), status = numeric(0)),
    time_col = "time",
    event_col = "status"
  ) |>
    expect_equal(
      tibble::tibble(
        time = numeric(0),
        estimate = numeric(0),
        n.risk = numeric(0),
        cum_events = numeric(0)
      )
    )

  get_surv_curve(
    df = df,
    time_col = "time",
    event_col = "status",
    include_ci = TRUE
  ) |>
    dplyr::mutate(dplyr::across(c("n.risk", "cum_events"), as.numeric)) |>
    expect_equal(
      tibble::tribble(
        ~time, ~estimate, ~conf.low, ~conf.high, ~n.risk, ~cum_events,
        3, 1, 1, 1, 10, 0,
        4, 0.889, 0.706, 1, 9, 0,
        5, 0.778, 0.549, 1, 8, 1,
        6, 0.778, 0.549, 1, 7, 2,
        7, 0.648, 0.393, 1, 6, 2,
        8, 0.519, 0.267, 1, 5, 3,
        9, 0.519, 0.267, 1, 4, 4,
        10, 0.346, 0.122, 0.978, 3, 4,
        11, 0.173, 0.031, 0.978, 2, 5,
        12, 0.173, 0.031, 0.978, 1, 6
      ),
      tolerance = 0.01
    )

  # CIs should also be flipped (inverted and swapped) when estimate = "event"
  surv_curve_ci <- get_surv_curve(
    df = df,
    time_col = "time",
    event_col = "status",
    include_ci = TRUE
  ) |>
    dplyr::mutate(dplyr::across(c("n.risk", "cum_events"), as.numeric))

  event_curve_ci <- get_surv_curve(
    df = df,
    time_col = "time",
    event_col = "status",
    estimate = "event",
    include_ci = TRUE
  ) |>
    dplyr::mutate(dplyr::across(c("n.risk", "cum_events"), as.numeric))

  event_curve_ci |>
    expect_equal(
      surv_curve_ci |>
        dplyr::mutate(
          estimate = 1 - .data$estimate,
          new_conf.low = 1 - .data$conf.high,
          new_conf.high = 1 - .data$conf.low
        ) |>
        dplyr::select(-"conf.low", -"conf.high") |>
        dplyr::rename(conf.low = "new_conf.low", conf.high = "new_conf.high") |>
        dplyr::relocate("conf.low", "conf.high", .after = "estimate")
    )
})

test_that("unnest_strata works", {
  res <- tibble::tibble(
    strata = c(
      "county=01, gender=man",
      "county=01, gender=woman",
      "county=02, gender=man"
    ),
    estimate = c(0.48, 0.52, 1)
  )

  model_df <- tibble::tibble(
    county = c("01", "02"),
    gender = factor(c("man", "woman"))
  )

  unnest_strata(res, c("county", "gender"), model_df) |>
    dplyr::select(-dplyr::matches("^strata_\\d+")) |>
    expect_equal(
      tibble::tibble(
        estimate = c(0.48, 0.52, 1),
        county = c("01", "01", "02"),
        gender = factor(c("man", "woman", "man"))
      )
    )

  res_single <- tibble::tibble(
    strata = c("gender=man", "gender=woman"),
    estimate = c(0.4, 0.6)
  )

  unnest_strata(res_single, "gender", model_df) |>
    dplyr::select(-dplyr::matches("^strata_\\d+")) |>
    expect_equal(
      tibble::tibble(
        estimate = c(0.4, 0.6),
        gender = factor(c("man", "woman"))
      )
    )

  # no group_cols returns res unmodified
  res |>
    unnest_strata(NULL, model_df) |>
    expect_equal(res)
})
