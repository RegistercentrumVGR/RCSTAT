test_that("get_aggregate_value works", {
  df <- tibble::tribble(
    ~County, ~unit, ~y, ~z,
    "a", 1, 1, 2,
    "a", 1, 0, 3,
    "b", 2, 1, 4,
    "b", 2, 0, 5,
    "b", 2, 0, 6,
    "c", 3, 1, 7
  )

  res <- get_aggregate_value(
    df,
    group_cols = c("County", "unit"),
    vars = list(prop = "y")
  ) |>
    tibble::as_tibble()

  expected_res <- tibble::tribble(
    ~y_n, ~y_prop, ~total, ~County, ~unit,
    3, 0.5, 6L, "Riket", "Alla",
    1, 0.5, 2L, "a", "Alla",
    1, 1 / 3, 3L, "b", "Alla",
    1, 1, 1L, "c", "Alla",
    1, 0.5, 2L, "Riket", "1",
    1, 1 / 3, 3L, "Riket", "2",
    1, 1, 1L, "Riket", "3",
    1, 0.5, 2L, "a", "1",
    1, 1 / 3, 3L, "b", "2",
    1, 1, 1L, "c", "3"
  )

  expect_identical(res, expected_res)

  res <- get_aggregate_value(
    df,
    group_cols = c("County", "unit"),
    vars = list(mean = "z")
  ) |>
    tibble::as_tibble()

  expected_res <- tibble::tribble(
    ~z_mean, ~z_std, ~z_total_non_missing, ~total, ~County, ~unit,
    4.5, 1.87082869338697, 6L, 6L, "Riket", "Alla",
    2.5, 0.707106781186548, 2L, 2L, "a", "Alla",
    5, 1, 3L, 3L, "b", "Alla",
    7, NA, 1L, 1L, "c", "Alla",
    2.5, 0.707106781186548, 2L, 2L, "Riket", "1",
    5, 1, 3L, 3L, "Riket", "2",
    7, NA, 1L, 1L, "Riket", "3",
    2.5, 0.707106781186548, 2L, 2L, "a", "1",
    5, 1, 3L, 3L, "b", "2",
    7, NA, 1L, 1L, "c", "3"
  )

  expect_equal(res, expected_res)

  df <- tibble::tribble(
    ~County, ~y,
    "a", 10,
    "b", 5
  )

  res <- get_aggregate_value(
    df,
    group_cols = "County",
    vars = list(mean = "y")
  )

  expect_setequal(
    res$County, c("Riket", "a", "b")
  )

  expect_error({
    get_aggregate_value(
      df,
      group_cols = "unit",
      vars = list(mean = c("y", "z"))
    )
  })

  expect_error({
    get_aggregate_value(
      df,
      group_cols = "abc",
      vars = list(mean = "z")
    )
  })

  expect_error({
    get_aggregate_value(
      df,
      group_cols = "unit",
      vars = list(mean = "abc")
    )
  })

  df <- data.frame(
    unit = 1,
    y = "abc"
  )

  expect_error({
    get_aggregate_value(
      df,
      group_cols = "unit",
      vars = list(mean = "y")
    )
  })

  df <- data.frame(
    unit = 1,
    y = 1:2
  )

  expect_error({
    get_aggregate_value(
      df,
      group_cols = "unit",
      vars = list(abc = "y")
    )
  })

  df <- data.frame(
    unit = NULL, y = NULL
  )

  expect_error({
    get_aggregate_value(
      df,
      group_cols = "unit",
      vars = list(mean = "y")
    )
  })

  res <- data.frame(
    x = c(sample(1:10, 4), rep(NA, 11)),
    y = 1
  ) |>
    get_aggregate_value(
      group_cols = "y",
      vars = list(mean = "x"),
      obfuscate = TRUE
    ) |>
    tibble::as.tibble()

  expected_res <- tibble::tribble(
    ~x_mean, ~x_std, ~x_total_non_missing, ~total, ~y,
    NA_integer_, NA_integer_, 0, 20, "Alla",
    NA_integer_, NA_integer_, 0, 20, "1"
  )

  expect_equal(res, expected_res)

  expect_error({
    get_aggregate_value(
      df,
      vars = list(mean = "County")
    )
  })

})

test_that("prop_count works", {

  df <- data.frame(
    unit = 1,
    category = sample(letters[1:3], size = 100, replace = TRUE)
  )

  res <- get_aggregate_value(
    df,
    group_cols = "unit",
    vars = list(prop_count = "category"),
    include_missing = FALSE
  )

  res |>
    dplyr::mutate(
      sum = rowSums(
        dplyr::across(
          tidyselect::matches("category_n")
        )
      ),
      ok = .data[["total"]] == .data[["sum"]]
    ) |>
    dplyr::pull(ok) |>
    all() |>
    expect_true()

  res |>
    dplyr::mutate(
      sum = rowSums(
        dplyr::across(
          tidyselect::matches("category_prop")
        )
      ),
      ok = .data[["sum"]] == 1
    ) |>
    dplyr::pull(.data[["ok"]]) |>
    all() |>
    expect_true()

  expect_setequal(
    names(res),
    c(
      paste0("category_n_", letters[1:3]),
      paste0("category_prop_", letters[1:3]),
      "unit",
      "total",
      "category_total"
    )
  )

  df <- data.frame(
    unit = 1,
    category = sample(c(letters[1:3], NA), size = 100, replace = TRUE)
  )

  res <- get_aggregate_value(
    df,
    group_cols = "unit",
    vars = list(prop_count = "category"),
    include_missing = FALSE
  )

  res |>
    dplyr::mutate(
      sum = rowSums(
        dplyr::across(
          tidyselect::matches("category_n")
        )
      ),
      ok = .data[["category_total"]] == .data[["sum"]]
    ) |>
    dplyr::pull(ok) |>
    all() |>
    expect_true()

  res |>
    dplyr::mutate(
      sum = rowSums(
        dplyr::across(
          tidyselect::matches("category_prop")
        )
      ),
      ok = .data[["sum"]] == 1
    ) |>
    dplyr::pull(.data[["ok"]]) |>
    all() |>
    expect_true()

  res <- get_aggregate_value(
    df,
    group_cols = "unit",
    vars = list(prop_count = "category"),
    include_missing = TRUE
  )

  res |>
    dplyr::mutate(
      sum = rowSums(
        dplyr::across(
          tidyselect::matches("category_n")
        )
      ),
      ok = .data[["total"]] == .data[["sum"]]
    ) |>
    dplyr::pull(ok) |>
    all() |>
    expect_true()

  res |>
    dplyr::mutate(
      sum = rowSums(
        dplyr::across(
          tidyselect::matches("category_prop")
        )
      ),
      ok = .data[["sum"]] == 1
    ) |>
    dplyr::pull(.data[["ok"]]) |>
    all() |>
    expect_true()

  expect_setequal(
    names(res)[grepl("category_(n|prop)", names(res))],
    c(
      paste0("category_n_", c(letters[1:3], NA)),
      paste0("category_prop_", c(letters[1:3], NA))
    )
  )

  df <- data.frame(
    unit = 1,
    category = sample(letters[1:3], size = 14, replace = TRUE)
  )

  res <- get_aggregate_value(
    df,
    group_cols = "unit",
    vars = list(prop_count = "category"),
    obfuscate_data = TRUE
  )

  res |>
    dplyr::mutate(
      ok = dplyr::if_all(tidyselect::matches("category_prop"), ~ .x == 0)
    ) |>
    dplyr::pull(.data[["ok"]]) |>
    all() |>
    expect_true()

  res <- get_aggregate_value(
    df,
    group_cols = "unit",
    vars = list(prop_count = "category"),
    obfuscate_data = TRUE,
    censored_value = NA
  )

  res |>
    dplyr::mutate(
      ok = dplyr::if_all(tidyselect::matches("category_prop"), is.na)
    ) |>
    dplyr::pull(.data[["ok"]]) |>
    all() |>
    expect_true()

  df <- data.frame(
    unit = c(rep("a", 3), rep("b", 3)),
    category = c(1:3, rep(NA, 3))
  )

  expect_no_error({
    get_aggregate_value(
      df,
      group_cols = "unit",
      vars = list(prop_count = "category"),
      include_missing = FALSE
    )
  })

})
