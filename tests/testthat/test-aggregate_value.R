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
    ~z_mean, ~z_std, ~total, ~County, ~unit,
    4.5, 1.87082869338697, 6L, "Riket", "Alla",
    2.5, 0.707106781186548, 2L, "a", "Alla",
    5, 1, 3L, "b", "Alla",
    7, NA, 1L, "c", "Alla",
    2.5, 0.707106781186548, 2L, "Riket", "1",
    5, 1, 3L, "Riket", "2",
    7, NA, 1L, "Riket", "3",
    2.5, 0.707106781186548, 2L, "a", "1",
    5, 1, 3L, "b", "2",
    7, NA, 1L, "c", "3"
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
      obfuscate = TRUE,
      include_missing = FALSE
    ) |>
    tibble::as_tibble()

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


  df <- data.frame(
    kategori = c(rep("a", 15), rep("b", 2)),
    x = 1:17
  )

  res <- get_aggregate_value(
    df = df,
    group_cols = "kategori",
    vars = list(mean = "x"),
    obfuscate_data = TRUE
  ) |>
    tibble::as_tibble()

  expected_res <- tibble::tribble(
    ~x_mean, ~x_std, ~total, ~kategori,
    9, 5.04975246918104, 20, "Alla",
    8, 4.47213595499958, 20, "a",
    NA, NA, 0, "b"
  )

  expect_equal(res, expected_res)

  res <- get_aggregate_value(
    df = df,
    group_cols = "kategori",
    vars = list(median = "x"),
    obfuscate_data = TRUE
  ) |>
    tibble::as_tibble() |>
    purrr::map(unname) |>
    dplyr::bind_rows()

  expected_res <- tibble::tribble(
    ~x_median, ~x_quant_5, ~x_quant_25, ~x_quant_75, ~x_quant_95, ~total,
    ~kategori,
    9, 1.8, 5, 13, 16.2, 20, "Alla",
    8, 1.7, 4.5, 11.5, 14.3, 20, "a",
    NA, NA, NA, NA, NA, 0, "b"
  )

  expect_equal(res, expected_res)

  df <- data.frame(
    kategori = c(rep("a", 20), rep("b", 15)),
    x = c(1:5, rep(NA, 15), 1:15)
  )

  res <- get_aggregate_value(
    df = df,
    group_cols = "kategori",
    vars = list(mean = "x"),
    obfuscate_data = TRUE,
    include_missing = FALSE
  ) |>
    tibble::as_tibble()

  expected_res <- tibble::tribble(
    ~x_mean, ~x_std, ~x_total_non_missing, ~total, ~kategori,
    6.75, 4.49414824199788, 20, 40, "Alla",
    NA, NA, 10, 20, "a",
    8, 4.47213595499958, 20, 20, "b"
  )

  expect_equal(res, expected_res)

  res <- get_aggregate_value(
    df = df,
    group_cols = "kategori",
    vars = list(mean = "x"),
    obfuscate_data = TRUE,
    include_missing = TRUE
  ) |>
    tibble::as_tibble()

  expected_res <- tibble::tribble(
    ~x_mean, ~x_std, ~total, ~kategori,
    6.75, 4.49414824199788, 40, "Alla",
    3, 1.58113883008419, 20, "a",
    8, 4.47213595499958, 20, "b"
  )

  expect_equal(res, expected_res)

  res <- get_aggregate_value(
    df = df,
    group_cols = "kategori",
    vars = list(median = "x"),
    obfuscate_data = TRUE,
    include_missing = FALSE
  ) |>
    tibble::as_tibble() |>
    purrr::map(unname) |>
    dplyr::bind_rows()

  expected_res <- tibble::tribble(
    ~x_median, ~x_quant_5, ~x_quant_25, ~x_quant_75, ~x_quant_95, ~x_total_non_missing, ~total, ~kategori,
    5.5, 1, 3, 10.25, 14.05, 20, 40, "Alla",
    NA, NA, NA, NA, NA, 10, 20, "a",
    8, 1.7, 4.5, 11.5, 14.3, 20, 20, "b"
  )

  expect_equal(res, expected_res)

  res <- get_aggregate_value(
    df = df,
    group_cols = "kategori",
    vars = list(median = "x"),
    obfuscate_data = TRUE,
    include_missing = TRUE
  ) |>
    tibble::as_tibble() |>
    purrr::map(unname) |>
    dplyr::bind_rows()

  expected_res <- tibble::tribble(
    ~x_median, ~x_quant_5, ~x_quant_25, ~x_quant_75, ~x_quant_95, ~total, ~kategori,
    5.5, 1, 3, 10.25, 14.05, 40, "Alla",
    3, 1.2, 2, 4, 4.8, 20, "a",
    8, 1.7, 4.5, 11.5, 14.3, 20, "b"
  )

  expect_equal(res, expected_res)

  df <- data.frame(
    unit = letters[1:3],
    x = rep(1:3, each = 3),
    y = rep(4:6, each = 3)
  )

  res <- get_aggregate_value(
    df = df,
    group_cols = "unit",
    vars = list(prop_count = "x"),
    pivot_prop_count = TRUE
  )

  expected <- tibble::tribble(
    ~total, ~unit, ~x, ~x_n, ~x_prop,
    9L, "Alla", "1", 3L, 0.333333333333333,
    9L, "Alla", "2", 3L, 0.333333333333333,
    9L, "Alla", "3", 3L, 0.333333333333333,
    3L, "a", "1", 1L, 0.333333333333333,
    3L, "a", "2", 1L, 0.333333333333333,
    3L, "a", "3", 1L, 0.333333333333333,
    3L, "b", "1", 1L, 0.333333333333333,
    3L, "b", "2", 1L, 0.333333333333333,
    3L, "b", "3", 1L, 0.333333333333333,
    3L, "c", "1", 1L, 0.333333333333333,
    3L, "c", "2", 1L, 0.333333333333333,
    3L, "c", "3", 1L, 0.333333333333333
  )

  expect_equal(res, expected)

  expect_warning(
    get_aggregate_value(
      df = df,
      group_cols = "unit",
      vars = list(prop_count = c("x", "y")),
      pivot_prop_count = TRUE
    )
  )

  expect_warning(
    get_aggregate_value(
      df = df,
      group_cols = "unit",
      vars = list(prop_count = "x", mean = "y"),
      pivot_prop_count = TRUE
    )
  )

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
      "category_total_non_missing"
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
      ok = .data[["category_total_non_missing"]] == .data[["sum"]]
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

  df <- data.frame(
    unit = rep(letters[1:2], each = 34),
    x = rep(c(rep(1, 15), rep(2, 15), rep(3, 4)), 2)
  )

  res <- get_aggregate_value(
    df = df,
    group_cols = "unit",
    vars = list(prop_count = "x"),
    obfuscate_data = TRUE,
    censored_value = NA
  ) |>
    tibble::as_tibble()

  expected_res <- tibble::tribble(
    ~x_n_1, ~x_n_2, ~x_n_3, ~x_prop_1, ~x_prop_2, ~x_prop_3, ~total, ~unit,
    30, 30, 10, 0.44, 0.44, 0.12, 70, "Alla",
    20, 20, 0, NA, NA, NA, 30, "a",
    20, 20, 0, NA, NA, NA, 30, "b",
  )

  expect_equal(res, expected_res)

  df <- data.frame(
    unit = rep(letters[1:2], each = 34),
    x = rep(c(rep(1, 14), rep(2, 16), rep(NA, 4)), 2)
  )

  res <- get_aggregate_value(
    df = df,
    group_cols = "unit",
    vars = list(prop_count = "x"),
    obfuscate_data = FALSE,
    censored_value = NA,
    include_missing = TRUE
  ) |>
    tibble::as_tibble()

  expected_res <- tibble::tribble(
    ~x_n_1, ~x_n_2, ~x_n_NA, ~x_prop_1, ~x_prop_2, ~x_prop_NA, ~total, ~unit,
    28L, 32L, 8L, 0.411764705882353, 0.470588235294118, 0.117647058823529, 68L,
    "Alla",
    14L, 16L, 4L, 0.411764705882353, 0.470588235294118, 0.117647058823529, 34L,
    "a",
    14L, 16L, 4L, 0.411764705882353, 0.470588235294118, 0.117647058823529, 34L,
    "b"
  )

  expect_equal(res, expected_res)

  res <- get_aggregate_value(
    df = df,
    group_cols = "unit",
    vars = list(prop_count = "x"),
    obfuscate_data = TRUE,
    censored_value = NA,
    include_missing = TRUE
  ) |>
    tibble::as_tibble()

  expected_res <- tibble::tribble(
    ~x_n_1, ~x_n_2, ~x_n_NA, ~x_prop_1, ~x_prop_2, ~x_prop_NA, ~total, ~unit,
    30, 30, 10, 0.41, 0.47, 0.12, 70, "Alla",
    10, 20, 0, NA, NA, NA, 30, "a",
    10, 20, 0, NA, NA, NA, 30, "b"
  )

  expect_equal(res, expected_res)

  res <- get_aggregate_value(
    df = df,
    group_cols = "unit",
    vars = list(prop_count = "x"),
    obfuscate_data = FALSE,
    censored_value = NA,
    include_missing = FALSE
  ) |>
    tibble::as_tibble()

  expected_res <- tibble::tribble(
    ~x_total_non_missing, ~x_n_1, ~x_n_2, ~x_prop_1, ~x_prop_2, ~total, ~unit,
    60L, 28L, 32L, 0.466666666666667, 0.533333333333333, 68L, "Alla",
    30L, 14L, 16L, 0.466666666666667, 0.533333333333333, 34L, "a",
    30L, 14L, 16L, 0.466666666666667, 0.533333333333333, 34L, "b"
  )

  expect_equal(res, expected_res)

  res <- get_aggregate_value(
    df = df,
    group_cols = "unit",
    vars = list(prop_count = "x"),
    obfuscate_data = TRUE,
    censored_value = NA,
    include_missing = FALSE
  ) |>
    tibble::as_tibble()

  expected_res <- tibble::tribble(
    ~x_total_non_missing, ~x_n_1, ~x_n_2, ~x_prop_1, ~x_prop_2, ~total, ~unit,
    60, 30, 30, 0.47, 0.53, 70L, "Alla",
    30, 10, 20, 0.47, 0.53, 30L, "a",
    30, 10, 20, 0.47, 0.53, 30L, "b"
  )

  expect_equal(res, expected_res)

  df <- data.frame(
    unit = 1:2,
    x = 1:20,
    y = 21:40
  )

  res <- get_aggregate_value(
    df = df,
    group_cols = "unit",
    vars = list(
      median = c("x", "y")
    ),
    obfuscate_data = TRUE,
    include_missing = FALSE
  ) |>
    tibble::as_tibble() |>
    purrr::map(unname) |>
    dplyr::bind_rows()

  expected_res <- tibble::tribble(
    ~x_median, ~x_quant_5, ~x_quant_25, ~x_quant_75, ~x_quant_95,
    ~x_total_non_missing, ~y_median, ~y_quant_5, ~y_quant_25, ~y_quant_75,
    ~y_quant_95, ~y_total_non_missing, ~total, ~unit,
    10.5, 1.95, 5.75, 15.25, 19.05, 20, 30.5, 21.95, 25.75, 35.25, 39.05, 20,
    20, "Alla",
    NA, NA, NA, NA, NA, 10, NA, NA, NA, NA, NA, 10, 10, "1",
    NA, NA, NA, NA, NA, 10, NA, NA, NA, NA, NA, 10, 10, "2"
  )

  expect_equal(res, expected_res)


  res <- get_aggregate_value(
    df = df,
    group_cols = "unit",
    vars = list(
      mean = c("x", "y")
    ),
    obfuscate_data = TRUE,
    include_missing = FALSE
  ) |>
    tibble::as_tibble()

  expected_res <- tibble::tribble(
    ~x_mean, ~x_std, ~x_total_non_missing, ~y_mean, ~y_std,
    ~y_total_non_missing, ~total, ~unit,
    10.5, 5.91607978309962, 20, 30.5, 5.91607978309962, 20, 20, "Alla",
    NA, NA, 10, NA, NA, 10, 10, "1",
    NA, NA, 10, NA, NA, 10, 10, "2"
  )

  expect_equal(res, expected_res)

  df <- data.frame(
    some_var = integer(0),
    unit = character(0)
  )

  expect_warning(
    res <- get_aggregate_value(
      df = df,
      group_cols = "unit",
      vars = list(prop_count = "some_var")
    )
  )

  expect_equal(
    res,
    data.frame(
      some_var = NA,
      total = 0,
      unit = "Alla"
    )
  )

  expect_warning(
    res <- get_aggregate_value(
      df = df,
      group_cols = NULL,
      vars = list(prop_count = "some_var")
    )
  )

  expect_equal(
    res,
    data.frame(
      some_var = NA,
      total = 0
    )
  )



  expect_warning(
    res <- get_aggregate_value(
      df = df,
      group_cols = NULL,
      vars = list(prop_count = "some_var"),
      pivot_prop_count = TRUE
    ),
    "You are trying to aggregate a data.frame that contains 0 rows"
  )

  expect_equal(
    res,
    data.frame(
      some_var = NA,
      total = 0
    )
  )

  df <- data.frame(
    unit = c(rep(1, 10), rep(2, 10)),
    var = c(rep(NA, 10), rep(1, 5), rep(2, 5))
  )

  expect_no_error(
    res <- get_aggregate_value(
      df = df,
      group_cols = "unit",
      vars = list(prop_count = "var"),
      pivot_prop_count = TRUE,
      include_missing = FALSE
    )
  )

  expect_snapshot(res)

  df <- data.frame(
    unit = c(rep(1, 10), rep(2, 10)),
    var = c(rep(NA, 10), rep("a", 5), rep("b", 5))
  ) |>
    dplyr::mutate(var = factor(.data$var, levels = letters[1:3]))

  res <- get_aggregate_value(
    df = df,
    group_cols = "unit",
    vars = list(prop_count = "var"),
    pivot_prop_count = TRUE
  )

  expect_snapshot(res)

  res <- get_aggregate_value(
    df = df,
    group_cols = "unit",
    vars = list(prop_count = "var"),
    pivot_prop_count = TRUE,
    include_missing = FALSE
  )

  expect_snapshot(res)

})

test_that("get_aggregate_value works with no data", {

  df <- data.frame(x = integer(0), unit = character(0))

  get_aggregate_value(
    df,
    group_cols = "unit",
    vars = list(prop = "x")
  ) |>
    expect_no_error() |>
    expect_warning()

  get_aggregate_value(
    df,
    group_cols = "unit",
    vars = list(mean = "x")
  ) |>
    expect_no_error() |>
    expect_warning()

  get_aggregate_value(
    df,
    group_cols = "unit",
    vars = list(median = "x")
  ) |>
    expect_no_error() |>
    expect_warning()

  get_aggregate_value(
    df,
    group_cols = "unit",
    vars = list(prop_count = "x")
  ) |>
    expect_no_error() |>
    expect_warning()

  get_aggregate_value(
    df,
    group_cols = "unit",
    vars = list(prop = "x"),
    obfuscate_data = TRUE
  ) |>
    expect_no_error() |>
    expect_warning()

  get_aggregate_value(
    df,
    group_cols = "unit",
    vars = list(mean = "x"),
    obfuscate_data = TRUE
  ) |>
    expect_no_error() |>
    expect_warning()

  get_aggregate_value(
    df,
    group_cols = "unit",
    vars = list(median = "x"),
    obfuscate_data = TRUE
  ) |>
    expect_no_error() |>
    expect_warning()

  get_aggregate_value(
    df,
    group_cols = "unit",
    vars = list(prop_count = "x"),
    obfuscate_data = TRUE
  ) |>
    expect_no_error() |>
    expect_warning()

})

test_that("get_aggregate_value works with distinct_cols", {

  withr::local_seed(1)

  df <- data.frame(
    SubjectID = 1:2,
    county = rep(1:2, each = 2),
    hba1c = 1:4,
    date = lubridate::today() - 0:3
  ) |>
    dplyr::mutate(
      year = lubridate::year(.data$date)
    )

  df |>
    get_aggregate_value(
      group_cols = "county",
      vars = list(mean = "hba1c")
    ) |>
    expect_snapshot()

  df |>
    get_aggregate_value(
      group_cols = "county",
      vars = list(mean = "hba1c"),
      distinct_cols = "SubjectID",
      arrange_by = "date"
    ) |>
    expect_snapshot()

  df |>
    get_aggregate_value(
      group_cols = "county",
      vars = list(mean = "hba1c"),
      distinct_cols = c("subjectid", "county"),
      arrange_by = "date"
    ) |>
    expect_error()

  df |>
    get_aggregate_value(
      group_cols = "county",
      vars = list(mean = "hba1c"),
      distinct_cols = c("year"),
      arrange_by = "date"
    ) |>
    expect_warning()

  df |>
    get_aggregate_value(
      group_cols = "county",
      vars = list(mean = "hba1c"),
      distinct_cols = c("year"),
      arrange_by = "abcdef"
    ) |>
    expect_error()

})

test_that("get_aggregate_value works with count", {

  df <- data.frame(
    SubjectID = 1,
    county = 1:2,
    year = 2010:2013,
    ind = 1
  )

  res1 <- get_aggregate_value(
    df = df,
    group_cols = "county",
    vars = list(count = "year")
  ) |>
    dplyr::arrange(
      .data$county, .data$year
    ) |>
    dplyr::relocate(
      "total",
      "county",
      "year"
    )

  res2 <- get_aggregate_value(
    df = df,
    group_cols = "year",
    vars = list(count = "county")
  ) |>
    dplyr::arrange(
      .data$county, .data$year
    ) |>
    dplyr::relocate(
      "total",
      "county",
      "year"
    )

  expect_identical(res1, res2)

  expected <- tibble::tribble(
    ~total, ~county, ~year,
    1L, "1", "2010",
    1L, "1", "2012",
    2L, "1", "Alla",
    1L, "2", "2011",
    1L, "2", "2013",
    2L, "2", "Alla",
    1L, "Alla", "2010",
    1L, "Alla", "2011",
    1L, "Alla", "2012",
    1L, "Alla", "2013",
    4L, "Alla", "Alla"
  ) |>
    as.data.frame()

  expect_equal(res1, expected)

  get_aggregate_value(
    df = df,
    group_cols = "county",
    vars = list(
      prop = "ind",
      count = "year"
    )
  ) |>
    expect_error()

  df <- data.frame(
    x = rep(1, 9)
  )

  get_aggregate_value(
    df,
    vars = list(count = "x"),
    obfuscate_data = TRUE,
    marginal_cols = NULL
  ) |>
    expect_equal(
      data.frame(
        x = "1",
        total = 10
      )
    )

})

test_that("get_aggregate_value works with marginal_cols", {

  withr::local_seed(1)

  df <- data.frame(
    SubjectID = sample(1:10, 100, TRUE),
    year = sample(2019:2020, 100, TRUE),
    county = sample(1:2, 100, TRUE),
    ind = sample(0:1, 100, TRUE)
  )

  get_aggregate_value(
    df = df,
    group_cols = "county",
    vars = list(count = "year"),
    marginal_cols = "county"
  ) |>
    expect_snapshot()

  res1 <- get_aggregate_value(
    df = df,
    group_cols = "county",
    vars = list(count = "year")
  )

  res1 |>
    expect_snapshot()

  res2 <- get_aggregate_value(
    df = df,
    group_cols = "county",
    vars = list(count = "year"),
    marginal_cols = c("county", "year")
  )

  expect_identical(res1, res2)


  get_aggregate_value(
    df = df,
    group_cols = "county",
    marginal_cols = "year",
    vars = list(prop = "ind")
  ) |>
    expect_error()

  get_aggregate_value(
    df = df,
    group_cols = c("county", "year"),
    vars = list(prop = "ind"),
    marginal_cols = NULL
  ) |>
    expect_snapshot()

})

test_that("get_aggregate_value works with no group_cols", {

  df <- data.frame(
    SubjectID = rep(1:2, each = 5),
    county = 1:2,
    ind = rep(1, 10),
    date = lubridate::today() - 10:1
  )

  res <- get_aggregate_value(
    df = df,
    group_cols = NULL,
    vars = list(prop = "ind")
  )

  expected <- data.frame(
    ind_n = c(10),
    ind_prop = c(1),
    total = c(10L)
  )

  expect_identical(res, expected)

  get_aggregate_value(
    df = df,
    group_cols = NULL,
    marginal_cols = "county",
    vars = list(prop = "ind")
  ) |>
    expect_error()

  res <- get_aggregate_value(
    df = df,
    group_cols = NULL,
    vars = list(prop = "ind"),
    distinct_cols = "SubjectID",
    arrange_by = "date"
  )

  expected <- data.frame(
    ind_n = c(2),
    ind_prop = c(1),
    total = c(2L)
  )

  expect_identical(res, expected)

  df <- data.frame(
    SubjectID = 1,
    county = 1:2,
    year = 2010:2013,
    ind = 1
  )

  get_aggregate_value(
    df = df,
    group_cols = NULL,
    vars = list(count = "county")
  ) |>
    expect_identical(
      data.frame(
        total = c(4L, 2L, 2L),
        county = c("Alla", "1", "2")
      )
    )

  get_aggregate_value(
    df = df,
    group_cols = NULL,
    vars = list(count = "county"),
    marginal_cols = NULL
  ) |>
    expect_identical(
      data.frame(
        county = c("1", "2"),
        total = c(2L, 2L)
      )
    )

  get_aggregate_value(
    df = df,
    group_cols = NULL,
    vars = list(count = "county"),
    marginal_cols = "county"
  ) |>
    expect_identical(
      data.frame(
        total = c(4L, 2L, 2L),
        county = c("Alla", "1", "2")
      )
    )

  get_aggregate_value(
    df = df,
    group_cols = NULL,
    vars = list(count = "county"),
    distinct_cols = "SubjectID",
    arrange_by = "year",
    marginal_cols = NULL
  ) |>
    expect_identical(
      data.frame(
        county = c("1", "2"),
        total = c(1L, 1L)
      )
    )

})

test_that("add_reason_col works", {

  df <- data.frame(
    x = c(rep(1, 4), rep(0, 20))
  )

  get_aggregate_value(
    df,
    group_cols = NULL,
    vars = list(prop = "x"),
    marginal_cols = NULL,
    obfuscate_data = TRUE,
    add_reason_col = TRUE
  ) |>
    expect_equal(
      data.frame(
        x_n = 0,
        x_prop = 0,
        total = 20,
        obfuscated_reason = "n < 5"
      )
    )

  withr::local_seed(1)
  df <- data.frame(
    x = stats::rnorm(14)
  )

  get_aggregate_value(
    df,
    group_cols = NULL,
    vars = list(mean = "x"),
    marginal_cols = NULL,
    obfuscate_data = TRUE,
    add_reason_col = TRUE
  ) |>
    expect_snapshot()

  df <- data.frame(
    x = sample(1:10, size = 14, replace = TRUE)
  )

  get_aggregate_value(
    df,
    group_cols = NULL,
    vars = list(median = "x"),
    marginal_cols = NULL,
    obfuscate_data = TRUE,
    add_reason_col = TRUE
  ) |>
    expect_snapshot()

  get_aggregate_value(
    df,
    group_cols = NULL,
    vars = list(mean = "x"),
    marginal_cols = NULL,
    obfuscate_data = TRUE,
    add_reason_col = TRUE
  ) |>
    expect_snapshot()

  df <- data.frame(
    x = c(rep("a", 4), rep("b", 36))
  )

  get_aggregate_value(
    df,
    group_cols = NULL,
    vars = list(prop_count = "x"),
    marginal_cols = NULL,
    obfuscate_data = TRUE,
    add_reason_col = TRUE,
    pivot_prop_count = TRUE
  ) |>
    expect_equal(
      tibble::tibble(
        total = 40,
        x = c("a", "b"),
        x_n = c(0, 40),
        x_prop = 0,
        x_obfuscated_reason = "n < 5"
      )
    )

  df <- data.frame(
    x = c(rep(0, 4), rep(1, 200), rep(2, 30))
  )

  get_aggregate_value(
    df,
    vars = list(prop_count = "x"),
    obfuscate_data = TRUE,
    add_reason_col = TRUE,
    group_cols = NULL,
    pivot_prop_count = TRUE
  ) |>
    expect_snapshot()

})
