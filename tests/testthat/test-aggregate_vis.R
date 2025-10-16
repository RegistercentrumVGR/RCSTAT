test_that("get_measure_config works", {
  search_measure_id("002b50c4-6590-4b09-b0b3-3035a8088032") |>
    expect_snapshot()

  search_measure_id("doesnotexist") |>
    expect_null()

  get_measure_config("002b50c4-6590-4b09-b0b3-3035a8088032") |>
    expect_equal(
      list(
        version = 3L,
        freq = c("Kvartal", "År"),
        org = c("Riksnivå", "Länsnivå", "Mottagningsnivå"),
        title = "Vårdplan upprättad vid ADHD",
        status = "UNPUBLISHED",
        valid_from = as.POSIXct("2016-01-01 14:56:00 CET", tz = "Europe/Berlin"),
        type = "Andel"
      )
    )

  get_measure_config("doesnotexist") |>
    expect_error(regexp = "Assertion on 'measure_id' failed")
})

test_that("add_groups_long works", {
  df <- data.frame(
    date = lubridate::ymd("2024-09-14"),
    unit_var = 1,
    county_var = "14"
  )

  cfg <- list(
    org = c("Riksnivå", "Länsnivå", "Mottagningsnivå"),
    freq = c("Kvartal", "År", "Månad", "Halvår")
  )

  res <- add_groups_long(
    df,
    cfg,
    "date",
    "unit_var",
    "county_var"
  )

  expected <- data.frame(
    date = lubridate::ymd("2024-09-14"),
    unit_var = "1",
    county_var = "14",
    PeriodReportedStartDate = lubridate::ymd(
      c(
        "2024-07-01",
        "2024-01-01", "2024-09-01", "2024-07-01",
        "2024-07-01", "2024-01-01", "2024-09-01", "2024-07-01",
        "2024-07-01", "2024-01-01", "2024-09-01", "2024-07-01"
      )
    ),
    PeriodReportedEndDate = lubridate::ymd(
      c(
        "2024-09-30",
        "2024-12-31", "2024-09-30", "2024-12-31",
        "2024-09-30", "2024-12-31", "2024-09-30", "2024-12-31",
        "2024-09-30", "2024-12-31", "2024-09-30", "2024-12-31"
      )
    )
    ,
    unit = c(
      "Riket", "Riket",
      "Riket", "Riket", "14", "14", "14", "14", "1",
      "1", "1", "1"
    ),
    unit_type = c(
      "country",
      "country", "country", "country", "county", "county",
      "county", "county", "unit", "unit", "unit", "unit"
    ),
    county = c(
      NA, NA, NA, NA,
      NA, NA, NA, NA, "14", "14", "14", "14"
    )
  )

  expect_equal(res, expected)

  add_groups_long(
    df,
    list(
      org = c("abc", "Länsnivå", "Mottagningsnivå"),
      freq = c("Kvartal", "År")
    ),
    "date",
    "unit_var",
    "county_var"
  ) |>
    expect_error()

  add_groups_long(
    df,
    list(
      org = c("Länsnivå", "Mottagningsnivå"),
      freq = c("abc", "År")
    ),
    "date",
    "unit_var",
    "county_var"
  ) |>
    expect_error()

  df <- data.frame(
    date = c(
      lubridate::floor_date(lubridate::today(), unit = "month") - 1,
      lubridate::today() - 365
    ),
    unit_var = 1,
    county_var = "14"
  )

  cfg <- list(
    org = c("Riksnivå"),
    freq = c("Kvartal", "År", "Månad")
  )

  res <- add_groups_long(
    df = df,
    cfg = cfg,
    date_var = "date",
    unit_var = "unit_var",
    county_var = "county_var"
  )

  expect_true(all(res$PeriodReportedEndDate < lubridate::today()))

})

test_that("postprocess_indicator works", {
  data.frame(
    ind_n = 10,
    total = 20,
    ind_prop = 0.5
  ) |>
    postprocess_indicator_prop() |>
    expect_equal(
      data.frame(
        Denominator = 10,
        Numerator = 20,
        Rate = 0.5
      )
    )

  data.frame(
    ind_n = 10,
    total = 20,
    ind_total_non_missing = 10,
    ind_prop = 1
  ) |>
    postprocess_indicator_prop() |>
    expect_equal(
      data.frame(
        Denominator = 10,
        total = 20,
        Numerator = 10,
        Rate = 1
      )
    )

  data.frame(
    ind_n = 10,
    total = 20,
    ind_median = 1
  ) |>
    postprocess_indicator_median() |>
    expect_equal(
      data.frame(
        ind_n = 10,
        Measurepopulation = 20,
        Value = 1
      )
    )

  data.frame(
    ind_n = 10,
    total = 20,
    ind_total_non_missing = 10,
    ind_median = 1
  ) |>
    postprocess_indicator_median() |>
    expect_equal(
      data.frame(
        ind_n = 10,
        total = 20,
        Measurepopulation = 10,
        Value = 1
      )
    )

  data.frame(
    total = 10
  ) |>
    postprocess_indicator_count() |>
    expect_equal(
      data.frame(
        Value = 10
      )
    )

  data.frame(
    total = 10,
    n = 10
  ) |>
    postprocess_indicator_count() |>
    expect_error()

  data.frame(
    unit = c("14", "10003", NA),
    unit_type = c("county", "unit", "country"),
    county = c(NA, "14", NA)
  ) |>
    postprocess_indicator_org(100) |>
    expect_equal(
      data.frame(
        RegionOrganisationId = c("14", "14", NA),
        UnitName = c(NA, "Test U/Sahlgrenska", NA),
        UnitHSAID = c(NA, "SE2321000131-F000000000207", NA),
        RegionName = c("Västra Götaland", "Västra Götaland", NA)
      )
    )

  data.frame(
    idk = NA
  ) |>
    postprocess_indicator_register(100) |>
    expect_equal(
      data.frame(
        idk = NA,
        RegisterNamn = "Registercentrum Västra Götaland",
        RegisterHSAID = NA
      )
    )

  testthat::local_mocked_bindings(
    api_register = function(register_id) {
      return(
        list(
          data = list(
            RegisterName = "Superfint namn!",
            HSAID = "ännu-finare-hsa-id-!",
            RegisterID = register_id
          )
        )
      )
    }
  )

  data.frame(
    idk = NA
  ) |>
    postprocess_indicator_register(1) |>
    expect_equal(
      data.frame(
        idk = NA,
        RegisterNamn = "Superfint namn!",
        RegisterHSAID = "ännu-finare-hsa-id-!"
      )
    )
})

test_that("aggregate_vis works", {
  aggregate_vis(measure_id = "0-0-0-0-0") |>
    expect_error(regexp = "No config found for measure ID 0-0-0-0-0")

  df <- data.frame(
    date = lubridate::ymd("2024-09-14"),
    unit_var = 999,
    county_var = "14",
    some_value = rep(1, 100)
  )

  indicator_function <- function(df,
                                 group_cols,
                                 aggregate = TRUE,
                                 marginal_cols,
                                 obfuscate_data = TRUE) {
    df <- df |>
      dplyr::mutate(
        urval_indicator = TRUE,
        indicator = .data$some_value
      )

    if (aggregate) {
      res <- df |>
        dplyr::filter(.data$urval_indicator) |>
        get_aggregate_value(
          group_cols = group_cols,
          marginal_cols = marginal_cols,
          obfuscate_data = obfuscate_data,
          vars = list(prop = "indicator")
        )
      return(res)
    }

    return(df)
  }

  testthat::local_mocked_bindings(
    get_measure_config = function(...) {
      return(
        list(
          org = c("Riksnivå", "Vårdenhet", "Region"),
          freq = c("År", "Kvartal"),
          valid_from = "2024-01-01",
          type = "Andel",
          version = 1,
          status = "UNPUBLISHED"
        )
      )
    }
  )

  aggregate_vis(
    measure_id = "abc123"
  ) |>
    expect_null() |>
    expect_warning("MeasureID 'abc123' is unpublished, skipping")

  testthat::local_mocked_bindings(
    get_measure_config = function(...) {
      return(
        list(
          org = c("Riksnivå", "Vårdenhet", "Region"),
          freq = c("År", "Kvartal"),
          valid_from = "2024-01-01",
          type = "Andel",
          version = 1,
          status = "PUBLISHED"
        )
      )
    }
  )

  res <- aggregate_vis(
    df = df,
    measure_id = "abc123",
    indicator_function = indicator_function,
    date_var = "date",
    unit_var = "unit_var",
    county_var = "county_var",
    register_id = 100
  )

  expected <- data.frame(
    PeriodReportedStartDate = lubridate::ymd(
      c(
        "2024-01-01",
        "2024-01-01",
        "2024-01-01",
        "2024-07-01",
        "2024-07-01",
        "2024-07-01"
      )
    ),
    PeriodReportedEndDate = lubridate::ymd(
      c(
        "2024-12-31",
        "2024-12-31",
        "2024-12-31",
        "2024-09-30",
        "2024-09-30",
        "2024-09-30"
      )
    ),
    Denominator = 100,
    Rate = 1,
    Numerator = 100,
    RegionOrganisationId = c("14", "14", NA, "14", "14", NA),
    UnitName = c(
      NA,
      "Registercentrum",
      NA,
      NA,
      "Registercentrum",
      NA
    ),
    UnitHSAID = c(NA, "", NA, NA, "", NA),
    RegionName = c(
      "Västra Götaland",
      "Västra Götaland",
      NA,
      "Västra Götaland",
      "Västra Götaland",
      NA
    ),
    RegisterNamn = "Registercentrum Västra Götaland",
    RegisterHSAID = NA,
    Version = 1,
    MeasureID = "abc123"
  )

  expect_equal(res, expected)
})
