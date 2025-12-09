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
        type = "Andel",
        submeasures = list(
          list(
            gender_code = "2",
            measure_id = "6a984524-3ea9-4561-89d6-f5d46dc9ad9e"
          ),
          list(
            gender_code = "1",
            measure_id = "9d187574-e549-48fb-beb2-931ac035fda9"
          )
        )
      )
    )

  get_measure_config("doesnotexist") |>
    expect_error(regexp = "Assertion on 'measure_id' failed")

  expect_no_error(get_measure_config("99000"))

})

test_that("add_groups_long works", {
  df <- data.frame(
    date = lubridate::ymd("2024-09-14"),
    unit_var = 1,
    county_var = "14",
    gender_var = 1
  )

  cfg <- list(
    org = c("Riksnivå", "Länsnivå", "Mottagningsnivå"),
    freq = c("Kvartal", "År", "Månad", "Halvår"),
    submeasures = list(
      list(gender_code = "1", measure_id = 1),
      list(gender_code = "2", measure_id = 2)
    )
  )

  add_groups_long(
    df,
    cfg,
    "date",
    "unit_var",
    "county_var",
    "gender_var"
  ) |>
    expect_snapshot()

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
    county_var = "14",
    gender_var = 2
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
    county_var = "county_var",
    gender_var = "gender_var"
  )

  expect_true(all(res$PeriodReportedEndDate < lubridate::today()))

  df <- data.frame(gender_var = 1:2)

  add_groups_long_gender(df, list(), "gender_var") |>
    expect_equal(
      data.frame(
        gender_var = 1:2,
        gender = "all"
      )
    )

  add_groups_long_gender(
    df,
    list(
      list(gender_code = "1", measure_id = 1)
    ),
    "gender_var"
  ) |>
    expect_equal(
      data.frame(
        gender_var = c(1, 2, 1),
        gender = c("all", "all", "1")
      )
    )

  add_groups_long_gender(
    df,
    list(
      list(gender_code = "1", measure_id = 1),
      list(gender_code = "2", measure_id = 2)
    ),
    "gender_var"
  ) |>
    expect_equal(
      data.frame(
        gender_var = c(1, 2, 1, 2),
        gender = c("all", "all", "1", "2")
      )
    )

  add_groups_long_gender(
    df,
    list(
      list(gender_code = "def", measure_id = 1)
    ),
    "gender_var"
  ) |>
    expect_error()

  df <- data.frame(
    date = lubridate::ymd("2024-09-14"),
    unit_var = 1,
    county_var = "14",
    gender_var = 1
  )

  cfg <- list(
    org = c("Riksnivå", "Hälso-och sjukvårdsregion"),
    freq = "År"
  )

  expect_message(
    res <- add_groups_long_organization(
      df,
      "unit_var",
      "county_var",
      "Hälso-och sjukvårdsregion"
    ),
    "! Type 'Hälso-och sjukvårdsregion' is not yet supported, ignoring"
  )

  expect_null(res)

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
        Numerator = 10,
        Denominator = 20,
        Rate = 0.5
      )
    )

  data.frame(
    estimate = 0.5,
    cum_events = 5,
    total = 10
  ) |>
    postprocess_indicator_prop() |>
    expect_equal(
      data.frame(
        Rate = 0.5,
        Numerator = 5,
        Denominator = 10
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
        Numerator = 10,
        total = 20,
        Denominator = 10,
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
    ind_n = 10,
    total = 20,
    ind_total_non_missing = 10,
    ind_mean = 1
  ) |>
    postprocess_indicator_mean() |>
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
        CountryName = "Riket",
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

  df <- data.frame(
    x_prop = 1e-16,
    x_median = 1e-16,
    x_mean = 1e-16,
    total = 10
  )

  df |>
    postprocess_indicator_mean() |>
    dplyr::pull("Value") |>
    expect_equal(0)

  df |>
    postprocess_indicator_median() |>
    dplyr::pull("Value") |>
    expect_equal(0)

  df |>
    postprocess_indicator_prop() |>
    dplyr::pull("Rate") |>
    expect_equal(0)
})

test_that("aggregate_vis works", {

  expect_warning(
    res <- aggregate_vis(measure_id = "0-0-0-0-0"),
    regexp = "No config found for MeasureID '0-0-0-0-0', skipping"
  )

  expect_null(res)

  df <- data.frame(
    date = lubridate::ymd("2024-09-14"),
    unit_var = c(rep(999, 100), rep(1000, 10)),
    county_var = "14",
    some_value = c(rep(1, 100), rep(1, 10)),
    SubjectKey = "111111-1111"
  )

  indicator_function <- function(df,
                                 group_cols,
                                 aggregate = TRUE,
                                 marginal_cols,
                                 obfuscate_data = TRUE,
                                 add_reason_col = TRUE,
                                 censored_value = NA) {
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
          add_reason_col = add_reason_col,
          vars = list(prop = "indicator"),
          censored_value = censored_value
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

  expect_snapshot(res)

  tmp_func <- function(df,
                       obfuscate_data,
                       aggregate,
                       group_cols,
                       marginal_cols) {
    "Hello, World!"
  }

  aggregate_vis(
    df = df,
    measure_id = "abc123",
    indicator_function = tmp_func,
    date_var = "date",
    unit_var = "unit_var",
    county_var = "county_var",
    register_id = 100
  ) |>
    expect_error(
      "Assertion on 'indicator_function' failed: Must have formal arguments:"
    )

  testthat::local_mocked_bindings(
    get_measure_config = function(...) {
      return(
        list(
          org = c("Riksnivå", "Region"),
          freq = c("År"),
          valid_from = "2024-01-01",
          type = "Andel",
          version = 1,
          status = "PUBLISHED"
        )
      )
    }
  )

  aggregate_vis(
    df = df,
    measure_id = "abc123",
    indicator_function = indicator_function,
    date_var = "date",
    unit_var = "unit_var",
    county_var = "county_var",
    register_id = 100
  ) |>
    expect_no_error()

  aggregate_vis(
    df = dplyr::select(df, -"SubjectKey"),
    gender_var = NULL
  ) |>
    expect_error()

  testthat::local_mocked_bindings(
    get_measure_config = function(...) {
      return(
        list(
          org = c("Riksnivå", "Vårdenhet", "Region"),
          freq = c("År", "Kvartal"),
          valid_from = "2024-01-01",
          type = "Andel",
          version = 1,
          status = "PUBLISHED",
          submeasures = list(
            list(gender_code = "1", measure_id = "abc124"),
            list(gender_code = "2", measure_id = "abc125")
          )
        )
      )
    }
  )

  # Duplicated MeasureID columns with submeasures
  df |>
    dplyr::mutate(gender_var = c(rep(1, 55), rep(2, 55))) |>
    aggregate_vis(
      measure_id = "abc123",
      indicator_function = indicator_function,
      date_var = "date",
      unit_var = "unit_var",
      county_var = "county_var",
      register_id = 100
    ) |>
    expect_no_error()

  testthat::local_mocked_bindings(
    get_measure_config = function(...) {
      return(
        list(
          org = c("Riksnivå"),
          freq = c("År"),
          valid_from = "2024-01-01",
          type = "Andel",
          version = 1,
          status = "PUBLISHED"
        )
      )
    }
  )

  df <- data.frame(
    date = lubridate::make_date(2024, 1, 1),
    county = "14",
    unit = NA,
    x = 1:100,
    gender = 1
  )

  ind <- function(df,
                  aggregate,
                  group_cols,
                  marginal_cols,
                  obfuscate_data,
                  add_reason_col,
                  censored_value,
                  abc) {
    df <- df |>
      dplyr::mutate(
        urval = TRUE,
        ind = .data$x <= abc
      )

    if (aggregate) {
      df <- df |>
        dplyr::filter(.data$urval) |>
        RCStat::get_aggregate_value(
          group_cols = group_cols,
          marginal_cols = marginal_cols,
          obfuscate_data = obfuscate_data,
          add_reason_col = add_reason_col,
          censored_value = censored_value,
          vars = list(prop = "ind")
        )
    }
    df
  }

  expect_no_error(
    res <- aggregate_vis(
      df = df,
      measure_id = "11111",
      unit_var = "unit",
      date_var = "date",
      county_var = "county",
      indicator_function = ind,
      gender_var = "gender",
      abc = 50,
      register_id = 100,
      def = "meow"
    )
  )

  expect_equal(res$Numerator, 50)
  expect_equal(res$Rate, 0.5)
  expect_equal(res$Denominator, 100)


  ind <- function(df,
                  aggregate,
                  group_cols,
                  marginal_cols,
                  obfuscate_data,
                  add_reason_col,
                  censored_value) {
    df <- df |>
      dplyr::mutate(
        urval = TRUE,
        ind = .data$x <= 50
      )

    if (aggregate) {
      df <- df |>
        dplyr::filter(.data$urval) |>
        RCStat::get_aggregate_value(
          group_cols = group_cols,
          marginal_cols = marginal_cols,
          obfuscate_data = obfuscate_data,
          add_reason_col = add_reason_col,
          censored_value = censored_value,
          vars = list(prop = "ind")
        )
    }
    df
  }

  # Unused arguments cause no error
  expect_no_error(
    aggregate_vis(
      df = df,
      measure_id = "11111",
      unit_var = "unit",
      date_var = "date",
      county_var = "county",
      indicator_function = ind,
      gender_var = "gender",
      abc = 50,
      register_id = 100
    )
  )
})
