test_that("check_duplicate_rows works", {
  data.frame(
    PeriodReportedStartDate = "2000-01-01",
    PeriodReportedEndDate = "2000-12-31",
    UnitName = "abc",
    UnitHSAID = "abc123",
    HospitalName = NA,
    HospitalHSAID = NA,
    Rate = c(0.5, 0.75),
    MeasureID = 1,
    RegionOrganisationId = "01",
    HospitalOrganisationId = NA
  ) |>
    check_duplicate_rows() |>
    expect_equal(
      paste0("MeasureID 1, period 2000-01-01-2000-12-31, RegionOrganisation 01, unit/hospital name abc,",
             " unit/hospital HSA/PAR-id abc123 is duplicated")
    )

  data.frame(
    PeriodReportedStartDate = "2000-01-01",
    PeriodReportedEndDate = "2000-12-31",
    RegionOrganisationId = c("01", "14"),
    UnitName = NA,
    UnitHSAID = NA,
    HospitalName = NA,
    HospitalHSAID = NA,
    Rate = c(0.5, 0.75),
    MeasureID = 1,
    HospitalOrganisationId = NA
  ) |>
    check_duplicate_rows() |>
    expect_null()
})

test_that("check_instruction works", {
  data.frame(
    RegisterHSAID = "abc"
  ) |>
    check_instruction() |>
    expect_equal("First cell does not contain correct keep or delete existing instruction")

  data.frame(
    MeasureID = NA,
    RegisterHSAID = "DELETE_EXISTING_DATA_FOR_REGISTER_HSAID:abc"
  ) |>
    check_instruction() |>
    expect_equal("First cell does not contain correct keep or delete existing instruction")

  data.frame(
    RegisterHSAID = "DELETE_EXISTING_DATA_FOR_REGISTER_HSAID:abc"
  ) |>
    check_instruction() |>
    expect_null()
})

test_that("check_names works", {
  nms <- get_vis_names()

  df <- matrix(NA, ncol = length(nms) - 1) |>
    as.data.frame() |>
    setNames(nms[-1])

  check_names(df) |>
    expect_equal(sprintf("Variables missing: %s", nms[1]))

  df <- matrix(NA, ncol = length(nms) + 1) |>
    as.data.frame() |>
    setNames(c(nms, "abc"))

  check_names(df) |>
    expect_equal("Variables that should not be included: abc")
})

test_that("check_dates works", {
  data.frame(
    PeriodReportedStartDate = lubridate::ymd("2024-12-31"),
    PeriodReportedEndDate = lubridate::ymd("2024-01-01"),
    UnitName = "a",
    HospitalName = NA,
    UnitHSAID = 1,
    HospitalHSAID = NA,
    MeasureID = 1,
    RegionOrganisationId = "01",
    HospitalOrganisationId = NA
  ) |>
    check_dates() |>
    expect_equal(
      "MeasureID 1, RegionOrganisationId 01, unit/hospital name a has PeriodReportedEndDate <= PeriodReportedStartDate"
    )

  data.frame(
    PeriodReportedStartDate = lubridate::ymd("2024-01-01"),
    PeriodReportedEndDate = lubridate::today() + 1,
    UnitName = "a",
    HospitalName = NA,
    UnitHSAID = 1,
    HospitalHSAID = NA,
    MeasureID = 1,
    RegionOrganisationId = "01",
    HospitalOrganisationId = NA
  ) |>
    check_dates() |>
    expect_equal(
      "MeasureID 1, RegionOrganisationId 01, unit/hospital name a has PeriodReportedEndDate > lubridate::today()"
    )

  data.frame(
    PeriodReportedStartDate = lubridate::ymd("2024-01-01", "2025-01-01"),
    PeriodReportedEndDate = c(lubridate::ymd("2024-12-31"), NA),
    UnitName = "a",
    HospitalName = NA,
    UnitHSAID = 1,
    HospitalHSAID = NA,
    MeasureID = 1,
    RegionOrganisationId = "01",
    HospitalOrganisationId = NA
  ) |>
    check_dates() |>
    expect_equal("Some PeriodReportedEndDates are missing")
})

test_that("check_register_hsaid works", {
  data.frame(
    RegisterHSAID = c(
      "DELETE_EXISTING_DATA_FOR_REGISTER_HSAID:abc",
      "abc"
    )
  ) |>
    check_register_hsaid() |>
    expect_null()

  data.frame(
    RegisterHSAID = c(
      "DELETE_EXISTING_DATA_FOR_REGISTER_HSAID:abc",
      "abc123"
    )
  ) |>
    check_register_hsaid() |>
    expect_equal("RegisterHSAID in instruction does not equal other RegisterHSAIDs")

  data.frame(
    RegisterHSAID = c(
      "DELETE_EXISTING_DATA_FOR_REGISTER_HSAID:abc",
      "abc123",
      "abc"
    )
  ) |>
    check_register_hsaid() |>
    expect_equal(
      c(
        "RegisterHSAID in instruction does not equal other RegisterHSAIDs",
        "Multiple RegisterHSAIDs observed"
      )
    )
})

test_that("check_value_rate works", {
  data.frame(
    Rate = 0.5,
    Numerator = 5,
    Denominator = 10,
    ReasonCode = NA,
    UnitName = "a",
    HospitalName = NA,
    Value = NA,
    MeasureID = 1,
    PeriodReportedStartDate = lubridate::ymd("2024-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2024-12-31"),
    RegionOrganisationId = "14"
  ) |>
    check_value_rate() |>
    expect_null()

  data.frame(
    Value = 10,
    Rate = NA,
    ReasonCode = NA,
    UnitName = "a",
    HospitalName = NA,
    MeasureID = 1,
    Numerator = NA,
    Denominator = NA,
    PeriodReportedStartDate = lubridate::ymd("2024-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2024-12-31"),
    RegionOrganisationId = "14"
  ) |>
    check_value_rate() |>
    expect_null()

  data.frame(
    Value = NA,
    Rate = NA,
    ReasonCode = NA,
    UnitName = "a",
    HospitalName = NA,
    MeasureID = 1,
    PeriodReportedStartDate = lubridate::ymd("2024-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2024-12-31"),
    RegionOrganisationId = "14",
    Numerator = NA,
    Denominator = NA
  ) |>
    check_value_rate() |>
    expect_equal(
      paste0("MeasureID 1, period 2024-01-01-2024-12-31, RegionOrganisationId 14, unit/hospital name a",
             " is missing both Value and Rate with no ReasonCode")
    )

  data.frame(
    Value = NA,
    Rate = NA,
    ReasonCode = "MSK",
    UnitName = "a",
    HospitalName = NA,
    MeasureID = 1,
    PeriodReportedStartDate = lubridate::ymd("2024-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2024-12-31"),
    RegionOrganisationId = "14",
    Numerator = NA,
    Denominator = NA
  ) |>
    check_value_rate() |>
    expect_null()

  data.frame(
    Rate = 0.5,
    Numerator = c(5, NA),
    Denominator = c(NA, 10),
    ReasonCode = NA,
    UnitName = "a",
    HospitalName = NA,
    MeasureID = 1,
    PeriodReportedStartDate = lubridate::ymd("2024-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2024-12-31"),
    RegionOrganisationId = "14",
    Value = NA
  ) |>
    check_value_rate() |>
    expect_equal(
      c(
        paste0("MeasureID 1, period 2024-01-01-2024-12-31, RegionOrganisationId 14, unit/hospital name a",
               " is missing Numerator where Rate is present"),
        paste0("MeasureID 1, period 2024-01-01-2024-12-31, RegionOrganisationId 14, unit/hospital name a",
               " is missing Denominator where Rate is present")
      )
    )
})

test_that("check_ci works", {
  data.frame(
    ConfidenceIntervalLower = 0,
    ConfidenceIntervalHigher = 1,
    ReasonCode = NA,
    UnitName = "a",
    HospitalName = NA,
    MeasureID = 1,
    PeriodReportedStartDate = lubridate::ymd("2024-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2024-12-31"),
    RegionOrganisationId = "14"
  ) |>
    check_ci() |>
    expect_null()

  data.frame(
    ConfidenceIntervalLower = c(0, NA),
    ConfidenceIntervalHigher = c(NA, 1),
    ReasonCode = NA,
    UnitName = "a",
    HospitalName = NA,
    MeasureID = 1,
    PeriodReportedStartDate = lubridate::ymd("2024-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2024-12-31"),
    RegionOrganisationId = "14"
  ) |>
    check_ci() |>
    expect_equal(
      paste0("MeasureID 1, period 2024-01-01-2024-12-31, RegionOrganisationId 14, unit/hospital name a",
             " is missing one of ConfidenceIntervalLower/Higher")
    )

  data.frame(
    ConfidenceIntervalLower = 1,
    ConfidenceIntervalHigher = 0.99,
    ReasonCode = NA,
    UnitName = "a",
    HospitalName = NA,
    MeasureID = 1,
    PeriodReportedStartDate = lubridate::ymd("2024-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2024-12-31"),
    RegionOrganisationId = "14"
  ) |>
    check_ci() |>
    expect_equal(
      paste0("MeasureID 1, period 2024-01-01-2024-12-31, RegionOrganisationId 14, unit/hospital name a",
             " has a higher ConfidenceIntervalLower than ConfidenceIntervalHigher")
    )
})

test_that("check_censor_rate works", {
  data.frame(
    MeasureID = 1,
    ReasonCode = rep("MSK", 10)
  ) |>
    check_censor_rate() |>
    expect_equal(
      c(
        "The overall censored rate is 100.0%",
        "MeasureID 1 is censored to an abnormally high degree (100.0%)"
      )
    )

  data.frame(
    MeasureID = c(1, 1, 2, 2),
    ReasonCode = rep(c(NA, "MSK"), 4)
  ) |>
    check_censor_rate() |>
    expect_equal("The overall censored rate is 50.0%")

  data.frame(
    MeasureID = c(rep(1, 9), 2),
    ReasonCode = c(rep(NA, 9), "MSK")
  ) |>
    check_censor_rate() |>
    expect_equal("MeasureID 2 is censored to an abnormally high degree (100.0%)")
})

test_that("check_counties_and_units works", {
  data.frame(
    RegionOrganisationId = c(NA, "01"),
    RegionName = c(NA, "Stockholm"),
    UnitName = "a",
    UnitHSAID = 1,
    HospitalName = NA,
    HospitalHSAID = NA,
    MeasureID = 1,
    PeriodReportedStartDate = lubridate::ymd("2023-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2023-12-31"),
    HospitalOrganisationId = NA
  ) |>
    check_counties_and_units() |>
    expect_setequal(
      c(
        "The unit/hospital a (1) belongs to several counties: NA, 01",
        "Unit a is missing RegionOrganisationId"
      )
    )

  data.frame(
    RegionOrganisationId = NA,
    RegionName = NA,
    UnitName = NA,
    UnitHSAID = NA,
    HospitalName = NA,
    HospitalHSAID = NA,
    MeasureID = 1,
    PeriodReportedStartDate = lubridate::ymd("2023-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2023-12-31"),
    Rate = c(0.5, 1),
    HospitalOrganisationId = NA
  ) |>
    check_counties_and_units() |>
    expect_equal(
      c(
        "MeasureID 1, period 2023-01-01-2023-12-31 is missing RegionOrganistionId in 2 rows, should be exactly 1",
        paste0("MeasureID 1, RegionOrganisationId NA, period 2023-01-01-2023-12-31",
               " has 2 rows missing both unit/hospital name and HSA/PAR-id, should be exactly 1")
      )
    )

  data.frame(
    RegionOrganisationId = c(NA, "01"),
    RegionName = c(NA, "Stockholm"),
    UnitName = "a",
    UnitHSAID = 1,
    HospitalName = NA,
    HospitalHSAID = NA,
    MeasureID = 1,
    PeriodReportedStartDate = lubridate::ymd("2023-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2023-12-31"),
    Rate = c(0.5, 1),
    HospitalOrganisationId = NA
  ) |>
    check_counties_and_units() |>
    expect_equal(
      c(
        "Unit a is missing RegionOrganisationId",
        "The unit/hospital a (1) belongs to several counties: NA, 01"
      )
    )

  data.frame(
    RegionOrganisationId = "01",
    RegionName = "Stockholm",
    UnitName = NA,
    UnitHSAID = NA,
    HospitalName = NA,
    HospitalHSAID = NA,
    MeasureID = 1,
    PeriodReportedStartDate = lubridate::ymd("2023-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2023-12-31"),
    Rate = 0.5,
    HospitalOrganisationId = NA
  ) |>
    check_counties_and_units() |>
    expect_null()

  data.frame(
    RegionOrganisationId = "01",
    RegionName = "Stockholm",
    UnitName = NA,
    UnitHSAID = NA,
    HospitalName = NA,
    HospitalHSAID = NA,
    MeasureID = 1,
    PeriodReportedStartDate = lubridate::ymd("2023-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2023-12-31"),
    Rate = c(0.5, 0.75),
    HospitalOrganisationId = NA
  ) |>
    check_counties_and_units() |>
    expect_equal(
      paste0(
        "MeasureID 1, RegionOrganisationId 01, period 2023-01-01-2023-12-31",
        " has 2 rows missing both unit/hospital name and HSA/PAR-id, should be exactly 1"
      )
    )


  data.frame(
    RegionOrganisationId = "01",
    RegionName = "Stockholm",
    UnitName = "a",
    UnitHSAID = NA,
    HospitalName = NA,
    HospitalHSAID = NA,
    MeasureID = 1,
    PeriodReportedStartDate = lubridate::ymd("2023-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2023-12-31"),
    HospitalOrganisationId = NA
  ) |>
    check_counties_and_units() |>
    expect_equal("Unit a is missing HSA/PAR-id")

  data.frame(
    RegionOrganisationId = "01",
    RegionName = "Stockholm",
    UnitName = NA,
    UnitHSAID = 123,
    HospitalName = NA,
    HospitalHSAID = NA,
    MeasureID = 1,
    PeriodReportedStartDate = lubridate::ymd("2023-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2023-12-31"),
    HospitalOrganisationId = NA
  ) |>
    check_counties_and_units() |>
    expect_equal("HSA/PAR-id 123 is missing name")

  data.frame(
    RegionOrganisationId = "01",
    RegionName = "Stockholm",
    UnitName = letters[1:3],
    UnitHSAID = 123,
    HospitalName = NA,
    HospitalHSAID = NA,
    MeasureID = 1,
    PeriodReportedStartDate = lubridate::ymd("2023-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2023-12-31"),
    HospitalOrganisationId = NA
  ) |>
    check_counties_and_units() |>
    expect_equal("The unit/hospital HSA/PAR-id 123 belongs to several units: a, b, c")

  data.frame(
    RegionOrganisationId = "01",
    RegionName = "Stockholm",
    UnitName = "a",
    UnitHSAID = 1:3,
    HospitalName = NA,
    HospitalHSAID = NA,
    MeasureID = 1,
    PeriodReportedStartDate = lubridate::ymd("2023-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2023-12-31"),
    HospitalOrganisationId = NA
  ) |>
    check_counties_and_units() |>
    expect_equal("The unit/hospital a has several HSA/PAR-ids 1, 2, 3")

  data.frame(
    RegionOrganisationId = c("01", "14"),
    RegionName = c("Stockholm", "Västra Götalandsregionen"),
    UnitName = "a",
    UnitHSAID = 1,
    HospitalName = NA,
    HospitalHSAID = NA,
    MeasureID = 1,
    PeriodReportedStartDate = lubridate::ymd("2023-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2023-12-31"),
    HospitalOrganisationId = NA
  ) |>
    check_counties_and_units() |>
    expect_equal("The unit/hospital a (1) belongs to several counties: 01, 14")


  data.frame(
    RegionOrganisationId = "01",
    RegionName = "Stockholm",
    UnitName = "a",
    UnitHSAID = 1,
    HospitalName = NA,
    HospitalHSAID = NA,
    MeasureID = 1,
    PeriodReportedStartDate = lubridate::ymd("2023-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2023-12-31"),
    HospitalOrganisationId = NA
  ) |>
    check_counties_and_units() |>
    expect_null()

  data.frame(
    RegionOrganisationId = c("01", "14"),
    RegionName = c("Stockholm", "VGR"),
    UnitName = NA,
    UnitHSAID = NA,
    HospitalName = NA,
    HospitalHSAID = NA,
    MeasureID = 1,
    PeriodReportedStartDate = lubridate::ymd("2023-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2023-12-31"),
    HospitalOrganisationId = NA
  ) |>
    check_counties_and_units() |>
    expect_null()
})

test_that("validate_vis works", {
  df <- data.frame(
    RegionOrganisationId = "01",
    RegionName = "Stockholm",
    UnitName = "a",
    UnitHSAID = 1,
    HospitalName = NA,
    HospitalHSAID = NA,
    Rate = 0.5,
    Numerator = 5,
    Denominator = 10,
    ReasonCode = NA
  )

  df |>
    validate_vis() |>
    expect_equal(
      paste0(
        "Variables missing: ",
        paste0(setdiff(get_vis_names(), names(df)), collapse = ", ")
      )
    )

  df <- data.frame(
    RegionOrganisationId = "01",
    RegionName = "Stockholm",
    UnitName = "a",
    UnitHSAID = 1,
    HospitalName = NA,
    HospitalHSAID = NA,
    Rate = 0.5,
    Numerator = 5,
    Denominator = 10,
    ReasonCode = NA,
    PeriodReportedStartDate = lubridate::ymd("2000-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2000-12-31"),
    MeasureID = 1,
    HospitalOrganisationId = NA,
    CountryName = "Sverige",
    Value = NA,
    Cohort = NA,
    Version = 1,
    ReferenceIntervalRate = NA,
    ReferenceIntervalValue = NA,
    MunicipalityOrganisationID = NA,
    MunicipalityName = NA,
    RegisterHSAID = 1,
    RegisterNamn = "abc",
    Coverage = NA,
    ConfidenceIntervalLower = NA,
    ConfidenceIntervalHigher = NA,
    Standarddeviation = NA,
    Measurepopulation = NA,
    Exclusions = NA
  ) |>
    dplyr::add_row(
      RegionOrganisationId = "KEEP_EXISTING_DATA_FOR_REGISTER_HSAID:1",
      .before = 1
    )

  validate_vis(df) |>
    expect_null()

  data.frame(
    RegionOrganisationId = "01",
    RegionName = "Stockholm",
    UnitName = "a",
    UnitHSAID = 1,
    HospitalName = NA,
    HospitalHSAID = NA,
    Rate = 0.5,
    Numerator = NA,
    Denominator = 10,
    ReasonCode = NA,
    PeriodReportedStartDate = lubridate::ymd("2000-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2000-12-31"),
    MeasureID = 1,
    HospitalOrganisationId = NA,
    CountryName = "Sverige",
    Value = NA,
    Cohort = NA,
    Version = 1,
    ReferenceIntervalRate = NA,
    ReferenceIntervalValue = NA,
    MunicipalityOrganisationID = NA,
    MunicipalityName = NA,
    RegisterHSAID = 1,
    RegisterNamn = "abc",
    Coverage = NA,
    ConfidenceIntervalLower = NA,
    ConfidenceIntervalHigher = NA,
    Standarddeviation = NA,
    Measurepopulation = NA,
    Exclusions = NA
  ) |>
    dplyr::add_row(
      RegionOrganisationId = "KEEP_EXISTING_DATA_FOR_REGISTER_HSAID:1",
      .before = 1
    ) |>
    validate_vis() |>
    expect_equal(
      paste0("MeasureID 1, period 2000-01-01-2000-12-31, RegionOrganisationId 01, unit/hospital name a",
             " is missing Numerator where Rate is present")
    )

  data.frame(
    RegionOrganisationId = "01",
    RegionName = "Stockholm",
    UnitName = "a",
    UnitHSAID = 1,
    HospitalName = NA,
    HospitalHSAID = NA,
    Rate = 0.5,
    Numerator = NA,
    Denominator = 10,
    ReasonCode = NA,
    PeriodReportedStartDate = lubridate::ymd("2000-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2000-12-31"),
    MeasureID = 1,
    HospitalOrganisationId = NA,
    CountryName = "Sverige",
    Value = NA,
    Cohort = NA,
    Version = 1,
    ReferenceIntervalRate = NA,
    ReferenceIntervalValue = NA,
    MunicipalityOrganisationID = NA,
    MunicipalityName = NA,
    RegisterHSAID = 1,
    RegisterNamn = "abc",
    Coverage = NA,
    ConfidenceIntervalLower = NA,
    ConfidenceIntervalHigher = NA,
    Standarddeviation = NA,
    Measurepopulation = NA,
    Exclusions = NA
  ) |>
    validate_vis() |>
    expect_equal("First cell does not contain correct keep or delete existing instruction")

  data.frame(
    RegionOrganisationId = "01",
    RegionName = "Stockholm",
    UnitName = "a",
    UnitHSAID = 1,
    HospitalName = NA,
    HospitalHSAID = NA,
    Rate = 0.5,
    Numerator = NA,
    Denominator = 10,
    ReasonCode = NA,
    PeriodReportedStartDate = lubridate::ymd("2000-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2000-12-31"),
    MeasureID = 1,
    HospitalOrganisationId = NA,
    CountryName = "Sverige",
    Value = NA,
    Cohort = NA,
    Version = 1,
    ReferenceIntervalRate = NA,
    ReferenceIntervalValue = NA,
    MunicipalityOrganisationID = NA,
    MunicipalityName = NA,
    RegisterHSAID = 1,
    RegisterNamn = "abc",
    Coverage = NA,
    ConfidenceIntervalLower = NA,
    ConfidenceIntervalHigher = NA,
    Standarddeviation = NA,
    Measurepopulation = NA,
    Exclusions = NA
  ) |>
    dplyr::add_row(
      RegionOrganisationId = "DELETE_EXISTING_DATA_FOR_REGISTER_HSAID:1",
      .after = 1
    ) |>
    validate_vis() |>
    expect_equal("First cell does not contain correct keep or delete existing instruction")

  data.frame(
    RegionOrganisationId = "01",
    RegionName = "Stockholm",
    UnitName = "a",
    UnitHSAID = 1,
    HospitalName = NA,
    HospitalHSAID = NA,
    Rate = 0.5,
    Numerator = NA,
    Denominator = 10,
    ReasonCode = NA,
    PeriodReportedStartDate = lubridate::ymd("2000-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2000-12-31"),
    MeasureID = 1,
    HospitalOrganisationId = NA,
    CountryName = "Riket",
    Value = NA,
    Cohort = NA,
    Version = 1,
    ReferenceIntervalRate = NA,
    ReferenceIntervalValue = NA,
    MunicipalityOrganisationID = NA,
    MunicipalityName = NA,
    RegisterHSAID = 1,
    RegisterNamn = "abc",
    Coverage = NA,
    ConfidenceIntervalLower = NA,
    ConfidenceIntervalHigher = NA,
    Standarddeviation = NA,
    Measurepopulation = NA,
    Exclusions = NA
  ) |>
    dplyr::add_row(
      RegionOrganisationId = "DELETE_EXISTING_DATA_FOR_REGISTER_HSAID:1",
      .before = 1
    ) |>
    validate_vis(as_data_frame = TRUE) |>
    expect_equal(
      data.frame(
        errors = paste0("MeasureID 1, period 2000-01-01-2000-12-31, RegionOrganisationId 01, unit/hospital name a is",
                        " missing Numerator where Rate is present")
      )
    )

})

test_that("check_unit_hospital works", {

  data.frame(
    MeasureID = 1,
    RegionOrganisationId = 14,
    PeriodReportedStartDate = lubridate::ymd("2024-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2024-12-31"),
    UnitName = "a",
    HospitalName = "b",
    UnitHSAID = 1,
    HospitalHSAID = NA,
    HospitalOrganisationId = NA
  ) |>
    check_unit_hospital() |>
    expect_equal(
      paste0("MeasureID 1, period 2024-01-01-2024-12-31, RegionOrganisationId 14 contains rows where both UnitName",
             " and HospitalName are present")
    )

  data.frame(
    MeasureID = 1:2,
    RegionOrganisationId = 14,
    PeriodReportedStartDate = lubridate::ymd("2024-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2024-12-31"),
    UnitName = "a",
    HospitalName = NA,
    UnitHSAID = 1,
    HospitalHSAID = 2,
    HospitalOrganisationId = 3
  ) |>
    check_unit_hospital() |>
    expect_equal(
      c(
        paste0("MeasureID 1, period 2024-01-01-2024-12-31, RegionOrganisationId 14 contains rows where more than one",
               " of UnitHSAID, HospitalHSAID, and HospitalOrganisationId are present"),
        paste0("MeasureID 2, period 2024-01-01-2024-12-31, RegionOrganisationId 14 contains rows where more than one",
               " of UnitHSAID, HospitalHSAID, and HospitalOrganisationId are present")
      )
    )
})

test_that("check_censored works", {

  data.frame(
    MeasureID = 1,
    PeriodReportedStartDate = lubridate::ymd("2024-01-01"),
    PeriodReportedEndDate = lubridate::ymd("2024-12-31"),
    RegionOrganisationId = "14",
    UnitName = "a",
    HospitalName = NA,
    ReasonCode = "MSK",
    Value = 1,
    Rate = 0.5,
    ConfidenceIntervalLower = 0.25,
    ConfidenceIntervalHigher = 0.75
  ) |>
    check_censored() |>
    expect_equal(
      c(
        paste0("MeasureID 1, period 2024-01-01-2024-12-31, RegionOrganisationId 14, unit/hospital name a",
               " has Value where a ReasonCode is present"),
        paste0("MeasureID 1, period 2024-01-01-2024-12-31, RegionOrganisationId 14, unit/hospital name a",
               " has Rate where a ReasonCode is present"),
        paste0("MeasureID 1, period 2024-01-01-2024-12-31, RegionOrganisationId 14, unit/hospital name a",
               " has ConfidenceIntervalLower where a ReasonCode is present"),
        paste0("MeasureID 1, period 2024-01-01-2024-12-31, RegionOrganisationId 14, unit/hospital name a",
               " has ConfidenceIntervalHigher where a ReasonCode is present")
      )
    )

})

test_that("check_reasoncode works", {

  data.frame(
    ReasonCode = c(NA, "MSK", "abc", "woops", "INV")
  ) |>
    check_reasoncode() |>
    expect_equal("ReasonCode contains disallowed values: abc, woops")

})

test_that("check_countryname works", {

  data.frame(
    CountryName = c("abc", "riket", "Riket", NA)
  ) |>
    check_countryname() |>
    expect_equal("CountryName contains disallowed values: abc, NA")

})

test_that("check_measureid works", {

  data.frame(
    MeasureID = c("abc", NA)
  ) |>
    check_measureid() |>
    expect_equal("MeasureID is NA in at least one row")

})

test_that("check_version works", {

  data.frame(
    Version = c("abc", NA, 1, 2)
  ) |>
    check_version() |>
    expect_equal("Version contains disallowed values: abc, NA")

})
