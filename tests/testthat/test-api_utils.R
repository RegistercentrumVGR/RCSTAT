test_that("extract_function_call_works", {
  expect_no_error(
    extract_function_call(
      paste0(
        "https://ndr.registercentrum.se/stratum/api",
        "/statistics/ndr/soklistan?",
        "apikey=bK3H9bwaG4o%253D&from_date=2008-01-01&to_date=2025-01-16"
      )
    )
  )

  extract_function_call(
    paste0(
      "https://ndr.registercentrum.se/stratum/api",
      "/statistics/ndr/soklistan?",
      "apikey=bK3H9bwaG4o%253D&from_date=2008-01-01&to_date=2025-01-16"
    )
  ) |>
    expect_type("character") |>
    expect_length(1) |>
    expect_identical("soklistan(from_date='2008-01-01', to_date='2025-01-16')")
})
