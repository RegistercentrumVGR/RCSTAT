test_that("api_works", {
  expect_no_error(api_register_meta(102))

  api_register_meta(102) |>
    expect_type("list") |>
    expect_length(2) |>
    names() |>
    expect_setequal(c("RegisterName", "ShortName"))
})
