test_that("api_works", {
  expect_no_error(api_register_meta(102))

  api_register_meta(102) |>
    expect_type("list") |>
    expect_length(2) |>
    names() |>
    expect_setequal(c("RegisterName", "ShortName"))
})

test_that("api_statistics works", {

  testthat::local_mocked_bindings(
    GET = function(...) {
      structure(
        list(
          url = "https://api.example.com/data",
          status_code = 200L,
          headers = list("Content-Type" = "application/json; charset=utf-8"),
          content = list(data = data.frame(x = 1:3)) |>
            jsonlite::toJSON() |>
            charToRaw()
        ),
        class = "response"
      )
    },
    .package = "httr"
  )

  api_statistics("ndr", "get_unit_labels") |>
    expect_equal(
      tibble::tibble(x = 1:3)
    )

})

test_that("api_url works", {

  api_url("ndr", "get_unit_labels") |>
    expect_equal(
      paste0(
        "https://stratum.registercentrum.se/stratum/api/statistics",
        "/ndr/get_unit_labels?apikey=MpuYxfbtp5I="
      )
    )

  api_url("ndr", "get_unit_labels", api_url = "local")  |>
    expect_equal(
      paste0(
        "https://localhost:8530",
        "/ndr/get_unit_labels?apikey=MpuYxfbtp5I="
      )
    )

  withr::with_options(
    list(rcstat.local_plumber = TRUE),
    api_url("ndr", "get_unit_labels")  |>
      expect_equal(
        paste0(
          "https://localhost:8530",
          "/ndr/get_unit_labels?apikey=MpuYxfbtp5I="
        )
      )
  )

  api_url("ndr", "get_unit_labels", api_url = "abc.def/")  |>
    expect_equal(
      paste0(
        "abc.def",
        "/ndr/get_unit_labels?apikey=MpuYxfbtp5I="
      )
    )

  api_url(
    "ndr",
    "get_unit_labels",
    api_url = "abc.def/",
    arguments = list(a = 1, b = 2)
  )  |>
    expect_equal(
      paste0(
        "abc.def",
        "/ndr/get_unit_labels?apikey=MpuYxfbtp5I=&a=1&b=2"
      )
    )

  api_url(
    "ndr",
    "get_unit_labels",
    api_url = "abc.def/",
    arguments = list(a = 1, b = 2),
    dev = TRUE
  )  |>
    expect_equal(
      paste0(
        "abc.def",
        "/ndr/get_unit_labels?apikey=MpuYxfbtp5I=&a=1&b=2&forcenewr=true"
      )
    )

})
