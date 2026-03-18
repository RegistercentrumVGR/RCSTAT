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
      args <- list(...)

      headers <- purrr::keep(args, \(x) inherits(x, "request")) |>
        purrr::pluck(1, "headers")

      message(paste0(names(headers), " ", headers, collapse = ", "))

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

  withr::with_options(
    list(rcstat.local_plumber = TRUE),
    expect_message(
      api_statistics("", ""),
      "x-data-scope 1, x-unit 0, x-role 900, x-registerid 100"
    )
  )

  withr::with_options(
    list(
      rcstat.local_plumber = TRUE,
      rcstat.local_scope = 3,
      rcstat.local_unit = 1000,
      rcstat.local_role = 9001,
      rcstat.local_register = -1
    ),
    expect_message(
      api_statistics("", ""),
      "x-data-scope 3, x-unit 1000, x-role 9001, x-registerid -1"
    )
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
        "http://127.0.0.1:8530",
        "/ndr/get_unit_labels?apikey=MpuYxfbtp5I="
      )
    )

  withr::with_options(
    list(rcstat.local_plumber = TRUE),
    api_url("ndr", "get_unit_labels")  |>
      expect_equal(
        paste0(
          "http://127.0.0.1:8530",
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
