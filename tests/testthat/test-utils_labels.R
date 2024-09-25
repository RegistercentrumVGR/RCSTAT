test_that("decode_data works on data.frames", {
  # Test set_factors on dummy data
  df <- data.frame(x = 1:10, y = 1:10)
  vl <- dplyr::bind_rows(
    data.frame(
      ColumnName = rep("x", 20),
      ValueCode = 1:20,
      ValueName = letters[1:20]
    ),
    data.frame(
      ColumnName = rep("y", 10),
      ValueCode = 1:10,
      ValueName = letters[1:10]
    )
  )

  dff <- decode_data(df, vl)

  expect_equal(dff$x, as.factor(letters[1:10]))
  expect_equal(dff$y, as.factor(letters[1:10]))

  # Check that droplevels argument works
  dff <- decode_data(df, vl, droplevels = FALSE)
  expect_equal(levels(dff$x), letters[1:20])

  dff <- decode_data(df, vl, droplevels = "x")

  expect_equal(levels(dff$x), letters[1:10])
  expect_equal(dff$y, as.factor(letters[1:10]))
  # Test if warning is thrown when there are
  # unlabeled levels in data.
  dff <- data.frame(x = 1:40, y = 1:10)
  expect_warning(decode_data(dff, vl))

  #- Test with options droplevels = TRUE,
  #- add_cols = FALSE, suffix = "_label", as_character = FALSE
  df <- data.frame(x = 1:10, y = 1:10)

  dfc <- decode_data(df, vl, as_character = TRUE)

  expect_equal(dfc, data.frame(x = letters[1:10], y = letters[1:10]))

  dfac <- decode_data(df, vl, as_character = TRUE, add_cols = TRUE)
  dfr <- data.frame(
    x = 1:10,
    x_label = letters[1:10],
    y = 1:10,
    y_label = letters[1:10]
  )
  expect_equal(dfac, dfr)
})

test_that("decode_data works with form_id", {
  skip_on_ci()

  df <- RCDBT::GetDatalayer(2398) |>
    dplyr::distinct(I_Unit)

  labels <- RCDBT::GetValueLabels(2398)

  df_decoded <- df |>
    decode_data(labels = labels, as_character = TRUE)

  expected <- data.frame(
    I_Unit = c(
      "Barndiabetes SUS Lund",
      "DSBUS Göteborg Barnklinik",
      "Linköping Barnklinik",
      "Borås Barnklinik",
      "Helsingborg Barnklinik",
      "Ängelholm Barnklinik",
      "Nyköping Barnklinik",
      "Kalmar Barnklinik",
      "Kristianstad Barnklinik",
      "Kungsbacka Barnklinik",
      "Trollhättan Barnklinik",
      "Visby Barnklinik",
      "Växjö Barnklinik",
      "Halmstad Barnklinik",
      "Örebro Barnklinik",
      "Östersund Barnklinik",
      "Lidköping Barnklinik",
      "Skövde Barnklinik",
      "Karlstad Barnklinik",
      "ALB Solna Barnklinik",
      "Uddevalla Barnklinik",
      "Norrköping Barnklinik",
      "Eskilstuna Barnklinik",
      "Falun Barnklinik",
      "Karlskrona Barnklinik",
      "Hudiksvall Barnklinik",
      "Ystad Barnklinik",
      "Jönköping Barnklinik",
      "Västerås Barnklinik",
      "Uppsala Barnklinik",
      "Västervik Barnklinik",
      "Sachsska Barnklinik",
      "Barndiabetes SUS Malmö",
      "Sundsvall Barnklinik",
      "ALB Huddinge Barnklinik",
      "Gävle Barnklinik",
      "Umeå Barnklinik",
      "Skellefteå Barnklinik",
      "Gällivare Barnklinik",
      "Örnsköldsvik Barnklinik",
      "Sollefteå Barnklinik",
      "Luleå Barnklinik",
      "Simrishamn Barnklinik"
    )
  )

  expect_equal(df_decoded, expected)

  df_decoded <- df |>
    decode_data(form_id = 2398, as_character = TRUE)

  expect_equal(df_decoded, expected)

  df_decoded <- df |>
    decode_data(labels = labels, form_id = 2398, as_character = TRUE)

  expect_equal(df_decoded, expected)

  expect_error(
    decode_data(df, labels = NULL, form_id = NULL),
    regexp = "Both labels and form_id can not be null"
  )
})

test_that("decode_data handles character value codes", {
  labels <- data.frame(
    ColumnName = "x",
    ValueCode = 1:3 |> as.character(),
    ValueName = c("a", "b", "c")
  ) |>
    dplyr::bind_rows(
      data.frame(
        ColumnName = "y",
        ValueCode = c("x", "y", "z"),
        ValueName = c("abc", "def", "ghi")
      )
    ) |>
    dplyr::bind_rows(
      data.frame(
        ColumnName = "z",
        ValueCode = 0:1 |> as.character(),
        ValueName = c("false", "true")
      )
    )

  df <- data.frame(
    x = rep(1:3, 4)[1:10],
    y = rep(c("x", "y", "z"), 4)[1:10],
    z = rep(c(TRUE, FALSE), 5)
  )

  df_decoded <- df |>
    decode_data(labels = labels, as_character = TRUE)

  expected <- data.frame(
    x = rep(c("a", "b", "c"), 4)[1:10],
    y = rep(c("abc", "def", "ghi"), 4)[1:10],
    z = rep(c("true", "false"), 5)
  )

  expect_equal(df_decoded, expected)
})

test_that("set_factors works on data.tables", {
  # Test set_factors on dummy data
  df <- data.table::data.table(x = 1:10, y = 1:10)
  vl <- rbind(
    data.table::data.table(
      ColumnName = rep("x", 20),
      ValueCode = 1:20,
      ValueName = letters[1:20]
    ),
    data.table::data.table(
      ColumnName = rep("y", 10),
      ValueCode = 1:10,
      ValueName = letters[1:10]
    )
  )

  dff <- decode_data(df, vl)

  expect_equal(dff$x, as.factor(letters[1:10]))
  expect_equal(dff$y, as.factor(letters[1:10]))

  # Check that droplevels argument works
  dff <- decode_data(df, vl, droplevels = FALSE)
  expect_equal(levels(dff$x), letters[1:20])

  dff <- decode_data(df, vl, droplevels = "x")

  expect_equal(levels(dff$x), letters[1:10])
  expect_equal(dff$y, as.factor(letters[1:10]))
  # Test if warning is thrown when there are
  # unlabeled levels in data.
  dff <- data.frame(x = 1:40, y = 1:10)
  expect_warning(decode_data(dff, vl))

  dt <- data.table::setDT(data.frame(x = 1:10, y = 1:10))

  dtc <- decode_data(dt, vl, as_character = TRUE)

  expect_equal(dtc$x, letters[1:10])
  expect_equal(dtc$y, letters[1:10])

  dt <- data.table::setDT(data.frame(x = 1:10, y = 1:10))

  dtac <- decode_data(dt, vl, as_character = TRUE, add_cols = TRUE)

  dtr <- data.table::data.table(
    x = 1:10,
    x_label = letters[1:10],
    y = 1:10,
    y_label = letters[1:10]
  )
  expect_equal(dtac, dtr)
})
