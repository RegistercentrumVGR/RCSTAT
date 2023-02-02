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

  # droplevels = TRUE,
  # add_cols = FALSE,
  # suffix = "_label",
  # as_character = FALSE
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
