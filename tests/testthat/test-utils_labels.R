test_that("set_factors works on data.frames", {

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

  dff <- set_factors(df, vl)

  expect_equal(dff$x, as.factor(letters[1:10]))
  expect_equal(dff$y, as.factor(letters[1:10]))

  # Check that droplevels argument works
  dff <- set_factors(df, vl, droplevels = FALSE)
  expect_equal(levels(dff$x), letters[1:20])

  dff <- set_factors(df, vl, droplevels = "x")

  expect_equal(levels(dff$x), letters[1:10])
  expect_equal(dff$y, as.factor(letters[1:10]))
  # Test if warning is thrown when there are
  # unlabeled levels in data.
  dff <- data.frame(x = 1:40, y = 1:10)
  expect_warning(set_factors(dff, vl))

})

test_that("Labels functions throw warnings when called without labels", {
  df <- data.frame(x = 1:10, y = 1:10)

  testthat::expect_error(SetVariableLabels(df = df))
  testthat::expect_error(SetVariableNames(df = df))
  testthat::expect_error(SetValueLabels(df = df))
  testthat::expect_error(set_factors(df = df))

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

  dff <- set_factors(df, vl)

  expect_equal(dff$x, as.factor(letters[1:10]))
  expect_equal(dff$y, as.factor(letters[1:10]))

  # Check that droplevels argument works
  dff <- set_factors(df, vl, droplevels = FALSE)
  expect_equal(levels(dff$x), letters[1:20])

  dff <- set_factors(df, vl, droplevels = "x")

  expect_equal(levels(dff$x), letters[1:10])
  expect_equal(dff$y, as.factor(letters[1:10]))
  # Test if warning is thrown when there are
  # unlabeled levels in data.
  dff <- data.frame(x = 1:40, y = 1:10)
  expect_warning(set_factors(dff, vl))

})
