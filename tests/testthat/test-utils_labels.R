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

test_that("decode_data handles missing labels", {

  x <- data.frame(
    x = 1:4,
    y = 1:4
  )

  labels <- data.frame(
    ColumnName = rep(c("x", "y"), each = 3),
    ValueCode = rep(1:3, 2),
    ValueName = c("Volvo", "Saab", "Opel", "Test1", "Test2", "Test3")
  )

  # Not possible (?) to expect several warnings
  suppressWarnings({
    res <- decode_data(x, labels, missing_labels_na = TRUE, as_character = TRUE)
  })

  expected_res <- data.frame(
    x = c("Volvo", "Saab", "Opel", NA),
    y = c("Test1", "Test2", "Test3", NA)
  )

  expect_equal(res, expected_res)

  suppressWarnings({
    res <- decode_data(x, labels, missing_labels_na = FALSE,
                       as_character = TRUE)
  })

  expected_res <- data.frame(
    x = c("Volvo", "Saab", "Opel", 4),
    y = c("Test1", "Test2", "Test3", 4)
  )

  expect_equal(res, expected_res)

})

test_that("decode names works", {
  # Test 1
  df <- data.frame(
    "SubjectKey" = c(1, 2, 3),
    "RC_Age" = c(1, 2, 3),
    "RC_Gender" = c(1, 2, 3),
    "RC_Unit" = c("a", "b", "C"),
    "FollowUp_Unit" = c("a", "b", "b"),
    "County" = c("A", "B", "C")
  )

  labels <- data.frame(
    "ColumnName" = c(
      "RC_Age",
      "RC_Gender",
      "RC_Unit",
      "FollowUp_Unit"
    ),
    "Description" = c(
      "Ålder",
      "Kön",
      "Enhet",
      "Enhet"
    )
  )
  df_decoded <- decode_names(df, labels)

  expect_setequal(
    colnames(df_decoded),
    c(
      "Personnummer",
      "Ålder",
      "Kön",
      "Enhet (RC_Unit)",
      "Enhet (FollowUp_Unit)",
      "County"
    )
  )

  #Test 2
  df <- data.frame(
    "SubjectKey" = c(1, 2, 3),
    "RC_Age" = c(1, 2, 3),
    "RC_Gender" = c(1, 2, 3),
    "RC_Unit" = c("a", "b", "C"),
    "FollowUp_Unit" = c("a", "b", "b"),
    "County" = c("A", "B", "C")
  )

  labels <- data.frame(
    "ColumnName" = c(
      "RC_Age",
      "RC_Gender",
      "RC_Unit",
      "RC_Unit",
      "FollowUp_Unit"
    ),
    "Description" = c(
      "Ålder",
      "Kön",
      "Enhet",
      "Enhet",
      "Enhet"
    )
  )
  df_decoded <- decode_names(df, labels)

  expect_setequal(
    colnames(df_decoded),
    c(
      "Personnummer",
      "Ålder",
      "Kön",
      "Enhet (RC_Unit)",
      "Enhet (FollowUp_Unit)",
      "County"
    )
  )


})
