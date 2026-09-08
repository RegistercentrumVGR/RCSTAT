test_that("table_one summarizes binary, categorical, and numeric variables", {
  df <- tibble::tribble(
    ~sex,     ~dead, ~event01, ~age,
    "Male",   TRUE,  1,        50,
    "Male",   FALSE, 0,        60,
    "Female", FALSE, 0,        70,
    "Female", TRUE,  1,        40,
    "Female", FALSE, 0,        80,
    "Male",   TRUE,  1,        30
  )

  res <- table_one(
    df,
    vars = c("sex", "dead", "event01", "age"),
    include_missing = TRUE,
    obfuscate_data = FALSE,
    group_cols = NULL
  )

  expected_res <- tibble::tribble(
    ~variable, ~category, ~value,
    "N",       " ",       "6",
    "dead",    " ",       "3 (50.0%)",
    "event01", " ",       "3 (50.0%)",
    "sex",     "Female",  "3 (50.0%)",
    "sex",     "Male",    "3 (50.0%)",
    "age",     " ",       "55.0 (18.7)"
  )

  expect_equal(tibble::as_tibble(res), expected_res)
})

test_that("table_one pivots wide with one column per group_cols combination", {
  df <- tibble::tribble(
    ~grp, ~sex,     ~age,
    "A",  "Male",   50,
    "A",  "Male",   60,
    "A",  "Female", 70,
    "B",  "Female", 40,
    "B",  "Female", 80,
    "B",  "Male",   30
  )

  res <- table_one(
    df,
    vars = c("sex", "age"),
    include_missing = TRUE,
    obfuscate_data = FALSE,
    group_cols = "grp"
  )

  expected_res <- tibble::tribble(
    ~variable, ~category, ~Alla,        ~A,           ~B,
    "N",       " ",       "6",          "3",          "3",
    "sex",     "Female",  "3 (50.0%)",  "1 (33.3%)",  "2 (66.7%)",
    "sex",     "Male",    "3 (50.0%)",  "2 (66.7%)",  "1 (33.3%)",
    "age",     " ",       "55.0 (18.7)", "60.0 (10.0)", "50.0 (26.5)"
  )

  expect_equal(tibble::as_tibble(res), expected_res)
})

test_that("table_one summarizes median_vars as median (5%-95%)", {
  df <- tibble::tibble(los = c(1, 2, 3, 4, 5))

  res <- table_one(
    df,
    vars = "los",
    median_vars = "los",
    include_missing = TRUE,
    obfuscate_data = FALSE,
    group_cols = NULL
  )

  expected_res <- tibble::tribble(
    ~variable, ~category, ~value,
    "N",       " ",       "5",
    "los",     " ",       "3.0 (1.2-4.8)"
  )

  expect_equal(tibble::as_tibble(res), expected_res)
})

test_that("table_one classifies logicals and 0/1 numerics as binary", {
  df <- tibble::tibble(
    flag_lgl = c(TRUE, TRUE, FALSE, FALSE),
    flag_num = c(1, 1, 0, 0),
    sex = c("M", "M", "F", "F")
  )

  res <- table_one(
    df,
    vars = c("flag_lgl", "flag_num", "sex"),
    include_missing = TRUE,
    obfuscate_data = FALSE,
    group_cols = NULL
  )

  expect_equal(sum(res$variable == "flag_lgl"), 1)
  expect_equal(sum(res$variable == "flag_num"), 1)
  expect_equal(sum(res$variable == "sex"), 2)
  expect_true(all(res$category[res$variable %in% c("flag_lgl", "flag_num")] == " "))
})

test_that("table_one decodes categorical variables using value_labels before lumping", {
  df <- tibble::tibble(region = c("1", "1", "2", "2", "3"))

  value_labels <- data.frame(
    ColumnName = "region",
    ValueCode = c("1", "2", "3"),
    ValueName = c("North", "South", "East")
  )

  res <- table_one(
    df,
    vars = "region",
    include_missing = TRUE,
    obfuscate_data = FALSE,
    group_cols = NULL,
    value_labels = value_labels
  )

  expect_setequal(
    res$category[res$variable == "region"],
    c("North", "South", "East")
  )
})

test_that("table_one classifies binary variables as binary even when value_labels would decode them", {
  df <- tibble::tibble(
    dead = c(1, 1, 0, 0),
    sex = c("M", "M", "F", "F")
  )

  value_labels <- data.frame(
    ColumnName = c("dead", "dead"),
    ValueCode = c(0, 1),
    ValueName = c("Nej", "Ja")
  )

  res <- table_one(
    df,
    vars = c("dead", "sex"),
    include_missing = TRUE,
    obfuscate_data = FALSE,
    group_cols = NULL,
    value_labels = value_labels
  )

  expect_equal(sum(res$variable == "dead"), 1)
  expect_equal(res$category[res$variable == "dead"], " ")
  expect_equal(res$value[res$variable == "dead"], "2 (50.0%)")
})

test_that("table_one leaves categorical values undecoded when value_labels is NULL", {
  df <- tibble::tibble(region = c("1", "1", "2", "2", "3"))

  res <- table_one(
    df,
    vars = "region",
    include_missing = TRUE,
    obfuscate_data = FALSE,
    group_cols = NULL,
    value_labels = NULL
  )

  expect_setequal(
    res$category[res$variable == "region"],
    c("1", "2", "3")
  )
})

test_that("table_one only decodes columns classified as categorical", {
  df <- tibble::tibble(
    age = c(20, 30, 40),
    region = c("1", "2", "1")
  )

  value_labels <- data.frame(
    ColumnName = c("age", "age", "region", "region"),
    ValueCode = c("20", "30", "1", "2"),
    ValueName = c("twenty", "thirty", "North", "South")
  )

  res <- table_one(
    df,
    vars = c("age", "region"),
    include_missing = TRUE,
    obfuscate_data = FALSE,
    group_cols = NULL,
    value_labels = value_labels
  )

  expect_equal(res$value[res$variable == "age"], "30.0 (10.0)")
  expect_setequal(res$category[res$variable == "region"], c("North", "South"))
})

test_that("table_one errors informatively when value_labels is missing required columns", {
  df <- tibble::tibble(region = c(1, 2))

  expect_error(
    table_one(
      df,
      vars = "region",
      include_missing = TRUE,
      obfuscate_data = FALSE,
      group_cols = NULL,
      value_labels = data.frame(ColumnName = "region", ValueName = "North")
    ),
    regexp = "ValueCode"
  )
})

test_that("table_one lumps categories exceeding max_categories", {
  df <- tibble::tibble(
    category = c(rep("a", 5), rep("b", 4), rep("c", 3), rep("d", 2), "e")
  )

  expect_message(
    res <- table_one(
      df,
      vars = "category",
      include_missing = TRUE,
      obfuscate_data = FALSE,
      max_categories = 3,
      group_cols = NULL
    ),
    regexp = "category.*5 categories"
  )

  expect_setequal(res$category[res$variable == "category"], c("a", "b", "Other"))

  other_row <- dplyr::filter(res, .data$variable == "category", .data$category == "Other")
  expect_equal(other_row$value, "6 (40.0%)")
})

test_that("table_one errors when a variable has an unsupported class", {
  df <- tibble::tibble(d = as.Date("2020-01-01"))

  expect_error(
    table_one(
      df,
      vars = "d",
      include_missing = TRUE,
      obfuscate_data = FALSE,
      group_cols = NULL
    ),
    regexp = "unsupported class"
  )
})

test_that("table_one errors when median_vars is not a subset of the numeric vars", {
  df <- tibble::tibble(sex = c("M", "F"), age = c(20, 30))

  expect_error(
    table_one(
      df,
      vars = c("sex", "age"),
      median_vars = "sex",
      include_missing = TRUE,
      obfuscate_data = FALSE,
      group_cols = NULL
    )
  )
})

test_that("table_one errors when vars and group_cols overlap", {
  df <- tibble::tibble(sex = c("M", "F"), age = c(20, 30))

  expect_error(
    table_one(
      df,
      vars = "sex",
      include_missing = TRUE,
      obfuscate_data = FALSE,
      group_cols = "sex"
    )
  )
})

test_that("prettify_table_one blanks repeated variable names and renames columns", {
  df <- tibble::tribble(
    ~variable, ~category, ~value,
    "N",       " ",       "6",
    "dead",    " ",       "3 (50.0%)",
    "sex",     "Female",  "3 (50.0%)",
    "sex",     "Male",    "3 (50.0%)",
    "age",     " ",       "55.0 (18.7)"
  )

  res <- prettify_table_one(df)

  expected_res <- tibble::tribble(
    ~Variabel, ~Utfall,  ~"Värde",
    "N",       " ",      "6",
    "dead",    " ",      "3 (50.0%)",
    "sex",     "Female", "3 (50.0%)",
    "",        "Male",   "3 (50.0%)",
    "age",     " ",      "55.0 (18.7)"
  )

  expect_equal(res, expected_res)
})

test_that("prettify_table_one can leave repeated variable names untouched", {
  df <- tibble::tribble(
    ~variable, ~category, ~value,
    "sex",     "Female",  "3 (50.0%)",
    "sex",     "Male",    "3 (50.0%)"
  )

  res <- prettify_table_one(df, blank_repeated = FALSE)

  expect_equal(res$Variabel, c("sex", "sex"))
})

test_that("prettify_table_one recodes variable identifiers via vars", {
  df <- tibble::tribble(
    ~variable, ~category, ~value,
    "sex",     "Female",  "3 (50.0%)",
    "sex",     "Male",    "3 (50.0%)",
    "age",     " ",       "55.0 (18.7)"
  )

  res <- prettify_table_one(
    df,
    vars = data.frame(
      ColumnName = c("sex", "age"),
      Description = c("Kön", "Ålder")
    )
  )

  expect_equal(res$Variabel, c("Kön", "", "Ålder"))
})

test_that("prettify_table_one errors on malformed vars", {
  df <- tibble::tribble(
    ~variable, ~category, ~value,
    "sex",     "Female",  "3 (50.0%)"
  )

  expect_error(
    prettify_table_one(df, vars = data.frame(ColumnName = "sex")),
    regexp = "Description"
  )
})

test_that("prettify_table_one handles table_one output without a value column", {
  df <- tibble::tribble(
    ~variable, ~category, ~Alla,      ~A,         ~B,
    "sex",     "Female",  "3 (50.0%)", "1 (50.0%)", "2 (66.7%)",
    "sex",     "Male",    "3 (50.0%)", "1 (50.0%)", "1 (33.3%)"
  )

  res <- prettify_table_one(df)

  expect_setequal(names(res), c("Variabel", "Utfall", "Alla", "A", "B"))
  expect_equal(res$Variabel, c("sex", ""))
})
