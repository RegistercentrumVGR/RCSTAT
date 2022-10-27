test_that("Age can be calculated ", {
  df <- dummy_data

  age <- dplyr::mutate(df, age = age(adate, bdate))

})
