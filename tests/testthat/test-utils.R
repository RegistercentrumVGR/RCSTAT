test_that("multiplication works", {
  df <- dummy_data

  age <- mutate(df, age = age(adate, bdate))

})
