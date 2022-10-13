test_that("multiplication works", {
  df <- dummy_data

  ageint <- mutate(df, age_int(adate, bdate))
  age <- mutate(df, age(adate, bdate))

})
