test_that("Statistics tables works ", {
  df <- dummy_data
  res_1 <- group_means(df, vars = c("x", "y"), a, b)

  res_2 <- group_proportions(df, a, b)

  res_3 <- proportion_missing(df, vars = c("c", "x", "y", "z"), a, b)


})
