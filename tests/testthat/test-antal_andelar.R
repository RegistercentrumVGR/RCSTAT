test_that("count and proportions are calculated correctly:", {
  df <- dummy_data
  res_2 <- grupp_medel(df,vars = c("x", "y"), a, b)

  res_2 <- antal_andelar(df, vars = c("c", "x", "y", "z"), a, b)

  res_2 <- andel_missing(df, vars = c("c", "x", "y", "z"), a, b)


})
