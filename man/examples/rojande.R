
data_aggregated <- tibble::tribble(
  ~a, ~b, ~Count, ~Total,
  1,   1, 4,       26,
  1,   2, 16,      26,
  1,   1, 6,       26,
  2,   1, 24,      50,
  2,   2, 26,      50,
  3,   1, 7,       14,
  3,   2, 7,       14,
) |> dplyr::mutate(Proportion = Count/Total)

# Rows with less than 5 will be cleared
# frequencies rounded to nearest 10
# Proportion rounded to whole percents
# Groups with less than 15 in total are cleared
obfuscate_data(
  data_aggregated,
  freq_vars = "Count",
  tot_freq_var = "Total",
  statistics_vars = "Proportion"
)



data_aggregated <- tibble::tribble(
  ~a, ~b, ~n, ~x_non_missing, ~x_mean, ~x_sd,
  1,   1, 48, 12,             -0.593,   1.050,
  1,   2, 54, 16,             -0.308,   0.672,
  2,   1, 36, 31,              0.861,   0.502,
  2,   2, 66, 59,              0.401,   1.130,
)

# Clear statistics that is based on fewer than
# 15 observations
# Round statistics to 2 decimals
# Round frequencies, total and non_missing
obfuscate_data(
  data = data_aggregated,
  tot_freq_var = "x_non_missing",
  freq_var = "n",
  statistics_vars = c("x_sd", "x_mean")
)

# Example with multiple variables in result
data_aggregated <- tibble::tribble(
  ~a, ~b, ~n, ~x_non_missing, ~y_non_missing, ~x_mean, ~x_sd, ~y_mean, ~y_sd,
  1,   1, 48, 42,             36,             -0.593,   1.050, 0.269,  0.214,
  1,   2, 54, 42,             30,             -0.308,   0.672, 0.176,  0.120,
  2,   1, 36, 30,             30,              0.861,   0.502, 0.465,  0.224,
  2,   2, 66, 60,             54,              0.401,   1.130, 0.557,  0.370
)
res <- data_aggregated

for (v in c("x", "y")) {
  res <- obfuscate_data(
    data = res,
    tot_freq_var = c(paste0(v, "_non_missing"), "n"),
    statistics_vars = c(paste0(v, "_sd"), paste0(v, "_mean"))
  )
}
res
