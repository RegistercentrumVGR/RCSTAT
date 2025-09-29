# get_aggregate_value works with distinct_cols

    Code
      get_aggregate_value(df, group_cols = "county", vars = list(mean = "hba1c"))
    Output
        hba1c_mean hba1c_std total county
      1        2.5 1.2909944     4   Alla
      2        1.5 0.7071068     2      1
      3        3.5 0.7071068     2      2

---

    Code
      get_aggregate_value(df, group_cols = "county", vars = list(mean = "hba1c"),
      distinct_cols = "SubjectID", arrange_by = "date")
    Output
        hba1c_mean hba1c_std total county
      1        1.5 0.7071068     2   Alla
      2        1.5 0.7071068     2      1
      3        3.5 0.7071068     2      2

# get_aggregate_value works with marginal_cols

    Code
      get_aggregate_value(df = df, group_cols = "county", vars = list(count = "year"),
      marginal_cols = "county")
    Output
        year total county
      1 2019    52   Alla
      2 2020    48   Alla
      3 2019    20      1
      4 2019    32      2
      5 2020    26      1
      6 2020    22      2

---

    Code
      res1
    Output
        total year county
      1   100 Alla   Alla
      2    52 2019   Alla
      3    48 2020   Alla
      4    46 Alla      1
      5    54 Alla      2
      6    20 2019      1
      7    32 2019      2
      8    26 2020      1
      9    22 2020      2

---

    Code
      get_aggregate_value(df = df, group_cols = c("county", "year"), vars = list(
        prop = "ind"), marginal_cols = NULL)
    Output
        county year ind_n  ind_prop total
      1      1 2019     9 0.4500000    20
      2      1 2020    11 0.4230769    26
      3      2 2019    12 0.3750000    32
      4      2 2020    11 0.5000000    22

# add_reason_col works

    Code
      get_aggregate_value(df, group_cols = NULL, vars = list(mean = "x"),
      marginal_cols = NULL, obfuscate_data = TRUE, add_reason_col = TRUE)
    Output
        x_mean x_std total obfuscated_reason
      1     NA    NA    10            N < 15

---

    Code
      get_aggregate_value(df, group_cols = NULL, vars = list(median = "x"),
      marginal_cols = NULL, obfuscate_data = TRUE, add_reason_col = TRUE)
    Output
        x_median x_quant_5 x_quant_25 x_quant_75 x_quant_95 total obfuscated_reason
      1       NA        NA         NA         NA         NA    10            N < 15

---

    Code
      get_aggregate_value(df, group_cols = NULL, vars = list(mean = "x"),
      marginal_cols = NULL, obfuscate_data = TRUE, add_reason_col = TRUE)
    Output
        x_mean x_std total obfuscated_reason
      1     NA    NA    10            N < 15

---

    Code
      get_aggregate_value(df, vars = list(prop_count = "x"), obfuscate_data = TRUE,
      add_reason_col = TRUE, group_cols = NULL, pivot_prop_count = TRUE)
    Output
      # A tibble: 3 x 5
        total x       x_n x_prop x_obfuscated_reason  
        <dbl> <chr> <dbl>  <dbl> <chr>                
      1   230 0         0   0    rounded to nearest 5%
      2   230 1       200   0.85 <NA>                 
      3   230 2        30   0.13 <NA>                 

