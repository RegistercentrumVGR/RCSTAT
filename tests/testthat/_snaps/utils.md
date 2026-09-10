# prettify_table works

    Code
      prettify_table(data.frame(x_prop = 0.513, this_is_also_something = 0.123,
        this_is_a_third_guy = 0.789111, n.risk = -1), prop_vars = c(
        "this_is_also_something", "does_not_exist"))
    Message
      ! does_not_exist not found in names of `df`
    Output
        this_is_a_third_guy Andel this_is_also_something Antal i riskmängd
      1                 0.8  51.3                   12.3                -1

