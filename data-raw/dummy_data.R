#
#
library(dplyr)
N = 100000

dummy_data <- data.frame(
  id = sample(1:100, size = N, replace = TRUE),
  adate = sample(seq(
    from = as.Date("1990-01-01"),
    to = as.Date("1999-12-31"),
    by = 1),
    size = N, replace = TRUE
  ),
  bdate = sample(seq(
    from = as.Date("2000-01-01"),
    to = as.Date("2009-12-31"),
    by = 1),
    size = N, replace = TRUE
  ),
  a = sample(c(NA_integer_, as.integer(1:5)), size = N, replace = TRUE),
  b = sample(c(NA_integer_, as.integer(1:50)), size = N, replace = TRUE),
  c = sample(c(NA_integer_, as.integer(1:15)), size = N, replace = TRUE),
  x = ifelse(rbinom(N, 1, 0.8) == 1, rnorm(N, mean = 0, sd = 1), as.numeric(NA)),
  y = ifelse(rbinom(N, 1, 0.8) == 1, rexp(N, rate = 2), as.numeric(NA)),
  z = ifelse(rbinom(N, 1, 0.8) == 1, rnorm(N, mean = 0, sd = 1), as.numeric(NA))
) %>%
  dplyr::arrange(id)

usethis::use_data(dummy_data, overwrite = TRUE)
