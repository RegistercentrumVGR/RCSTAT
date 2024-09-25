test_that("eq5d_index", {
  expect_equal(
    eq5d_index(1, 1, 1, 1, 1, levels = 3, type = "TTO"),
    eq5d_3l_index_tto(1, 1, 1, 1, 1)
  )
  expect_equal(
    eq5d_index(1, 1, 1, 1, 1, levels = 5, type = "TTO"),
    eq5d_5l_index_tto(1, 1, 1, 1, 1)
  )
  expect_equal(
    eq5d_index(1, 1, 1, 1, 1, levels = 5, type = "VAS"),
    eq5d_5l_index_vas(1, 1, 1, 1, 1)
  )
  expect_equal(
    eq5d_index(c("1,1,1,1,1"), levels = 3, type = "TTO", old = TRUE),
    eq5d_3l_index_tto_old(c("1,1,1,1,1"))
  )
  expect_warning(
    eq5d_index(1, 1, 1, 1, 1, levels = 5, type = "TTO", old = TRUE),
    "Argument `old` ignored"
  )
  expect_error(eq5d_index(1, 1, 1, 1, 1, type = "TTO", levels = 4))
  expect_error(eq5d_index(1, 1, 1, 1, 1, type = "VAS", levels = 3))
})


test_that("eq5d_3l_index_tto", {
  expect_equal(
    eq5d_3l_index_tto(1, 1, 1, 1, 1),
    0.9694
  )
  expect_equal(
    eq5d_3l_index_tto(1, 2, 2, 3, 3),
    0.4992
  )
  expect_equal(
    eq5d_3l_index_tto(2, 1, 3, 1, 1),
    0.724
  )
  expect_equal(
    eq5d_3l_index_tto(rep(1, 3), rep(1, 3), rep(1, 3), rep(1, 3), rep(1, 3)),
    c(0.9694, 0.9694, 0.9694)
  )
  expect_equal(
    eq5d_3l_index_tto(2, 1, 3, 1, NA_integer_),
    NA
  )
  expect_error(
    eq5d_3l_index_tto(2, 1, 3, 5, 1),
    "EQ5D-values must be: 1, 2, 3"
  )
  expect_error(
    eq5d_3l_index_tto(2, 1, 3, 1, "2"),
    "EQ5D-values must be numeric!"
  )
})


test_that("eq5d_5l_index_tto", {
  expect_equal(
    eq5d_5l_index_tto(3, 4, 5, 4, 3),
    0.5031
  )
  expect_equal(
    eq5d_5l_index_tto(
      rep(3, 10), rep(4, 10), rep(5, 10),
      rep(4, 10), rep(3, 10)
    ),
    rep(0.5031, 10)
  )
  expect_error(
    eq5d_5l_index_tto(2, 1, 3, 7, 1),
    "EQ5D-values must be: 1, 2, 3, 4, 5"
  )
  expect_error(
    eq5d_5l_index_tto(2, 1, 3, 1, "2"),
    "EQ5D-values must be numeric!"
  )
})


test_that("eq5d_3l_index_tto_old", {
  expect_equal(
    eq5d_3l_index_tto_old(c("2,2,2,2,3")),
    eq5d_3l_index_tto(2, 2, 2, 2, 3)
  )
  expect_equal(
    eq5d_3l_index_tto_old(c("2,2,2,2,3")),
    eq5d_3l_index_tto_old(c("22223"), split = "")
  )
})
