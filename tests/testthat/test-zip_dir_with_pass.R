test_that("zip_dir_with_pass works", {

  skip_if_not(dir.exists("C:/Program Files/7-Zip/"))
  withr::local_path(new = "C:/Program Files/7-Zip/")

  dir <- withr::local_tempdir()
  f_df <- withr::local_tempfile(tmpdir = dir) |>
    normalizePath(winslash = "/", FALSE)

  dir_zip <- withr::local_tempdir()
  f_zip <- withr::local_tempfile(tmpdir = dir_zip) |>
    normalizePath(winslash = "/", FALSE)

  df <- data.frame(a = 1)
  data.table::fwrite(df, sprintf("%s.csv", f_df))

  zip_dir_with_pass(
    directory = dir, file = f_zip, pass = "abc123", sink_password = FALSE
  )
  expect_true(file.exists(sprintf("%s.zip", f_zip)))

  system(
    sprintf("7z x %s.zip -pwrongpassword -o%s", f_zip, withr::local_tempdir())
  ) |>
    expect_equal(2)

  system(sprintf("7z x %s.zip -pabc123 -o%s", f_zip, dir_zip)) |>
    expect_equal(0)

  expect_true(file.exists(sprintf("%s/%s.csv", dir_zip, gsub("C:/", "", f_df))))


  dir_zip <- withr::local_tempdir()
  f_zip <- withr::local_tempfile(tmpdir = dir_zip) |>
    normalizePath(winslash = "/", FALSE)

  zip_dir_with_pass(directory = dir, file = f_zip, pass = "\\abc123") |>
    expect_error()
})
