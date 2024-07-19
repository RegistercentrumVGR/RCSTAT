test_that("Metadata", {


  df1 <- data.frame(
    a = 1:3,
    b = c(TRUE, TRUE, FALSE),
    c = rep("abc", 3)
  )


  df2 <- data.frame(
    a = 1:4,
    b = c(TRUE, TRUE, FALSE, FALSE),
    c = rep("abc657", 4),
    d = rep("abcdefgh", 4)
  )

  dir.create("Output")

  sos_metadata(dfs = list(df1, df2),
               file_names = list("df1", "df2"),
               output_dir = "Output",
               zip_file_name = "zip_file",
               separator = ",",
               zip = FALSE)

  df1_res <- read.table("./Output/df1.csv", sep = ",", header = TRUE)
  df2_res <- read.table("./Output/df2.csv", sep = ",", header = TRUE)


  var_names_res <- readxl::read_xlsx("./Output/metadata.xlsx",
                                     sheet = "variabler")

  metadata_res <- readxl::read_xlsx("./Output/metadata.xlsx",
                                    sheet = "dataset")

  metadata <- data.frame(
    filename = c("df1.csv", "df2.csv"),
    encoding = rep("UTF-8", 2),
    separator = rep(",", 2),
    end_of_line = rep("CRLF", 2),
    nrows = c(3, 4),
    ncols = c(3, 4)
  )

  var_names <- data.frame(
    filename = c(rep("df1.csv", 3), rep("df2.csv", 4)),
    variable = c(c("a", "b", "c"), c("a", "b", "c", "d")),
    position_in_file = c(1:3, 1:4),
    type = c("integer", "logical", "character", "integer", "logical",
             "character", "character"),
    length =  c(1, 1, 3, 1, 1, 6, 8)
  )

  var_names <- tibble::tibble(var_names)
  metadata <- tibble::tibble(metadata)

  testthat::expect_equal(df1_res, df1)
  testthat::expect_equal(df2_res, df2)
  testthat::expect_equal(var_names_res, var_names)
  testthat::expect_equal(metadata_res, metadata)

  unlink("Output", recursive = TRUE)

})
