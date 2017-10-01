library(eckplates)
context("Reading Plates")

test_that("plates are read correctly", {
  plate1 <- plate_read(
    plate_num = 1,
    path = "./",
    additional_vars = c("name"),
    additional_sep = "[^[:alnum:]]+",
    file_name = "plate_#NUM#.csv",
    sep = ",",
    cols = NULL,
    rows = NULL
  )

  expect_equal(length(plate1), 4)
  expect_equal(nrow(plate1), 12)
  expect_equal(plate1$name[[5]], "B2")
  expect_equal(plate1$position[[5]], "B2")
})

test_that("plates with additional variables are read correctly", {
  plate2 <- plate_read(
    plate_num = 2,
    path = "./",
    additional_vars = c("name", "prop1"),
    additional_sep = "[^[:alnum:]]+",
    file_name = "plate_#NUM#.csv",
    sep = ",",
    cols = NULL,
    rows = NULL
  )

  expect_equal(plate2$prop1[[10]], "PA4")
  expect_equal(plate2$prop1[[6]], NA_character_)
  expect_equal(plate2$prop1[[8]], "PB3_PB3X")
})

test_that("plates with arbitrary dimensions are handled correctly", {
  expect_error(
    plate_read(
      plate_num = 3,
      path = "./",
      additional_vars = c(),
      additional_sep = "[^[:alnum:]]+",
      file_name = "plate_#NUM#.csv",
      sep = ",",
      cols = 5,
      rows = NULL
    ))
  expect_error(
    plate_read(
      plate_num = 3,
      path = "./",
      additional_vars = c(),
      additional_sep = "[^[:alnum:]]+",
      file_name = "plate_#NUM#.csv",
      sep = ",",
      cols = 3,
      rows = NULL
    ))
  expect_error(
    plate_read(
      plate_num = 3,
      path = "./",
      additional_vars = c(),
      additional_sep = "[^[:alnum:]]+",
      file_name = "plate_#NUM#.csv",
      sep = ",",
      cols = NULL,
      rows = 3
    ))
  expect_error(
    plate_read(
      plate_num = 3,
      path = "./",
      additional_vars = c(),
      additional_sep = "[^[:alnum:]]+",
      file_name = "plate_#NUM#.csv",
      sep = ",",
      cols = NULL,
      rows = 30
    ))
})

test_that("plates without additional names are read correctly", {
  plate3 <- plate_read(
    plate_num = 3,
    path = "./",
    additional_vars = c(),
    additional_sep = "[^[:alnum:]]+",
    file_name = "plate_#NUM#.csv",
    sep = ",",
    cols = NULL,
    rows = NULL
  )
  plate4 <- plate_read(
    plate_num = 4,
    path = "./",
    additional_vars = c(),
    additional_sep = "[^[:alnum:]]+",
    file_name = "plate_#NUM#.csv",
    sep = ",",
    cols = NULL,
    rows = NULL
  )

  expect_equal(plate4$position[[24]], "X1")
  expect_equal(plate3$position[[27]], "27x1")
})
