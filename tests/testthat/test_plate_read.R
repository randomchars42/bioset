library(eckplates)
context("Reading Plates")

test_that("plates are read correctly", {
  plate1 <- plate_read(
    plate_num = 1,
    path = "./",
    additional_vars = c(),
    additional_sep = "[^[:alnum:]]+",
    name_scheme = "plate_#NUM#.csv",
    sep = ",",
    cols = 4,
    rows = 3
  )

  expect_equal(length(plate1), 3)
  expect_equal(nrow(plate1), 12)
  expect_equal(plate1$name[[5]], "B2")
})


test_that("plates with additional variables are read correctly", {
  plate2 <- plate_read(
    plate_num = 2,
    path = "./",
    additional_vars = c("prop1"),
    additional_sep = "[^[:alnum:]]+",
    name_scheme = "plate_#NUM#.csv",
    sep = ",",
    cols = 4,
    rows = 3
  )

  expect_equal(plate2$prop1[[10]], "PA4")
  expect_equal(plate2$prop1[[6]], NA_character_)
  expect_equal(plate2$prop1[[8]], "PB3_PB3X")
})
