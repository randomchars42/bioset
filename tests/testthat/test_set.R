library(bioset)
context("Handling sets")

test_that("sets are read correctly", {
  set1 <- set_read(num = 1, additional_vars = c("name"))

  expect_equal(length(set1), 5)
  expect_equal(nrow(set1), 12)
  expect_equal(set1$name[[5]], "B2")
  expect_equal(set1$position[[5]], "B2")
})

test_that("sets with additional variables are read correctly", {
  set2 <- set_read(num = 2, additional_vars = c("id", "prop1"))

  expect_equal(set2$prop1[[10]], "PA4")
  expect_equal(set2$prop1[[6]], NA_character_)
  expect_equal(set2$prop1[[8]], "PB3_PB3X")
})

test_that("sets with arbitrary dimensions are handled correctly", {
  expect_error(set_read(num = 3, cols = 5))
  expect_error(set_read(num = 3, cols = 3))
  expect_error(set_read(num = 3, rows = 3))
  expect_error(set_read(num = 3, rows = 30))
})

test_that("sets without additional names are read correctly", {
  set3 <- set_read(num = 3)
  set4 <- set_read(num = 4)

  expect_equal(set4$position[[24]], "X1")
  expect_equal(set3$position[[27]], "27x1")
})

test_that("concentrations are calculated correctly using ln-ln-transform", {
  `%>%` <- magrittr::`%>%`

  data <- tibble::tibble(
    value = c(1, 2, 3, 4, 5, 6),
    name = c("a", "b", "c", "d", "e", "f")) %>%
    set_calc_concentrations(
      cal_names = c("a", "b", "c"),
      cal_values = c(1, 2, 3))

  expect_equal(data$real[[2]], 2)
  expect_equal(data$real[[5]], NA_real_)
  expect_equal(data$conc[[6]], 6)
  expect_equal(data$recovery[[3]], 1)
  expect_equal(data$recovery[[4]], NA_real_)
})

test_that("variability is calculated correctly", {
  `%>%` <- magrittr::`%>%`

  data <- tibble::tibble(
    name = c("a", "b", "c", "d", "e", "f", "a", "b", "c", "d", "e", "f", "a"),
    raw = c(100, 200, 300, 400, 500, 600, 120, 205, 294, 426, 489, 560, 89),
    conc = c(2, 4, 6, 8, 10, 12, 1.9, 3.7, 6.3, 7.45, 10.4, 12.1, 2.1)) %>%
    set_calc_variability(name, conc, raw)

  expect_equal(data$raw_n[[1]], 3)
  expect_equal(data$raw_n[[2]], 2)
  expect_equal(data$raw_mean[[1]], mean(c(100, 120, 89)))
  expect_equal(data$raw_mean[[2]], mean(c(200, 205)))
  expect_equal(data$raw_sd[[1]], sd(c(100, 120, 89)))
  expect_equal(data$raw_cv[[1]], sd(c(100, 120, 89)) / mean(c(100, 120, 89)))
})
