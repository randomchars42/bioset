library(eckplates)
context("Calculations")

test_that("concentrations are calculated correctly using ln-ln-transform", {
  `%>%` <- magrittr::`%>%`

  data <- tibble::tibble(
    values = c(1, 2, 3, 4, 5, 6),
    name = c("a", "b", "c", "d", "e", "f")) %>%
    calc_concentrations(c("a", "b", "c"), c(1, 2, 3), "", "", name, values, conc)

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
    calc_variability(name, raw, conc)

  expect_equal(data$raw_n[[1]], 3)
  expect_equal(data$raw_n[[2]], 2)
  expect_equal(data$raw_mean[[1]], mean(c(100, 120, 89)))
  expect_equal(data$raw_mean[[2]], mean(c(200, 205)))
  expect_equal(data$raw_sd[[1]], sd(c(100, 120, 89)))
  expect_equal(data$raw_cv[[1]], sd(c(100, 120, 89)) / mean(c(100, 120, 89)))
  expect_equal(data$conc_mean[[1]], 2)
})
