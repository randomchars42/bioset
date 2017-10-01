library(eckplates)
context("Calculating concentrations")

test_that("concentrations are calculated correctly using ln-ln-transform", {
  `%>%` <- magrittr::`%>%`

  data <- tibble::tibble(
    values = c(1, 2, 3, 4, 5, 6),
    name = c("a", "b", "c", "d", "e", "f")) %>%
    calc_concentrations(c("a", "b", "c"), c(1, 2, 3), "", "", name, values, conc)

  expect_equal(data$real[[2]], 2)
  expect_equal(data$real[[5]], NA_real_)
  expect_equal(data$conc[[6]], 6)
})
