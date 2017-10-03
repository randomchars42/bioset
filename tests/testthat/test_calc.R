library(bioset)
context("Calculations")

test_that("concentration types are detected correctly", {

  expect_equal(bioset:::get_conc_type("µl"), FALSE)
  expect_equal(bioset:::get_conc_type("µg / dl"), FALSE)
  # ".g / .l", ".g / .m^3", "% w / v" (= 0.1 g / l)
  expect_equal(bioset:::get_conc_type("µg / ml"), "mass_vol")
  expect_equal(bioset:::get_conc_type("µg / dm^3"), "mass_vol")
  expect_equal(bioset:::get_conc_type("% w / v"), "mass_vol")
  # ".M", ".mol / .l",  ".mol / .m^3"
  expect_equal(bioset:::get_conc_type("mM"), "molar_vol")
  expect_equal(bioset:::get_conc_type("nmol / µl"), "molar_vol")
  expect_equal(bioset:::get_conc_type("pmol / cm^3"), "molar_vol")
  # ".l/.l", ".l / m^3", ".m^3/.m^3", ".m^3 / .l", "% v / v", "v / v"
  expect_equal(bioset:::get_conc_type("nl / ml"), "vol_vol")
  expect_equal(bioset:::get_conc_type("nl / mm^3"), "vol_vol")
  expect_equal(bioset:::get_conc_type("nm^3 / mm^3"), "vol_vol")
  expect_equal(bioset:::get_conc_type("nm^3 / ml"), "vol_vol")
  expect_equal(bioset:::get_conc_type("% v / v"), "vol_vol")
  expect_equal(bioset:::get_conc_type("v / v"), "vol_vol")
  # ".g / .g", "w / w", "% w / w"
  expect_equal(bioset:::get_conc_type("µg / kg"), "mass_mass")
  expect_equal(bioset:::get_conc_type("% w / w"), "mass_mass")
  expect_equal(bioset:::get_conc_type("w / w"), "mass_mass")
})

test_that("concentration are converted correctly", {
  # same concentration type
  expect_equal(calc_factor_conc("µg / ml", "ng / l"), 1000000)
  expect_warning(calc_factor_conc("% w / v", "µg / µl"))
  expect_equal(calc_factor_conc("g / l", "µg / µl"), 1)
  expect_equal(calc_factor_conc("g / dm^3", "µg / µl"), 1)
})
