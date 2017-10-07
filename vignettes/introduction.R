## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  echo = FALSE,
  comment = "#>"
)

## ----gh-installation, echo = TRUE, eval = FALSE--------------------------
#  # install.packages("devtools")
#  devtools::install_github("randomchars42/bioset")

## ------------------------------------------------------------------------
data <-
  utils::read.csv(
    system.file("extdata", "values.csv", package = "bioset"),
    header = FALSE)
rownames(data) <- LETTERS[1:4]

knitr::kable(
  data,
  row.names = TRUE,
  col.names = as.character(1:6))

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  set_read()

## ------------------------------------------------------------------------
data <- bioset::set_read(
  file_name = "values.csv",
  path = system.file("extdata", package = "bioset")
)
knitr::kable(data)

## ------------------------------------------------------------------------
data <-
  utils::read.csv(
    system.file("extdata", "names.csv", package = "bioset"),
    header = FALSE)
rownames(data) <- LETTERS[1:4]

knitr::kable(
  data,
  row.names = TRUE,
  col.names = as.character(1:6))

## ------------------------------------------------------------------------
data <-
  utils::read.csv(
    system.file("extdata", "values_names.csv", package = "bioset"),
    header = FALSE)
rownames(data) <- LETTERS[1:8]

knitr::kable(
  data,
  row.names = TRUE,
  col.names = as.character(1:6))

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  set_read(
#    additional_vars = c("name")
#  )

## ------------------------------------------------------------------------
data <- bioset::set_read(
  file_name = "values_names.csv",
  path = system.file("extdata", package = "bioset"),
  additional_vars = c("name")
)
knitr::kable(data)

## ------------------------------------------------------------------------
data <-
  utils::read.csv(
    system.file("extdata", "values_names_properties.csv", package = "bioset"),
    header = FALSE)
rownames(data) <- LETTERS[1:8]

knitr::kable(
  data,
  row.names = TRUE,
  col.names = as.character(1:6))

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  set_read(
#    additional_vars = c("name", "day")
#  )

## ------------------------------------------------------------------------
data <- bioset::set_read(
  file_name = "values_names_properties.csv",
  path = system.file("extdata", package = "bioset"),
  additional_vars = c("name", "day")
)

knitr::kable(data)

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  set_calc_concentrations(
#    data,
#    cal_names = c("CAL1", "CAL2", "CAL3", "CAL4"),
#    cal_values = c(1, 2, 3, 4) # ng / ml
#  )

## ------------------------------------------------------------------------
data <- bioset::set_calc_concentrations(
  data,
  cal_names = c("CAL1", "CAL2", "CAL3", "CAL4"),
  cal_values = c(1, 2, 3, 4) # ng / ml
)

knitr::kable(data)

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  data <- set_calc_variability(
#    data = data,
#    ids = sample_id,
#    value,
#    conc
#  )

## ------------------------------------------------------------------------
data <- bioset::set_calc_variability(
  data = data,
  ids = sample_id,
  value,
  conc
)

knitr::kable(data)

