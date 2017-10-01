#'
#' Bla.
#'
#' Vla.
#'
#' @export
#'
#' @param data A tibble containing the data.
#' @param cal_names A vector of strings containing the names of the samples used
#'   as calibrators,
#' @param cal_values A numeric vector with the known concentrations of those
#'   samples (must be in the same order).
#' @param cal_unit A string indicating the unit, see \code{\link{calc_factor}}.
#' @param target_unit A string indicating the unit, see
#'   \code{\link{calc_factor}}.
#' @param col_names A string indicating the name of the column used to identify
#'   the calibrators.
#' @param col_values A string indicating the name of the column holding the raw
#'   values.
#' @param col_target A string indicating the name of the column that will be
#'   created for the calculated concentration.
#' @param col_real A string indicating the name of the column that will be
#'   created for the calculated concentration.
#' @return A tibble containing all original and additional columns.
#'
calc_concentrations <- function(
  data,
  cal_names,
  cal_values,
  cal_unit,
  target_unit,
  col_names = name,
  col_values = values,
  col_target = conc,
  col_real = real,
  model_func = fit_lnln,
  interp_func = interp_lnln
){
  # make some handy operators available
  `%>%` <- magrittr::`%>%`
  `!!` <- rlang::`!!`
  `:=` <- rlang::`:=`

  stopifnot(
    tibble::is.tibble(data),
    is.vector(cal_names),
    is.vector(cal_values),
    is.character(cal_unit),
    is.character(target_unit),
    is.function(model_func),
    is.function(interp_func)
  )

  # enquose the give column names
  # for columns that appear to the left of := (i.e. are mutated) a string is
  # stored in col_*_name

  col_values <- rlang::enquo(col_values)
  col_target <- rlang::enquo(col_target)
  col_target_name <- rlang::quo_name(col_target)
  col_names <- rlang::enquo(col_names)
  col_real <- rlang::enquo(col_real)
  col_real_name <- rlang::quo_name(col_real)

  data <- data %>%
    dplyr::mutate(
      !! col_real_name := NA
    )

  # set known values for calibrators
  for (x in seq_along(cal_values)) {
    cal <- cal_names[[x]]
    value <- cal_values[[x]]

    data <- data %>%
      dplyr::mutate(
        !! col_real_name :=
          ifelse((!! col_names) == (!! cal), (!! value), (!! col_real))
      )
  }

  cals <- data %>%
    dplyr::filter(! is.na(!! col_real))

  real <- dplyr::pull(cals, !! col_real)
  measured <- dplyr::pull(cals, !! col_values)

  model <- model_func(x = real, y = measured)

  data <- data %>%
    dplyr::mutate(
      !! col_target_name := interp_func(y = !! col_values, model = !! model)
    )

  return(data)
}

#' @export
fit_lnln <- function(x, y) {
  x <- log(x)
  y <- log(y)

  return(lm(formula = y~x))
}

#' @export
interp_lnln <- function(y, model) {
  x <-
    exp(
      (log(y) - model$coefficients["(Intercept)"]) /
        model$coefficients["x"]
    )
  return(x)
}
