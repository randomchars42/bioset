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
#' @param col_names The name of the column used to identify the calibrators.
#' @param col_values The name of the column holding the raw values.
#' @param col_target The name of the column to created for the calculated
#'   concentration.
#' @param col_real The name of the column to create for the known
#'   concentrations.
#' @param col_recov The name of the column to create for the recovery of the
#'   calibrators.
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
  col_recov = recovery,
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
  col_recov <- rlang::enquo(col_recov)
  col_recov_name <- rlang::quo_name(col_recov)

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
      !! col_target_name := interp_func(y = !! col_values, model = !! model),
      !! col_recov_name := (!! col_target) / (!! col_real)
    )

  return(data)
}

#'
#' @export
#'
fit_lnln <- function(x, y) {
  x <- log(x)
  y <- log(y)

  return(lm(formula = y~x))
}

#'
#' @export
#'
interp_lnln <- function(y, model) {
  x <-
    exp(
      (log(y) - model$coefficients["(Intercept)"]) /
        model$coefficients["x"]
    )
  return(x)
}

#'
#' @export
#'
calc_variability <- function(data, groups, ...) {
  # make some handy operators available
  `%>%` <- magrittr::`%>%`
  `!!` <- rlang::`!!`
  `:=` <- rlang::`:=`

  stopifnot(tibble::is.tibble(data))

  groups <- rlang::enquo(groups)
  calc_for <- rlang::quos(...)

  for (i in seq_along(calc_for)) {
    message(i)
    message(rlang::quo_name(calc_for[[i]]))
    target <- calc_for[[i]]
    target_base <- rlang::quo_name(target)
    target_mean <- paste0(target_base, "_mean")
    target_n <- paste0(target_base, "_n")
    target_sd <- paste0(target_base, "_sd")
    target_cv <- paste0(target_base, "_cv")

    data <- data %>%
      dplyr::group_by(!! groups) %>%
      dplyr::mutate(
        !! target_n := n(),
        !! target_mean := mean(!! target),
        !! target_sd := sd(!! target),
        !! target_cv := sd(!! target) / mean(!! target)
      ) %>%
      dplyr::ungroup()
  }

  return(data)
}
