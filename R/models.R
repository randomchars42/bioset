#'
#' Fit a linear function.
#'
#' @description
#' Fit a linear function to the points with the given x and y coordinates.
#'
#' Can be used in \code{\link{set_calc_concentrations}} to fit a model to the
#' calibrators.
#'
#' @export
#' @seealso \code{\link{set_calc_concentrations}}
#' @family model
#' @param x The x coordinates of the points.
#' @param y The y coordinates of the points.
#' @return Line model.
#'
fit_linear <- function(x, y) {
  return(stats::lm(formula = y~x))
}

#'
#' Fit a linear function and plot it.
#'
#' @description
#' Fit a linear function to the points with the given x and y coordinates and
#' plot it.
#'
#' Can be used after \code{\link{set_calc_concentrations}} to visualise the fit
#' of the function.
#'
#' @export
#' @seealso \code{\link{set_calc_concentrations}}
#' @family model
#' @inheritParams fit_linear
#' @return Plot.
#'
plot_linear <- function(x, y) {
  return(stats::lm(formula = y~x))
}

#'
#' Interpolate using a linear function.
#'
#' @description
#' Interpolate values using a linear function and the coefficients
#' of the given line model.
#'
#' Can be used in \code{\link{set_calc_concentrations}} to interpolate
#' concentrations.
#'
#' @export
#' @seealso \code{\link{set_calc_concentrations}}
#' @family model
#' @param y The y coordinates of the points.
#' @param model The line model.
#' @return Vector of x values.
#'
interpolate_linear <- function(y, model) {
  x <- (y - model$coefficients["(Intercept)"]) / model$coefficients["x"]
  return(x)
}

#'
#' Fit a linear function to ln-ln transformed values.
#'
#' @description
#' Fit a linear function to the points with the given x and y coordinates after
#' transforming both coordinates using ln.
#'
#'
#' Can be used in \code{\link{set_calc_concentrations}} to fit a model to the
#' calibrators.
#'
#' @export
#' @seealso \code{\link{set_calc_concentrations}}
#' @family model
#' @inheritParams fit_linear
#' @return Line model.
#'
fit_lnln <- function(x, y) {
  x <- log(x)
  y <- log(y)

  return(stats::lm(formula = y~x))
}

#'
#' Fit a linear function to ln-ln transformed values and plot it.
#'
#' @description
#' Fit a linear function to the points with the given x and y coordinates after
#' transforming both coordinates using ln and plot them.
#'
#' Can be used after \code{\link{set_calc_concentrations}} to visualise the fit
#' of the function.
#'
#' @export
#' @seealso \code{\link{set_calc_concentrations}}
#' @family model
#' @inheritParams fit_linear
#' @return Plot.
#'
plot_lnln <- function(x, y) {
  x <- log(x)
  y <- log(y)

}

#'
#' Interpolate using a linear function on ln-ln transformed values.
#'
#' @description
#' Interpolate values using a linear function and the coefficients
#' of the given line model after transforming both coordinates using ln.
#'
#' Can be used in \code{\link{set_calc_concentrations}} to interpolate
#' concentrations.
#'
#' @export
#' @seealso \code{\link{set_calc_concentrations}}
#' @family model
#' @inheritParams interpolate_linear
#' @return Vector of x values.
#'
interpolate_lnln <- function(y, model) {
  x <-
    exp(
      (log(y) - model$coefficients["(Intercept)"]) /
        model$coefficients["x"]
    )
  return(x)
}
