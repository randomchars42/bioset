#'
#' Linear model functions.
#'
#' @description
#' Use these functions to calculate a linear model from data, plot the model and
#' use it to calculate x-values from the model data and y-values (inverse
#' function).
#'
#' Those function are intended to be used in [set_calc_concentrations] /
#' [sets_read] to be applied to the calibrators (`fit_linear`) and interpolate
#' concentrations from the raw values (`interpolate_linear`). Use `plot_linear`
#' to visually inspect goodness of fit.
#'
#' * `fit_linear`: Calculate a linear model from x and y.
#' * `plot_linear`: Draw the plot for the model that can be calculated with
#'   `fit_linear`. Uses [ggplot2::ggplot] if available.
#' * `interpolate_linear`: Inverse `fit_linear` using `model` and calculate x
#'   values from y values.
#'
#' @name models_linear
#' @param x The x coordinates of the points.
#' @param y The y coordinates of the points.
#' @param model The line model.
#' @return
#' * `fit_linear`: The line model.
#' * `plot_linear`: The plot.
#' * `interpolate_linear`: The calculated x values.
#' @seealso [set_calc_concentrations], [sets_read], [models_lnln]
#' @examples
#' # generate data
#' x <- c(1, 3, 4, 7)
#' y_known <- c(3.5, 6.5, 8, 12.5)  # x is known for these values
#' y_unknown <- c(5, 9.5, 11)       # we will calculate x for those
#'
#' model <- fit_linear(x = x, y = y_known)
#' model
#'
#' plot_linear(x = x, y = y_known)
#'
#' interpolate_linear(y = y_unknown, model)
#'
#' rm(x, y_known, y_unknown, model)
#'
NULL

#' @export
#' @rdname models_linear
fit_linear <- function(x, y) {
  return(stats::lm(formula = y~x))
}

#' @export
#' @rdname models_linear
plot_linear <- function(x, y) {
  if (package_available("ggplot2")) {
    data <- tibble::tibble(x = x, y = y)
    plot <- ggplot2::ggplot(ggplot2::aes(x = x, y = y), data = data) +
      ggplot2::geom_point() +
      ggplot2::labs(x = "ln(x)", y = "ln(y)") +
      ggplot2::stat_smooth(method = "lm", se = FALSE)
  } else {
    graphics::plot(x, y, pch = 20)
    graphics::abline(stats::lm(y ~ x), lwd = 2)
    plot <- grDevices::recordPlot()
    grDevices::dev.off()
  }

  return(plot)
}

#' @export
#' @rdname models_linear
interpolate_linear <- function(y, model) {
  x <- (y - model$coefficients["(Intercept)"]) / model$coefficients["x"]
  return(x)
}

#'
#' Model functions for data requiring ln-ln-transformation to fit a model.
#'
#' @description
#' Use these functions to transform x and y using the natural logarithm and
#' calculate a linear model, plot the model and use it to calculate x-values
#' from the model data and y-values (inverse function).
#'
#' Those function are intended to be used in [set_calc_concentrations] /
#' [sets_read] to be applied to the calibrators (`fit_lnln`) and interpolate
#' concentrations from the raw values (`interpolate_lnln`). Use `plot_lnln`
#' to visually inspect goodness of fit.
#'
#' * `fit_lnln`: Apply ln to x and y and calculate a linear model from x and y.
#' * `plot_lnln`: Draw the plot for the model that can be calculated with
#'   `fit_lnln`. Uses [ggplot2::ggplot] if available.
#' * `interpolate_lnln`: Inverse `fit_lnln` using `model` and calculate x
#'   values from y values.
#'
#' @name models_lnln
#' @param x The x coordinates of the points.
#' @param y The y coordinates of the points.
#' @param model The line model.
#' @return
#' * `fit_lnln`: The model.
#' * `plot_lnln`: The plot.
#' * `interpolate_lnln`: The calculated x values.
#' @seealso [set_calc_concentrations], [sets_read], [models_linear]
#' @examples
#' # generate data
#' x <- c(2.718282, 20.085537, 54.598150, 1096.633158)
#' # x is known for these values
#' y_known <- c(33.11545, 665.14163, 2980.95799, 268337.28652)
#' # we will calculate x for those:
#' y_unknown <- c(148.4132, 13359.7268, 59874.1417)
#'
#' model <- fit_lnln(x = x, y = y_known)
#' model
#'
#' plot_lnln(x = x, y = y_known)
#'
#' interpolate_lnln(y = y_unknown, model)
#'
#' rm(x, y_known, y_unknown, model)
#'
NULL

#' @export
#' @rdname models_lnln
fit_lnln <- function(x, y) {
  x <- log(x)
  y <- log(y)

  return(stats::lm(formula = y~x))
}

#' @export
#' @rdname models_lnln
plot_lnln <- function(x, y) {
  x <- log(x)
  y <- log(y)

  if (package_available("ggplot2")) {
    data <- tibble::tibble(x = x, y = y)
    plot <- ggplot2::ggplot(ggplot2::aes(x = x, y = y), data = data) +
      ggplot2::geom_point() +
      ggplot2::labs(x = "ln(x)", y = "ln(y)") +
      ggplot2::stat_smooth(method = "lm", se = FALSE)
  } else {
    graphics::plot(x, y, pch = 20)
    graphics::abline(stats::lm(y ~ x), lwd = 2)
    plot <- grDevices::recordPlot()
    grDevices::dev.off()
  }

  return(plot)
}

#' @export
#' @rdname models_lnln
interpolate_lnln <- function(y, model) {
  x <-
    exp(
      (log(y) - model$coefficients["(Intercept)"]) /
        model$coefficients["x"]
    )
  return(x)
}
