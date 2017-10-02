
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
interpolate_lnln <- function(y, model) {
  x <-
    exp(
      (log(y) - model$coefficients["(Intercept)"]) /
        model$coefficients["x"]
    )
  return(x)
}
