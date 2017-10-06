exp_mass_si <- c(0, -1, -2, -3, -4, -5, -6)
names(exp_mass_si) <- c("kg", "g", "mg", "\u00B5g", "ng", "pg", "fg")

exp_vol <- c(0, -1, -1, -2, -2, -3, -3, -4, -5, -6)
names(exp_vol) <- c("m^3", "l", "dm^3", "ml", "cm^3", "\u00B5l", "mm^3", "nl", "pl", "fl")

exp_molar_si <- c(0, -1, -2, -3, -4, -5)
names(exp_molar_si) <- c("mol", "mmol", "\u00B5mol", "nmol", "pmol", "fmol")

exp_molar_metric <- c(0, -1, -2, -3, -4, -5)
names(exp_molar_metric) <- c("M", "mM", "\u00B5M", "nM", "pM", "fM")

numerator_base <- c("g", "mol", "l", "g")
names(numerator_base) <- c("mass_vol", "molar_vol", "vol_vol", "mass_mass")

denominator_base <- c("l", "l", "l", "g")
names(denominator_base) <- c("mass_vol", "molar_vol", "vol_vol", "mass_mass")

#'
#' Get a factor to convert concentrations.
#'
#' @description
#' Calculate a factor to convert concentration "A" into concentration "B".
#'
#' @details
#' The following concentrations can be converted:
#'
#' mass / volume:
#' ".g / .l", ".g / .m^3", "\% w / v"
#'
#' molar / volume:
#' ".M", ".mol / .l",  ".mol / .m^3"
#'
#' volume / volume:
#' ".l/.l", ".l / m^3", ".m^3/.m^3", ".m^3 / .l", "\% v / v", "v / v"
#'
#' mass / mass:
#' ".g / .g", "w / w", "\% w / w"
#'
#' Where "." symbolises a metric prefix (see [calc_factor_prefix()]) :
#'
#' For g, l, mol and M: m (milli), µ (micro), n (nano), p (pico) and f (femto).
#'
#' For g you might use k (kilo) as well.
#'
#' For m^3 (cubic metres): d (deci), c (centi) and m (milli)
#'
#' Note: \% w / v is (incorrectly) taken as a short hand for 0.1 g / l.
#'
#' @family conversion functions
#' @export
#' @param from A string containing the units of concentration A.
#' @param to A string containing the units of concentration B.
#' @param molar_mass The molar mass of the solute (g / mol).
#' @param density_solute The density of the solute (g / l).
#' @param density_solution The density of the solution (g / l), not the solvent!
#' @return The factor to convert A into B.
#' @examples
#' calc_factor_conc(from = "ng / ml", to = "g / l")
#' calc_factor_conc(from = "ng / ml", to = "mmol / l", molar_mass = 150000)
#'
calc_factor_conc <- function(
  from,
  to,
  molar_mass = 0,
  density_solute = 0,
  density_solution = 0) {

  from <- enc2utf8(from)
  to <- enc2utf8(to)

  from_type <- get_conc_type(from)
  to_type <- get_conc_type(to)

  stopifnot(
    is_number(molar_mass),
    is_number(density_solute),
    is_number(density_solution),
    is.character(from_type),
    is.character(to_type)
  )

  #throw_message('calc_factor_conc - from: ', from)
  #throw_message('calc_factor_conc - to: ', to)

  if (needs_density_solute(from, to) && density_solute == 0) {
    throw_error(
      "The density of the solute is required to convert ", from, " to ", to)
  }

  if (needs_density_solution(from, to) && density_solution == 0) {
    throw_error(
      "The density of the solution (not the solvent!) is required to convert ",
      from, " to ", to)
  }

  if (needs_molar_mass(from, to) && molar_mass == 0) {
    throw_error(
      "The molar mass is required to convert ", from, " to ", to)
  }

  factor <- 1

  # convert from "from" to the base unit of its type
  # (one of "g / l", "mol / l", "l / l", "g / g")
  # see numerator_base / denominator_base
  factor <- factor * bring_to_base(from, from_type)

  # converting between different types requires densities / molar mass
  if (from_type != to_type) {
    # all units are now on of "g / l", "mol / l", "l / l", "g / g", according
    # to their type
    # see above
    # all units will be converted from "g / l", "mol / l", "l / l", "g / g",
    # according to their type
    # see below

    if (from_type == "mass_vol" && to_type == "molar_vol") {
      # mass_vol -> molar_vol
      # x [g / l] / m [g / mol] = y [mol / l]
      factor <- factor / molar_mass
    } else if (from_type == "molar_vol" && to_type == "mass_vol") {
      # molar_vol -> mass_vol
      # x [mol / l] * m [g / mol] = y [g / l]
      factor <- factor * molar_mass
    } else if (from_type == "mass_vol" && to_type == "vol_vol") {
      # mass_vol -> vol_vol
      # x [g / l] / dsolute [g / l] = y [l / l]
      factor <- factor / density_solute
    } else if (from_type == "vol_vol" && to_type == "mass_vol") {
      # vol_vol -> mass_vol
      # x [l / l] * dsolute [g / l] = y [g / l]
      factor <- factor * density_solute
    } else if (from_type == "mass_vol" && to_type == "mass_mass") {
      # mass_vol -> mass_mass
      # x [g / l] / dsolution [g / l] = y [g / g]
      factor <- factor / density_solution
    } else if (from_type == "mass_mass" && to_type == "mass_vol") {
      # mass_mass -> mass_vol
      # x [g / g] * dsolution [g / l] = y [g / l]
      factor <- factor * density_solution
    } else if (from_type == "molar_vol" && to_type == "vol_vol") {
      # molar_vol -> vol_vol = molar_vol -> mass_vol -> vol_vol
      # x [mol / l] * m [g / mol] / dsolute [g / l] = y [l / l]
      factor <- factor * molar_mass / density_solute
    } else if (from_type == "vol_vol" && to_type == "molar_vol") {
      # vol_vol -> molar_vol = vol_vol -> mass_vol -> molar_vol
      # x [l / l] * dsolute [g / l] / m [g / mol] = y [mol / l]
      factor <- factor * density_solute / molar_mass
    } else if (from_type == "molar_vol" && to_type == "mass_mass") {
      # molar_vol -> mass_mass = molar_vol -> mass_vol -> mass_mass
      # x [mol / l] * m [g / mol] / dsolution [g / l] = y [g / g]
      factor <- factor * molar_mass / density_solution
    } else if (from_type == "mass_mass" && to_type == "molar_vol") {
      # mass_mass -> molar_vol = mass_mass -> mass_vol -> molar_vol
      # x [g / g] * dsolution [g / l] / m [g / mol] = y [mol / l]
      factor <- factor * density_solution / molar_mass
    } else if (from_type == "mass_mass" && to_type == "vol_vol") {
      # mass_mass -> vol_vol
      # x [g / g] / dsolute [g / l] * dsolution [g / l] = y [l / l]
      factor <- factor / density_solute * density_solution
    } else if (from_type == "vol_vol" && to_type == "vol_vol") {
      # vol_vol -> mass_mass
      # x [l / l] * dsolute [g / l] / dsolution [g / l] = y [g / g]
      factor <- factor * density_solute / density_solution
    }
  }

  # convert to "to" from the base unit of its type
  factor <- factor / bring_to_base(to, to_type)

  return(factor)
}

#'
#' Get a factor to convert metric prefixes.
#'
#' @description
#' Get a factor to convert metric prefixes into one another.
#'
#' @details
#' Convert, e.g. "kg" to "µg". You can convert ".g", ".l", ".mol", ".M", ".m^3"
#' (cubic metres), where "." symbolises a metric prefix:
#'
#' For g, l, mol and M: m (milli), µ (micro), n (nano), p (pico) and f (femto).
#'
#' For g you might use k (kilo) as well.
#'
#' For m^3 (cubic metres): d (deci), c (centi) and m (milli)
#'
#' "." symbolises a metric prefix:
#'
#' For g, l, mol and M: m (milli), µ (micro), n (nano), p (pico) and f (femto).
#'
#' For g you might use k (kilo) as well.
#'
#' For m^3 (cubic metres): d (deci), c (centi) and m (milli)
#'
#' @export
#' @family conversion functions
#' @param from A string containing the prefixed unit A.
#' @param to A string containing the prefixed unit B.
#' @return A factor for multiplication with the value.
#' @examples
#' calc_factor_prefix(from = "ng", to = "kg")
#' calc_factor_prefix(from = "dm^3", to = "cm^3")
#' calc_factor_prefix(from = "fl", to = "pl")
#' calc_factor_prefix(from = "pmol", to = "nmol")
#' calc_factor_prefix(from = "pM", to = "nM")
#'
calc_factor_prefix <- function(from, to) {
  from <- enc2utf8(from)
  to <- enc2utf8(to)

  factor <- 1

  if (from %in% names(exp_mass_si) && to %in% names(exp_mass_si)) {
    factor <- 1000^(exp_mass_si[from]-exp_mass_si[to])
  } else if (from %in% names(exp_vol) && to %in% names(exp_vol)) {
    factor <- 1000^(exp_vol[from]-exp_vol[to])
  } else if (from %in% names(exp_molar_si) && to %in% names(exp_molar_si)) {
    factor <- 1000^(exp_molar_si[from]-exp_molar_si[to])
  } else if (
    from %in% names(exp_molar_metric) && to %in% names(exp_molar_metric)) {
    factor <- 1000^(exp_molar_metric[from]-exp_molar_metric[to])
  } else {
    throw_error("Could not convert ", from, " to ", to)
  }

  names(factor) <- NULL

  return(factor)
}

#'
#' Convert between metric prefixes.
#'
#' @description
#' A convenience wrapper around [calc_factor_conc()].
#'
#' @inherit calc_factor_conc details
#'
#' @export
#' @family conversion functions
#' @param x The value to convert.
#' @inheritParams calc_factor_conc
#' @return The converted value.
#' @examples
#' convert_conc(x = 2, from = "ng / ml", to = "g / l")
#' convert_conc(x = 2, from = "ng / ml", to = "mmol / l", molar_mass = 150000)
#'
convert_conc <- function(
  x,
  from,
  to,
  molar_mass = 0,
  density_solute = 0,
  density_solution = 0) {
  x <- x * calc_factor_conc(
    from = from,
    to = to,
    molar_mass = molar_mass,
    density_solute = density_solute,
    density_solution = density_solution)
  return(x)
}

#'
#' Convert a value of the given concentration into another concentration.
#'
#' @description
#' A convenience wrapper around [calc_factor_prefix()].
#'
#' @inherit calc_factor_prefix details
#'
#' @export
#' @family conversion functions
#' @param x The value to convert.
#' @inheritParams calc_factor_prefix
#' @return The converted value.
#' @examples
#' convert_prefix(x = 2, from = "ng", to = "kg")
#' convert_prefix(x = 2, from = "dm^3", to = "cm^3")
#' convert_prefix(x = 2, from = "fl", to = "pl")
#' convert_prefix(x = 2, from = "pmol", to = "nmol")
#' convert_prefix(x = 2, from = "pM", to = "nM")
#'
convert_prefix <- function(
  x,
  from,
  to) {
  x <- x * calc_factor_prefix(
    from = from,
    to = to)
  return(x)
}

get_conc_type <- function(unit) {
  stopifnot(is.character(unit))
  unit <- enc2utf8(unit)

  #throw_message('get_conc_type - unit: ', unit)

  if (grepl("(([km\u00B5nfp]?g / ([km\u00B5nfp]?l|[dcm]?m.3))|(% w / v))", unit)) {
    # ".g / .l", ".g / .m^3", "% w / v" (= 0.1 g / l)
    type <- "mass_vol"
  } else if (grepl("(([km\u00B5nfp]?mol / ([km\u00B5nfp]?l|[dcm]?m.3))|([km\u00B5nfp]?M))", unit)) {
    # ".M", ".mol / .l",  ".mol / .m^3"
    type <- "molar_vol"
  } else if (grepl("((([km\u00B5nfp]?l|[dcm]?m.3) / ([km\u00B5nfp]?l|[dcm]?m.3))|((% )?v / v))", unit)) {
    # ".l/.l", ".l / m^3", ".m^3/.m^3", ".m^3 / .l", "% v / v", "v / v"
    type <- "vol_vol"
  } else if (grepl("(([km\u00B5nfp]?g / [km\u00B5nfp]?g)|((% )?w / w))", unit)) {
    # ".g / .g", "w / w", "% w / w"
    type <- "mass_mass"
  } else {
    type <- FALSE
  }

  #throw_message('get_conc_type - type: ', type)

  return(type)
}

bring_to_base <- function(unit, type) {
  unit <- enc2utf8(unit)
  factor <- 1

  if (unit == "% w / v") {
    throw_warning("Using '0.1 g / l' instead of '% w / v'")
    factor <- factor * 0.1
    unit <- "g / l"
  } else if (unit == "% v / v") {
    # convert to "v / v" (same as "l / l")
    factor <- factor * 100
    unit <- "l / l"
  } else if (unit == "v / v") {
    # same as "l / l"
    unit <- "l / l"
  } else if (unit == "% w / w") {
    # convert to "w / w" (same as "g / g")
    factor <- factor * 100
    unit <- "g / g"
  } else if (unit == "w / w") {
    # same as "g / g"
    unit <- "g / g"
  } else if (substr(unit, length(unit), length(unit)) == "M") {
    # unit: .M
    unit <- paste0(substr(unit, 0, length(unit)), "mol / l")
  }

  # now unit contains an "/"

  parts <- strsplit(unit, split = "/", fixed = TRUE)[[1]]
  if (length(parts) != 2) {
    throw_error("Cannot process unit: ", unit)
  }
  numerator <- trimws(parts[1])
  denominator <- trimws(parts[2])
  #throw_message(type, " ", numerator, "->", numerator_base[type], ": ", calc_factor_prefix(numerator, numerator_base[type]))
  factor <- factor * calc_factor_prefix(numerator, numerator_base[type])
  #throw_message(type, " ", denominator, "->", denominator_base[type], ": ", calc_factor_prefix(denominator, denominator_base[type]))
  factor <- factor / calc_factor_prefix(denominator, denominator_base[type])

  return(factor)
}

needs_density_solute <- function(from, to) {
  types <- c(from, to)

  if (from == to) {
    needed <- FALSE
  } else if ("mass_vol" %in% types && "molar_vol" %in% types) {
    # mass_vol <-> molar_vol
    needed <- FALSE
  } else if ("mass_vol" %in% types && "vol_vol" %in% types) {
    # mass_vol <-> vol_vol
    needed <- TRUE
  } else if ("mass_vol" %in% types && "mass_mass" %in% types) {
    # mass_vol <-> mass_mass
    needed <- FALSE
  } else if ("molar_vol" %in% types && "vol_vol" %in% types) {
    # molar_vol <-> vol_vol
    needed <- TRUE
  } else if ("molar_vol" %in% types && "mass_mass" %in% types) {
    # molar_vol <-> mass_mass
    needed <- FALSE
  } else if ("mass_mass" %in% types && "vol_vol" %in% types) {
    # mass_mass <-> vol_vol
    needed <- TRUE
  } else {
    needed <- FALSE
  }

  return(needed)
}

needs_density_solution <- function(from, to) {
  types <- c(from, to)

  if (from == to) {
    needed <- FALSE
  } else if ("mass_vol" %in% types && "molar_vol" %in% types) {
    # mass_vol <-> molar_vol
    needed <- FALSE
  } else if ("mass_vol" %in% types && "vol_vol" %in% types) {
    # mass_vol <-> vol_vol
    needed <- FALSE
  } else if ("mass_vol" %in% types && "mass_mass" %in% types) {
    # mass_vol <-> mass_mass
    needed <- TRUE
  } else if ("molar_vol" %in% types && "vol_vol" %in% types) {
    # molar_vol <-> vol_vol
    needed <- FALSE
  } else if ("molar_vol" %in% types && "mass_mass" %in% types) {
    # molar_vol <-> mass_mass
    needed <- TRUE
  } else if ("mass_mass" %in% types && "vol_vol" %in% types) {
    # mass_mass <-> vol_vol
    needed <- TRUE
  } else {
    needed <- FALSE
  }

  return(needed)
}

needs_molar_mass <- function(from, to) {
  types <- c(from, to)

  if (from == to) {
    needed <- FALSE
  } else if ("mass_vol" %in% types && "molar_vol" %in% types) {
    # mass_vol <-> molar_vol
    needed <- TRUE
  } else if ("mass_vol" %in% types && "vol_vol" %in% types) {
    # mass_vol <-> vol_vol
    needed <- FALSE
  } else if ("mass_vol" %in% types && "mass_mass" %in% types) {
    # mass_vol <-> mass_mass
    needed <- FALSE
  } else if ("molar_vol" %in% types && "vol_vol" %in% types) {
    # molar_vol <-> vol_vol
    needed <- TRUE
  } else if ("molar_vol" %in% types && "mass_mass" %in% types) {
    # molar_vol <-> mass_mass
    needed <- TRUE
  } else if ("mass_mass" %in% types && "vol_vol" %in% types) {
    # mass_mass <-> vol_vol
    needed <- FALSE
  } else {
    needed <- FALSE
  }

  return(needed)
}
