exp_mass_si <- c(0, -3, -4, -5, -6, -9, -12, -15, -18)
names(exp_mass_si) <- c("kg", "g", "dg", "cg", "mg", "\u00B5g", "ng", "pg", "fg")

exp_vol <- c(0, -3, -3, -4, -5, -6, -6, -9, -9, -12, -15, -18)
names(exp_vol) <- c("m^3", "l", "dm^3", "dl", "cl", "ml", "cm^3", "\u00B5l", "mm^3", "nl", "pl", "fl")

exp_molar_si <- c(0, -1, -2, -3, -6, -9, -12, -15)
names(exp_molar_si) <- c("mol", "dmol", "cmol", "mmol", "\u00B5mol", "nmol", "pmol", "fmol")

exp_molar <- c(0, -1, -2, -3, -6, -9, -12, -15)
names(exp_molar) <- c("M", "dM", "cM", "mM", "\u00B5M", "nM", "pM", "fM")

numerator_base <- c("g", "mol", "l", "g")
names(numerator_base) <- c("mass_vol", "molar_vol", "vol_vol", "mass_mass")

denominator_base <- c("l", "l", "l", "g")
names(denominator_base) <- c("mass_vol", "molar_vol", "vol_vol", "mass_mass")

recognised_units <- c(
  names(exp_mass_si),
  names(exp_vol),
  names(exp_molar_si),
  names(exp_molar)
)

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
#' Where "." symbolizes a metric prefix (see [calc_factor_prefix()]) :
#'
#' For g, l, mol and M: d (deci), c (centi), m (milli), µ (micro), n (nano),
#' p (pico) and f (femto).
#'
#' For g you might use k (kilo) as well.
#'
#' For m^3 (cubic metres) you may only use: d (deci), c (centi) and m (milli).
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
#' library("dplyr")
#'
#' # generate test data
#' data <- tibble(
#'   sample = c("A", "B", "C"),
#'   conc = c(4.5, 2.3, 5.1),       # concentration in g  / l
#' )
#'
#' fctr_ng_ml <- calc_factor_conc(from = "g/l", to = "ng/ml")
#' # give molar mass in g / mol
#' fctr_mol_l <- calc_factor_conc(from = "g/l", to = "M", molar_mass = 78.971)
#' # give densities in g / l
#' fctr_pc <- calc_factor_conc(from = "g/l", to = "%v/v", density_solute = 4810)
#'
#' data %>%
#'   mutate(
#'     conc_ng_ml = conc * fctr_ng_ml,
#'     conc_mol_l = conc * fctr_mol_l,
#'     conc_pc = conc * fctr_pc
#' )
#'
#' # throws an error
#' \dontrun{
#' # will throw an error because molar_mass is missing
#' fctr_fail <- calc_factor_conc(from = "g/l", to = "mol/l")
#' }
calc_factor_conc <- function(
  from,
  to,
  molar_mass = 0,
  density_solute = 0,
  density_solution = 0) {

  stopifnot(
    is_number(molar_mass),
    molar_mass >= 0,
    is_number(density_solute),
    density_solute >= 0,
    is_number(density_solution),
    density_solution >= 0
  )

  from <- canonicalise_units(from)
  to <- canonicalise_units(to)

  factor <- factor_conc(
      from = from,
      to = to,
      molar_mass = molar_mass,
      density_solute = density_solute,
      density_solution = density_solution)

  return(factor)
}

#'
#' Get a factor to convert metric prefixes.
#'
#' @description
#' Get a factor to convert metric prefixes into one another.
#'
#' @details
#' Convert, e.g. "kg" to "µg". You can convert ".g", ".l", ".mol", ".m^3"
#' (cubic metres), where "." symbolizes a metric prefix:
#'
#' For g, l and mol: d (deci), c (centi), m (milli), µ (micro), n (nano),
#' p (pico) and f (femto).
#'
#' For g you might use k (kilo) as well.
#'
#' For m^3 (cubic metres) you may only use: d (deci), c (centi) and m (milli).
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
#'
calc_factor_prefix <- function(from, to) {
  from <- canonicalise_units(from)
  to <- canonicalise_units(to)

  if (is_concentration(from)) {
    stop(
      "Given unit (", from, ") is a concentration. Please use ",
      "calc_factor_conc() or convert_factor_conc() instead.")
  }

  if (is_concentration(to)) {
    stop(
      "Given unit (", to, ") is a concentration. Please use ",
      "calc_factor_conc() or convert_factor_conc() instead.")
  }

  factor <- factor_prefix(from = from, to = to)

  return(factor)
}

#'
#' Convert a value of the given concentration into another concentration.
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
#' library("dplyr")
#'
#' # generate test data
#' data <- tibble(
#'   sample = c("A", "B", "C"),
#'   conc = c(4.5, 2.3, 5.1),       # concentration in g  / l
#' )
#'
#' data %>%
#'   mutate(
#'     conc_ng_ml = convert_conc(x = conc, from = "g/l", to = "ng/ml"),
#'     # give molar mass in g / mol
#'     conc_mol_l = convert_conc(
#'       x = conc, from = "g/l", to = "M", molar_mass = 78.971),
#'     # give densities in g / l
#'     conc_pc = convert_conc(
#'       x = conc, from = "g/l", to = "%v/v", density_solute = 4810)
#' )
#'
#' # throws an error
#' \dontrun{
#' # will throw an error because molar_mass is missing
#' fail <- convert_conc(x = 5, from = "g/l", to = "mol/l")
#' }
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
#' Convert between metric prefixes.
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
#'
convert_prefix <- function(x, from, to) {
  x <- x * calc_factor_prefix(from = from, to = to)
  return(x)
}

canonicalise_units <- function(unit) {
  unit <- enc2utf8(unit)
  unit_orig <- unit
  unit <- gsub("\\s", "", unit)

  if (unit == "%w/v") {
    warning("Using '0.1 g / l' instead of '% w / v'")
    unit <- "dg/l"
  } else if (unit == "%v/v") {
    # convert to "v / v" (same as "l / l")
    unit <- "cl/l"
  } else if (unit == "v/v") {
    # same as "l / l"
    unit <- "l/l"
  } else if (unit == "%w/w") {
    # convert to "w / w" (same as "g / g")
    unit <- "cg/g"
  } else if (unit == "w/w") {
    # same as "g / g"
    unit <- "g/g"
  } else if (substr(unit, nchar(unit), nchar(unit)) == "M") {
    # unit: .M
    unit <- paste0(substr(unit, 0, nchar(unit) - 1), "mol/l")
  }

  parts <- strsplit(unit, split = "/", fixed = TRUE)[[1]]

  if (FALSE %in% is_recognised_unit(parts)) {
    stop("Not recognised: ", unit, "(",  unit_orig, ")")
  }

  return(unit)
}

factor_conc <- function(
  from,
  to,
  molar_mass,
  density_solute,
  density_solution) {

  from_type <- get_conc_type(from)
  to_type <- get_conc_type(to)

  if (needs_density_solute(from_type, to_type) & density_solute == 0) {
    stop("The density of the solute is required to convert ", from, " to ", to)
  }

  if (needs_density_solution(from_type, to_type) & density_solution == 0) {
    stop(
      "The density of the solution (not the solvent!) is required to convert ",
      from, " to ", to)
  }

  if (needs_molar_mass(from_type, to_type) & molar_mass == 0) {
    stop("The molar mass is required to convert ", from, " to ", to)
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

factor_prefix <- function(from, to) {
  factor <- 1

  if (from %in% names(exp_mass_si) && to %in% names(exp_mass_si)) {
    factor <- 10^(exp_mass_si[from]-exp_mass_si[to])
  } else if (from %in% names(exp_vol) && to %in% names(exp_vol)) {
    factor <- 10^(exp_vol[from]-exp_vol[to])
  } else if (from %in% names(exp_molar_si) && to %in% names(exp_molar_si)) {
    factor <- 10^(exp_molar_si[from]-exp_molar_si[to])
  #} else if (
  #  from %in% names(exp_molar) && to %in% names(exp_molar)) {
  #  factor <- 10^(exp_molar[from]-exp_molar[to])
  } else {
    stop("Could not convert ", from, " to ", to)
  }

  names(factor) <- NULL

  return(factor)
}

is_concentration <- function(x) {
  if (is.character(x) && nchar(x) > 0) {
    return(grepl("/", x, fixed = TRUE))
  }
  return(FALSE)
}

is_recognised_unit <- function(x) {
  if (is.character(x) && nchar(x) > 0) {
    return(x %in% recognised_units)
  }
  return(FALSE)
}

bring_to_base <- function(unit, type) {
  factor <- 1
  # unit should have been filtered through canonicalise_units
  # so should contain "/"
  parts <- strsplit(unit, split = "/", fixed = TRUE)[[1]]
  if (length(parts) != 2) {
    stop("Cannot process unit: ", unit)
  }
  numerator <- parts[1]
  denominator <- parts[2]
  factor <- factor * calc_factor_prefix(numerator, numerator_base[type])
  factor <- factor / calc_factor_prefix(denominator, denominator_base[type])

  return(factor)
}

get_conc_type <- function(unit) {
  # unit should have been filtered through canonicalise_units

  if (grepl("(.?g/(.?l|.?m.3))", unit)) {
    type <- "mass_vol"
  } else if (grepl(".?mol/(.?l|.?m.3)", unit)) {
    type <- "molar_vol"
  } else if (grepl("(.?l|.?m.3)/(.?l|.?m.3)", unit)) {
    type <- "vol_vol"
  } else if (grepl(".?g/.?g", unit)) {
    type <- "mass_mass"
  } else {
    stop("Unrecognised unit: ", unit)
  }

  return(type)
}

needs_density_solute <- function(from, to) {
  types <- c(from, to)

  if (from == to) {
    needed <- FALSE
  } else if ("mass_vol" %in% types & "molar_vol" %in% types) {
    # mass_vol <-> molar_vol
    needed <- FALSE
  } else if ("mass_vol" %in% types & "vol_vol" %in% types) {
    # mass_vol <-> vol_vol
    needed <- TRUE
  } else if ("mass_vol" %in% types & "mass_mass" %in% types) {
    # mass_vol <-> mass_mass
    needed <- FALSE
  } else if ("molar_vol" %in% types & "vol_vol" %in% types) {
    # molar_vol <-> vol_vol
    needed <- TRUE
  } else if ("molar_vol" %in% types & "mass_mass" %in% types) {
    # molar_vol <-> mass_mass
    needed <- FALSE
  } else if ("mass_mass" %in% types & "vol_vol" %in% types) {
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
  } else if ("mass_vol" %in% types & "molar_vol" %in% types) {
    # mass_vol <-> molar_vol
    needed <- FALSE
  } else if ("mass_vol" %in% types & "vol_vol" %in% types) {
    # mass_vol <-> vol_vol
    needed <- FALSE
  } else if ("mass_vol" %in% types & "mass_mass" %in% types) {
    # mass_vol <-> mass_mass
    needed <- TRUE
  } else if ("molar_vol" %in% types & "vol_vol" %in% types) {
    # molar_vol <-> vol_vol
    needed <- FALSE
  } else if ("molar_vol" %in% types & "mass_mass" %in% types) {
    # molar_vol <-> mass_mass
    needed <- TRUE
  } else if ("mass_mass" %in% types & "vol_vol" %in% types) {
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
  } else if ("mass_vol" %in% types & "molar_vol" %in% types) {
    # mass_vol <-> molar_vol
    needed <- TRUE
  } else if ("mass_vol" %in% types & "vol_vol" %in% types) {
    # mass_vol <-> vol_vol
    needed <- FALSE
  } else if ("mass_vol" %in% types & "mass_mass" %in% types) {
    # mass_vol <-> mass_mass
    needed <- FALSE
  } else if ("molar_vol" %in% types & "vol_vol" %in% types) {
    # molar_vol <-> vol_vol
    needed <- TRUE
  } else if ("molar_vol" %in% types & "mass_mass" %in% types) {
    # molar_vol <-> mass_mass
    needed <- TRUE
  } else if ("mass_mass" %in% types & "vol_vol" %in% types) {
    # mass_mass <-> vol_vol
    needed <- FALSE
  } else {
    needed <- FALSE
  }

  return(needed)
}
