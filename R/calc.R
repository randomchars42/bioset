exp_mass_si <- c(0, -1, -2, -3, -4, -5, -6)
names(exp_mass_si) <- c("kg", "g", "mg", "µg", "ng", "pg", "fg")

exp_vol <- c(0, -1, -1, -2, -2, -3, -3, -4, -5, -6)
names(exp_vol) <- c("m^3", "l", "dm^3", "ml", "cm^3", "µl", "mm^3", "nl", "pl", "fl")

exp_molar_si <- c(0, -1, -2, -3, -4, -5)
names(exp_molar_si) <- c("mol", "mmol", "µmol", "nmol", "pmol", "fmol")

exp_molar_metric <- c(0, -1, -2, -3, -4, -5)
names(exp_molar_metric) <- c("M", "mM", "µM", "nM", "pM", "fM")

numerator_base <- c("g", "mol", "l", "g")
names(numerator_base) <- c("mass_vol", "molar_vol", "vol_vol", "mass_mass")

denominator_base <- c("l", "l", "l", "g")
names(denominator_base) <- c("mass_vol", "molar_vol", "vol_vol", "mass_mass")

# molar mass in g/mol!
# accepts and converts to ".g/.l", ".g/.m^3", ".M", "% w/v", "% v/v", "% w/w", ".l/.l". ".m^3/.m^3"

#'
#' @export
#'
calc_factor_conc <- function(
  from,
  to,
  molar_mass = 0,
  density_solute = 0,
  density_solution = 0) {

  from_type <- get_conc_type(from)
  to_type <- get_conc_type(to)

  stopifnot(
    is_number(molar_mass),
    is_number(density_solute),
    is_number(to_density)
  )

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
#' @export
#'
calc_factor_unit <- function(from, to) {

  factor <- 1

  if (from %in% names(exp_mass_si)) {
    factor <- 1000^(exp_mass_si[from]-exp_mass_si[from])
  } else if (from %in% names(exp_vol)) {
    factor <- 1000^(exp_vol[from]-exp_vol[to])
  } else if (from %in% namesexp_(molar_si)) {
    factor <- 1000^(exp_molar_si[from]-exp_molar_si[to])
  } else if (from %in% names(exp_molar_metric)) {
    factor <- 1000^(exp_molar_metric[from]-exp_molar_metric[to])
  } else {
    stop(paste0("Could not convert ", from, " to ", to))
  }

  names(factor) <- NULL

  return(factor)
}

#'
#' @export
#'
convert <- function(
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

get_conc_type <- function(unit) {
  stopifnot(is.character(unit))
  unit <- enc2utf8(unit)

  if (grepl("(([kmµnfp]?g / [kmµnfp]?(l|m.3))|(% w / v))", unit)) {
    # ".g / .l", ".g / .m^3", "% w / v" (= 0.1 g / l)
    type <- "mass_vol"
  } else if (grepl("(([kmµnfp]?mol / [kmµnfp]?(l|m.3))|([kmµnfp]?M))", unit)) {
    # ".M", ".mol / .l",  ".mol / .m^3"
    type <- "molar_vol"
  } else if (grepl("(([kmµnfp]?(l|m.3) / [kmµnfp]?(l|m.3))|((% )?v / v))", unit)) {
    # ".l/.l". ".m^3/.m^3", "% v / v", "v / v"
    type <- "vol_vol"
  } else if (grepl("(([kmµnfp]?g / [kmµnfp]?g)|((% )?w / w))", unit)) {
    # ".g / .g", "w / w", "% w / w"
    type <- "mass_mass"
  } else {
    type <- FALSE
  }

  return(type)
}

bring_to_base <- function(unit, type) {
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
  factor <- factor * calc_factor_unit(numerator, numerator_base[type])
  factor <- factor / calc_factor_unit(denominator, denominator_base[type])

  factor <- factor * bring_to_base_frac(unit = unit, type = type)
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
