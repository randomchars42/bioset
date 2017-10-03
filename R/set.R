#' Read a matrix from a data-sheet and turn it into a multi-column tibble.
#'
#' @description
#' This function is intended to ease working with data coming in matrices, for
#' example from a plate-reader measuring extinction-values or relative light
#' units from bio-assays.
#'
#' This data tends to be presented in matrices in the shape of the measured
#' plate. For a 96-well plate, something like this might be output:
#'
#' @export
#' @param file_name Name of the file from which to read the data. May contain
#'   "#NUM#" as a placeholder if you have multiple files (see num).
#' @param path The path to file (needs to end with "/").
#' @param num Number of the plate to read, inserted for "#NUM#".
#' @param sep Separator used in the csv-file, either "," or ";" (see
#'   \code{\link[utils]{read.csv}})
#' @param cols Number of columns in the input matrix (\code{NULL} means
#'   auto-detect).
#' @param rows Number of rows containing values (not names / additional data)
#'   in the input matrix (\code{NULL} means auto-detect).
#' @param additional_vars Vector of strings containing the names for the
#'   additional columns.
#' @param additional_sep String / RegExp that separates additional vars, e.g.:
#'   \code{"ID_blue_cold"} with \code{additional_sep = "_"} will be separated
#'   into three columns containg \code{"ID"}, \code{"blue"} and \code{"cold"}.
#'   If the separated data would exceed the columns in \code{additional_vars}
#'   the last column will contain a string with separator (e.g.: "blue_cold").
#'   If data is missing \code{NA} is inserted.
#' @return A tibble containing (at minimum) \code{plate}, \code{position} and
#' \code{value}.
#'
set_read <- function(
  file_name = "set_#NUM#.csv",
  path = "",
  num = 1,
  sep = ",",
  cols = 0,
  rows = 0,
  additional_vars = vector(),
  additional_sep = "[^[:alnum:]]+"
) {

  stopifnot(
    is.character(file_name),
    is.character(path),
    is_number(num),
    is.character(sep),
    is_number(cols),
    is_number(rows),
    is.vector(additional_vars),
    is.character(additional_sep)
    )

  # make the pipe operator available to us
  `%>%` <- magrittr::`%>%`

  # load file

  file_name <- gsub(pattern = "#NUM#", replacement = num, x = file_name)
  file_name <- paste0(path, file_name)

  if (!file.exists(file_name)) {
    throw_error("Cannot find the file. Please check path (must end with \"/\") ",
      "and name_scheme (must contain \"#NUM#\")")
  }

  data_raw <- tibble::as_tibble(utils::read.csv(
    file = file_name,
    header = FALSE,
    sep = sep
  ))

  # check dimenions of input

  actual_cols <- length(data_raw)
  actual_rows <- nrow(data_raw)
  actual_vars <- length(additional_vars)

  if (actual_vars == 0) {
    additional_vars <- c("name")
  }

  if (cols == 0) {
    # auto-detect
    cols <- actual_cols
  } else {
    if (actual_cols < cols) {
      throw_error(
        "Column count in sheet (", actual_cols,
        ") lower than expected (", cols,
        "). Reducing cols to actual column count.")
      cols <- actual_cols
    } else if (actual_cols > cols) {
      throw_error(
        "Column count in sheet (", actual_cols,
        ") larger than expected (", cols,
        "). Ignoring residual columns.")
    }
  }

  if (rows == 0) {
    if (actual_vars == 0) {
      # no names given
      rows <- actual_rows
    } else {
      # there must be double the amount of rows than values
      rows <- actual_rows / 2
    }
  } else {
    # if there are names / additional variables double the amount of rows are
    # needed
    required_rows <- rows * ifelse(actual_vars > 0, 2, 1)

    if (actual_rows < required_rows) {
      throw_error(
        "Row count in sheet (", actual_rows,
        ") lower than required (", required_rows,
        ").")
    } else if (actual_rows > required_rows) {
      throw_error(
        "Row count in sheet (", actual_rows,
        ") larger than expected (", required_rows,
        ").")
    }
  }

  # re-organise data

  data_tbl <- data_raw %>%
    dplyr::slice(1:rows)

  if (actual_vars > 0) {
    # get the names if applicable
    names_tbl <- data_raw %>%
      dplyr::slice((rows+1):(rows*2+1))
  }

  data_vec <- c()
  names_vec <- c()
  positions_vec <- c()

  for (i in 1:cols) {
    column <- data_tbl %>%
      dplyr::pull(i) %>%
      as.numeric()
    data_vec <- c(data_vec, column)

    # determine position
    if (rows < length(LETTERS)) {
      column <- paste0(LETTERS[1:rows], i)
    } else {
      column <- paste0(seq(from = 1, to = rows, by = 1), "x", i)
    }
    positions_vec <- c(positions_vec, column)

    if (actual_vars > 0) {
      # use names
      column <- names_tbl %>%
        dplyr::pull(i) %>%
        as.character()
    } else {
      # use position as name
      column <- column
    }

    names_vec <- c(names_vec, column)
  }

  data <-
    tibble::tibble(
      set = num,
      position = positions_vec,
      tmp = names_vec,
      value = data_vec
    ) %>%
    tidyr::separate(
      col = "tmp",
      into = additional_vars,
      sep = additional_sep,
      remove = TRUE,
      extra = "merge",
      fill = "right"
    )

  return(data)
}

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
set_calc_concentrations <- function(
  data,
  cal_names,
  cal_values,
  col_names = name,
  col_values = values,
  col_target = conc,
  col_real = real,
  col_recov = recovery,
  model_func = fit_lnln,
  interp_func = interpolate_lnln
){
  # make some handy operators available
  `%>%` <- magrittr::`%>%`
  `!!` <- rlang::`!!`
  `:=` <- rlang::`:=`

  stopifnot(
    tibble::is.tibble(data),
    is.vector(cal_names),
    is.vector(cal_values),
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
set_calc_variability <- function(data, groups, ...) {
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
