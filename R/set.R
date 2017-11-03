#'
#' Read a data set from a data-sheet and turn it into a multi-column tibble.
#'
#' @description
#' Read a matrix of values from a csv sheet and sort them into a tibble. You
#' can name the values and encode several additional properties into the name,
#' which be split into several columns. Please refer to the vignette
#' (`browseVignettes("roxygen2")`) and examples below for in-depth explanation
#' and the whys and hows.
#'
#' @export
#' @family set functions
#' @param file_name Name of the file from which to read the data. May contain
#'   "#NUM#" as a placeholder if you have multiple files.
#' @param path The path to the file (no trailing "/" or "\\" !).
#' @param num Number of the set to read, inserted for "#NUM#".
#' @param sep Separator used in the csv-file, either "," or ";" (see
#'   [utils::read.csv()]).
#' @param dec The character used for decimal points (see [utils::read.csv()]).
#'   "AUTO" will result in "." if `sep` is "," and "," for ";".
#' @param cols Number of columns in the input matrix (`0` means auto-detect).
#' @param rows Number of rows containing values (not names / additional data)
#'   in the input matrix (`0` means auto-detect).
#' @param additional_vars Vector of strings containing the names for the
#'   additional columns.
#' @param additional_sep String / RegExp that separates additional vars, e.g.:
#'   `"ID_blue_cold"` with `additional_sep = "_"` will be separated
#'   into three columns containing `"ID"`, `"blue"` and `"cold"`.
#'   If the separated data would exceed the columns in `additional_vars`
#'   the last column will contain a string with separator (e.g.: `"blue_cold"`).
#'   If data is missing `NA` is inserted.
#' @return A tibble containing (at minimum) `set`, `position`, `sample_id`,
#'   `name` and `value`.
#' @examples
#' # a file containing only values
#' read.csv(
#'   file = system.file("extdata", "values.csv", package = "bioset"),
#'   header = FALSE,
#'   colClasses = "character"
#' )
#'
#' # read into a tibble
#' set_read(
#'   file_name = "values_names.csv",
#'   path = system.file("extdata", package = "bioset"),
#' )
#'
#' # file containing names
#' read.csv(
#'   file = system.file("extdata", "values_names.csv", package = "bioset"),
#'   header = FALSE,
#'   colClasses = "character"
#' )
#'
#' # read a file containing labels and store those in column "name"
#' set_read(
#'   file_name = "values_names.csv",
#'   path = system.file("extdata", package = "bioset"),
#'   additional_vars = c("name")
#' )
#'
#' # file with names and properties
#' read.csv(
#'   file = system.file(
#'     "extdata", "values_names_properties.csv", package = "bioset"),
#'   header = FALSE,
#'   colClasses = "character"
#' )
#'
#' # read a file containing labels and properties and store those in columns
#' # "name" and "time"
#' # splits names by every character that's not A-Z, a-z, 0-9
#' # to change that behaviour use additional_sep
#' set_read(
#'   file_name = "values_names_properties.csv",
#'   path = system.file("extdata", package = "bioset"),
#'   additional_vars = c("name", "time")
#' )
#'
#' # read file "set_1.csv" containing labels
#' set_read(
#'   num = 1,
#'   path = system.file("extdata", package = "bioset"),
#'   additional_vars = c("name", "time")
#' )
#'
#' # read file "set_2.csv" containing labels
#' set_read(
#'   num = 2,
#'   path = system.file("extdata", package = "bioset"),
#'   additional_vars = c("name", "time")
#' )
#'
#' # read file "plate_2.csv" containing labels
#' set_read(
#'   num = 2,
#'   file_name = "plate_#NUM#.csv",
#'   path = system.file("extdata", package = "bioset"),
#'   additional_vars = c("name", "time")
#' )
#'
set_read <- function(
  file_name = "set_#NUM#.csv",
  path = ".",
  num = 1,
  sep = ",",
  dec = ".",
  cols = 0,
  rows = 0,
  additional_vars = vector(),
  additional_sep = "[^[:alnum:]]+"
) {

  stopifnot(
    is.character(file_name),
    is.character(path),
    is_number(num),
    is_number(cols),
    is_number(rows),
    is.vector(additional_vars),
    is.character(additional_sep)
  )

  dec <- get_dec(dec = dec, sep = sep)

  # make the pipe operator available to us
  `%>%` <- magrittr::`%>%`

  # load file
  file <- get_path_set(path = path, file_name = file_name, set_number = num)
  check_file(file = file, report = "stop")
  data_raw <- read_data(file = file, sep = sep, dec = dec, raw = TRUE)

  # check dimenions of input

  actual_cols <- length(data_raw)
  actual_rows <- nrow(data_raw)
  actual_vars <- length(additional_vars)

  if (actual_vars == 0) {
    # no names are given, but reuqired for tidyr::separate to work
    additional_vars <- c("name")
  }

  if (cols == 0) {
    # auto-detect
    cols <- actual_cols
  } else {
    if (actual_cols < cols) {
      stop(
        "Column count in sheet (", actual_cols,
        ") lower than expected (", cols,
        "). Reducing cols to actual column count.")
      cols <- actual_cols
    } else if (actual_cols > cols) {
      stop(
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
      stop(
        "Row count in sheet (", actual_rows,
        ") lower than required (", required_rows,
        ").")
    } else if (actual_rows > required_rows) {
      stop(
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
      sample_id = names_vec,
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
#' Calculate concentrations for the set using contained calibrators.
#'
#' @description
#' If the data set is generated, for example by reading extinction rates or
#' relative light units from a plate, these raw values can be converted to
#' concentrations using data fields with known concentrations (calibrators).
#'
#' @details
#' If the data set contains samples with known concentrations (calibrators)
#' those can be used to interpolate the concentrations of the other samples.
#'
#' @export
#' @family set functions
#' @param data A tibble containing the data.
#' @param cal_names A vector of strings containing the names of the samples used
#'   as calibrators.
#' @param cal_values A numeric vector with the known concentrations of those
#'   samples (must be in the same order).
#' @param col_names The name of the column where the `cal_names` can be
#'   found.
#' @param col_values The name of the column holding the raw values.
#' @param col_target The name of the column to created for the calculated
#'   concentration.
#' @param col_real The name of the column to create for the known
#'   concentrations.
#' @param col_recov The name of the column to create for the recovery of the
#'   calibrators.
#' @param model_func A function generating a model to fit the calibrators,
#'   e.g. [fit_linear()], [fit_lnln()].
#' @param interpolate_func A function used to interpolate the concentrations of
#'   the other samples, based on the model, e.g.
#'   [interpolate_linear()], [interpolate_lnln()].
#' @return A tibble containing all original and additional columns.
#' @examples
#' # generate data
#' library("tibble")
#'
#' data <- tibble(
#'   name = c("CAL1", "CAL2", "CAL3", "A", "B", "C"),
#'   value = c(1, 5, 10, 2, 4, 6)
#' )
#'
#' data
#'
#' # the known concentration of the calibrators
#' cals <- c(1, 5, 10)
#' names(cals) <- c("CAL1", "CAL2", "CAL3")
#'
#' set_calc_concentrations(
#'   data = data,
#'   cal_names = names(cals),
#'   cal_values = cals
#' )
#'
#' # to set column names use notation like in dplyr / tidyverse
#' # set the name of the column holding the final concentration to "my_protein"
#' set_calc_concentrations(
#'   data = data,
#'   cal_names = names(cals),
#'   cal_values = cals,
#'   col_target = my_protein
#' )
#'
#' \dontrun{
#' # notice that col_target is given a string
#' # this will fail
#' set_calc_concentrations(
#'   data = data,
#'   cal_names = names(cals),
#'   cal_values =  cals,
#'   col_target = "my_protein"
#' )
#' }
#'
#' # simulate data which has to be transformed to get a good fit
#' cals <- exp(cals)
#' data$value <- exp(data$value)
#'
#' # use ln-transformation on values and known concentrations prior to
#' # fitting a model
#'
#' data <- set_calc_concentrations(
#'   data = data,
#'   cal_names = names(cals),
#'   cal_values = cals,
#'   model_func = fit_lnln,
#'   interpolate_func = interpolate_lnln
#' )
#'
#' data
#'
#' # inspect goodnes of fit
#' plot_lnln(data$real, data$value)
#'
#' rm(cals, data)
set_calc_concentrations <- function(
  data,
  cal_names,
  cal_values,
  col_names = name,
  col_values = value,
  col_target = conc,
  col_real = real,
  col_recov = recovery,
  model_func = fit_linear,
  interpolate_func = interpolate_linear
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
    is.function(interpolate_func)
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
      !! col_target_name :=
        interpolate_func(y = !! col_values, model = !! model),
      !! col_recov_name := (!! col_target) / (!! col_real)
    )

  return(data)
}


#'
#' Calculate parameters of variability for a given set of values.
#'
#' @description
#' Calculate mean, standard deviation and coefficient of variation for groups of
#' values.
#'
#' @details
#' Dealing with measured values, the measurement of sample "A" is often done in
#' duplicates / triplicates / ... . This function groups all samples with the
#' same name and calculates mean, standard deviation and coefficient of
#' variation (= sd / mean).
#'
#' @export
#' @family set functions
#' @param data A tibble containing the data.
#' @param ids The column holding the names used to group the values.
#' @param ... The name(s) of the columns used to calculate the variability.
#' @return A tibble containing all original and additional columns
#'   (NAMEA_mean, NAMEA_n, NAMEA_sd, NAMEA_cv, (NAMEB_mean, ...)).
#' @examples
#' # generate data
#' library("tibble")
#'
#' data <- tibble(
#'   names = c("A", "B", "C", "A", "B", "C"),
#'   value = c(19, 59, 22, 18, 63, 28),
#'   conc = c(1.9, 5.9, 2.2, 1.8, 6.3, 2.8)
#' )
#'
#' data
#'
#' set_calc_variability(
#'   data = data,
#'   ids = names,
#'   value,
#'   conc
#' )
#'
#' # to set column names use notation like in dplyr / tidyverse
#' \dontrun{
#' # notice how strings are given as column names
#' set_calc_variability(
#'   data = data,
#'   ids = "names",
#'   "value",
#'   "conc"
#' )
#' }
#'
#' rm(cals)
#'
set_calc_variability <- function(data, ids, ...) {
  # make some handy operators available
  `%>%` <- magrittr::`%>%`
  `!!` <- rlang::`!!`
  `:=` <- rlang::`:=`

  stopifnot(tibble::is.tibble(data))

  ids <- rlang::enquo(ids)
  ids_name <- rlang::quo_name(ids)
  calc_for <- rlang::quos(...)

  for (i in seq_along(calc_for)) {
    target <- calc_for[[i]]
    target_base <- rlang::quo_name(target)

    if (target_base == ids_name) {
      next()
    }

    target_mean <- paste0(target_base, "_mean")
    target_n <- paste0(target_base, "_n")
    target_sd <- paste0(target_base, "_sd")
    target_cv <- paste0(target_base, "_cv")

    data <- data %>%
      dplyr::group_by(!! ids) %>%
      dplyr::mutate(
        !! target_n := n(),
        !! target_mean := mean(!! target),
        !! target_sd := stats::sd(!! target),
        !! target_cv := stats::sd(!! target) / mean(!! target)
      ) %>%
      dplyr::ungroup()
  }

  return(data)
}

get_path_set <- function(path, file_name, set_number) {
  file_name <- gsub(pattern = "#NUM#", replacement = set_number, x = file_name)
  file <- get_path(path, file_name)
  return(file)
}

get_dec <- function(dec, sep) {
  stopifnot(
    is.character(sep),
    sep %in% c(";", ","),
    is.character(dec),
    dec %in% c(",", ".", "AUTO")
  )

  if (dec %in% c(",", ".")) {
    return(dec)
  } else if (sep == ",") {
    return(".")
  } else if (sep == ";") {
    return(",")
  }
}
