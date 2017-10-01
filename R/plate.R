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
#' \tabular{rrrrr}{
#'   ValA1 \tab ValA2 \tab ValA3 \tab ... \tab ValA12\cr
#'   ValB1 \tab ValB2 \tab ValB3 \tab ... \tab ValB12\cr
#'   ... \tab ... \tab ... \tab ... \tab ...\cr
#'   ValH1 \tab ValH2 \tab ValH3 \tab ... \tab ValH12
#' }
#'
#' Save these values as "plate_1.csv".
#'
#' You may add names by formatting your sheet like:
#'
#' \tabular{rrrrr}{
#'   ValA1 \tab ValA2 \tab ValA3 \tab ... \tab ValA12\cr
#'   ValB1 \tab ValB2 \tab ValB3 \tab ... \tab ValB12\cr
#'   ... \tab ... \tab ... \tab ... \tab ...\cr
#'   ValH1 \tab ValH2 \tab ValH3 \tab ... \tab ValH12\cr
#'   NameA1 \tab NameA2 \tab NameA3 \tab ... \tab NameA12\cr
#'   NameB1 \tab NameB2 \tab NameB3 \tab ... \tab NameB12\cr
#'   ... \tab ... \tab ... \tab ... \tab ...\cr
#' }
#'
#' (You get it ;) )
#'
#' \code{plate_read()} will turn it into a 3 x 96 tibble:
#'
#' \tabular{rrr}{
#'   plate \tab name \tab value\cr
#'   1 \tab NameA1 \tab ValA1\cr
#'   1 \tab NameA2 \tab ValA2\cr
#'   ... \tab ... \tab ...\cr
#'   1 \tab NameH12 \tab ValH12\cr
#' }
#'
#' To add arbitrary values you can format the data like:
#'
#' \tabular{rrrrr}{
#'   ValA1 \tab ValA2 \tab ValA3 \tab ... \tab ValA12\cr
#'   ValB1 \tab ValB2 \tab ValB3 \tab ... \tab ValB12\cr
#'   ... \tab ... \tab ... \tab ... \tab ...\cr
#'   ValH1 \tab ValH2 \tab ValH3 \tab ... \tab ValH12\cr
#'   NameA1_PropA1 \tab NameA2_PropA2 \tab NameA3_PropA3 \tab ... \tab
#'   NameA12_PropA12\cr
#'   NameB1_PropB1 \tab NameB2_PropB2 \tab NameB3_PropB3 \tab ... \tab
#'   NameB12_PropB12\cr
#'   ... \tab ... \tab ... \tab ... \tab ...\cr
#' }
#'
#' \code{plate_read()} will turn it into:
#'
#' \tabular{rrrr}{
#'   plate \tab name \tab value \tab property\cr
#'   1 \tab NameA1 \tab ValA1 \tab PropA1\cr
#'   1 \tab NameA2 \tab ValA2 \tab PropA2\cr
#'   ... \tab ... \tab ... \tab ...\cr
#'   1 \tab NameH12 \tab ValH12 \tab PropH12\cr
#' }
#'
#' @export
#' @param file_name Name of the file from which to read the data. May contain
#'   "#NUM#" as a placeholder if you have multiple files (see plate_num).
#' @param path The path to file (needs to end with "/").
#' @param plate_num Number of the plate to read, inserted for "#NUM#".
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
plate_read <- function(
  file_name = "plate_#NUM#.csv",
  path = "./",
  plate_num = 1,
  sep = ",",
  cols = 12,
  rows = 8,
  additional_vars = vector(),
  additional_sep = "[^[:alnum:]]+"
) {

  stopifnot(
    is.character(file_name),
    is.character(path),
    is.numeric(as.numeric(plate_num)),
    is.character(sep),
    is.numeric(as.numeric(cols)),
    is.numeric(as.numeric(rows)),
    is.vector(additional_vars),
    is.character(additional_sep)
    )

  # make the pipe operator available to us
  `%>%` <- magrittr::`%>%`

  # load file

  file_name <- gsub(pattern = "#NUM#", replacement = plate_num, x = file_name)
  file_name <- paste0(path, file_name)

  if (!file.exists(file_name)) {
    stop(paste0("Cannot find the file. Please check path (must end with \"/\") ",
      "and name_scheme (must contain \"#NUM#\")"))
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

  if (is.null(cols)) {
    # auto-detect
    cols <- actual_cols
  } else {
    if (actual_cols < cols) {
      stop(paste0(
        "Column count in sheet (", actual_cols,
        ") lower than expected (", cols,
        "). Reducing cols to actual column count."))
      cols <- actual_cols
    } else if (actual_cols > cols) {
      stop(paste0(
        "Column count in sheet (", actual_cols,
        ") larger than expected (", cols,
        "). Ignoring residual columns."))
    }
  }

  if (is.null(rows)) {
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
      stop(paste0(
        "Row count in sheet (", actual_rows,
        ") lower than required (", required_rows,
        ")."))
    } else if (actual_rows > required_rows) {
      stop(paste0(
        "Row count in sheet (", actual_rows,
        ") larger than expected (", required_rows,
        ")."))
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
      plate = plate_num,
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
