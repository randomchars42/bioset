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
#' @param
#'
#' @param
#'
#' @return
#'
#' @export
#'
plate_read <- function(
  plate_num,
  path = "./",
  additional_vars = c(),
  additional_sep = "[^[:alnum:]]+",
  name_scheme = "plate_#NUM#.csv",
  sep = ",",
  cols = 12,
  rows = 8
) {
  # make the pipe operator available to us
  `%>%` <- magrittr::`%>%`

  file_name <- gsub(pattern = "#NUM#", replacement = plate_num, x = name_scheme)
  file_name <- paste0(path, file_name)

  if (!file.exists(file_name)) {
    stop(paste0("Cannot find the file. Please check path (must end with \"/\") ",
      "and name_scheme (must contain \"#NUM#\")"))
  }

  data_raw <- tibble::as_tibble(read.csv(
    file = file_name,
    header = FALSE,
    sep = sep
  ))

  data_tbl <- data_raw %>%
    dplyr::slice(1:rows)

  names_tbl <- data_raw %>%
    dplyr::slice((rows+1):(rows*2+1))

  #return(names_tbl)

  data_vec <- c()
  names_vec <- c()

  for (i in 1:cols) {
    column <- data_tbl %>%
      dplyr::pull(i) %>%
      as.numeric(.)
    data_vec <- c(data_vec, column)

    column <- names_tbl %>%
      dplyr::pull(i) %>%
      as.character(.)
    names_vec <- c(names_vec, column)
  }

  #return(names_vec)

  data <-
    tibble::tibble(
      plate = plate_num,
      tmp = names_vec,
      value = data_vec
    ) %>%
    tidyr::separate(
      col = tmp,
      into = c("name", additional_vars),
      sep = additional_sep,
      remove = TRUE,
      extra = "merge",
      fill = "right"
    )

  return(data)
}
