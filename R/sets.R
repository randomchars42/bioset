#'
#' Read sets and calculate concentrations and variability.
#'
#' @description
#' Basicaly a wrapper around [set_read()], [set_calc_concentrations()] and
#' [set_calc_variability()].
#'
#' May write the processed data into two files: `data_samples.csv`,
#' `data_all.csv` and returns a list containing:
#'  * all: all rows including duplicate and calibrators.
#'  * samples: samples only, no calibrators, no duplicates.
#'  * plateNUMBER:
#'    * plot: plot of the calibrators
#'    * model: model used to fit a line to the calibrators
#'
#' @export
#' @family set functions
#' @param sets The number of sets (e.g. `3`` attempts to read `set_1.csv`,
#'   `set_2.csv`, `set_3.csv`), see `file_name`.
#' @param exclude_cals A list of calibrators to exclude, e.g.:
#'   `list(set1 = c("CAL1"))`.
#' @param plot_func Function used to display the fitted line.
#' @param write_data Write the calculated data into `data_all.csv` and
#'   `data_samples.csv`?
#' @param use_written_data Try to read `data_all.csv` and `data_read.csv`
#'   instead of raw data. Useful if you have to re-run the script, but the raw
#'   data does not change.
#' @inheritParams set_read
#' @inheritParams set_calc_concentrations
#' @inheritParams set_calc_variability
#' @return
#' A list:
#'
#'   * `$all`: here you will find all the data , including calibrators,
#'     duplicates, ... (saved in `data_all.csv`)
#'   * `$samples`: only samples here - no calibrators, no duplicates -> most
#'     often you will work with this data  (saved in `data_samples.csv`)
#'   * `$set1`: a list
#'       * `$plot`: a plot showing you the linear function used to calculate the
#'          concentrations for this plate.
#'
#'          The points are the calibrators. They should more or less lie close
#'          to the line.
#'       * `$model`: the model
#'   * (`$set2`): the same information for every plate you have
#'
sets_read <- function(
  sets,
  cal_names,
  cal_values,
  exclude_cals = list(),
  additional_vars = c("name"),
  additional_sep = "_",
  sep = ",",
  dec = ".",
  path = ".",
  file_name = "set_#NUM#.csv",
  model_func = fit_linear,
  plot_func = plot_linear,
  interpolate_func = interpolate_linear,
  write_data = TRUE,
  use_written_data = FALSE
) {
  `%>%` <- magrittr::`%>%`

  stopifnot(
    is.logical(write_data),
    is.logical(use_written_data)
  )

  results <- list()

  if (use_written_data) {
     data_samples <- read_data(
       file = get_path(path, "data_samples.csv"), sep = sep, dec = dec,
       raw = FALSE)
     data_all <- read_data(
       file = get_path(path, "data_all.csv"), sep = sep, dec = dec, raw = FALSE)
  } else {
    data <- sets_process(
      sets = sets,
      cal_names = cal_names,
      cal_values = cal_values,
      exclude_cals = exclude_cals,
      additional_vars = additional_vars,
      additional_sep = additional_sep,
      sep = sep,
      dec = dec,
      path = path,
      file_name = file_name,
      model_func = model_func,
      interpolate_func = interpolate_func
    )

    # make R CMD check happy
    real <- NULL

    data_samples <- data %>%
      dplyr::filter(is.na(real), exclude == FALSE) %>%
      dplyr::mutate(
        plate = set,
        n = value_n,
        raw = value_mean,
        raw_sd = value_sd,
        raw_cv = value_cv,
        concentration = conc_mean,
        concentration_sd = conc_sd,
        concentration_cv = conc_cv
      ) %>%
      dplyr::select(
        -set,
        -real,
        -value,
        -conc,
        -recovery,
        -value_n,
        -value_mean,
        -value_sd,
        -value_cv,
        -conc_n,
        -conc_mean,
        -conc_sd,
        -conc_cv,
        -exclude) %>%
      dplyr::distinct(sample_id, .keep_all = TRUE)

    data_all <- data %>%
      dplyr::mutate(
        set = set,
        n = value_n,
        raw = value,
        raw_mean = value_mean,
        raw_sd = value_sd,
        raw_cv = value_cv,
        concentration = conc_mean,
        concentration_sd = conc_sd,
        concentration_cv = conc_cv
      ) %>%
      dplyr::select(
        -conc,
        -value_n,
        -value_mean,
        -value_sd,
        -value_cv,
        -conc_n,
        -conc_mean,
        -conc_sd,
        -conc_cv,
        -exclude)

    if (write_data) {
      write_data(
        data_samples, file = get_path(path, "data_samples.csv"), sep = sep,
        dec = dec)
      write_data(
        data_all, file = get_path(path, "data_all.csv"), sep = sep, dec = dec)
    }
  }

  results["samples"] <- list(data_samples)
  results["all"] <- list(data_all)

  for (i in 1 : sets) {
    results[[paste0("set", i)]] <-
      get_plot_model(
        data = data_all,
        set_num = i,
        model_func = model_func,
        plot_func = plot_func
      )
  }

  return(results)
}

get_plot_model <- function(data, set_num, model_func, plot_func) {
  `%>%` <- magrittr::`%>%`
  data_plate  <- data %>%
    dplyr::filter(set == set_num)

  if (nrow(data_plate) == 0) {
    return(list(
      model = NA,
      plot = NA
    ))
  }

  return(list(
    model = model_func(data_plate$real, data_plate$value),
    plot = plot_func(data_plate$real, data_plate$value)
  ))
}

sets_process <- function(
  sets,
  cal_names,
  cal_values,
  exclude_cals,
  additional_vars,
  additional_sep,
  sep,
  dec,
  path,
  file_name,
  model_func,
  interpolate_func
) {
  `%>%` <- magrittr::`%>%`

  for (i in 1 : sets) {
    file <- get_path_set(path = path, file_name = file_name, set_number = i)

    if (! check_file(file = file, stop = FALSE)) {
      next()
    }

    data_plate <-
      set_read(
        file_name = file_name,
        path = path,
        num = i,
        sep = sep,
        dec = dec,
        cols = 0,
        rows = 0,
        additional_vars = additional_vars,
        additional_sep = additional_sep
      ) %>%
      dplyr::mutate(
        exclude = FALSE
      )

    exclude_from_set <- exclude_cals[[paste0("set", i)]]

    # make R CMD check happy
    real <- NULL

    if (!is.null(exclude_from_set)) {
      data_plate <- data_plate %>%
        dplyr::mutate(
          exclude = ifelse(
            sample_id %in% exclude_from_set, TRUE, FALSE),
          sample_id = ifelse(
            sample_id %in% exclude_from_set, paste0("x", sample_id), sample_id)
        )
    }

    data_plate <- data_plate %>%
      set_calc_concentrations(
        cal_names = cal_names,
        cal_values = cal_values,
        col_names = sample_id,
        col_values = value,
        col_target = conc,
        col_real = real,
        col_recov = recovery,
        model_func = model_func,
        interpolate_func = interpolate_func
      ) %>%
      bioset::set_calc_variability(sample_id, value, conc)

    if (i == 1) {
      data <- data_plate
    } else {
      data <- rbind(data, data_plate)
    }
  }

  return(data)
}

read_data <- function(file, sep, dec, raw) {
  dec <- get_dec(dec = dec, sep = sep)

  if (raw) {
    data <- utils::read.csv(
      file = file,
      header = FALSE,
      sep = sep,
      dec = dec,
      colClasses = "character"
    )
  } else {
    data <- utils::read.csv(
      file = file,
      header = TRUE,
      sep = sep,
      dec = dec
    )
  }

  return(tibble::as.tibble(data))
}

write_data <- function(data, file, sep, dec) {
  dec <- get_dec(dec = dec, sep = sep)

  utils::write.table(
    data,
    file = file,
    col.names = TRUE,
    row.names = FALSE,
    sep = sep,
    dec = dec
  )
}

# define as "global" to get rid of warnigns in R CMD check
name <- NULL
value <- NULL
conc <- NULL
recovery <- NULL
real <- NULL
conc_cv <- NULL
conc_mean <- NULL
conc_n <- NULL
conc_sd <- NULL
sample_id <- NULL
set <- NULL
exclude <- NULL
value_cv <- NULL
value_mean <- NULL
value_n <- NULL
value_sd <- NULL
n <- function() {}
