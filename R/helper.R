
is_number <- function(x) {
  if(! is.null(x) && ! is.na(x) && (is.numeric(x) || is.integer(x))) {
    return(TRUE)
  }
  return(FALSE)
}

args_to_text <- function(...) {
  parts <- list(...)
  return(paste0(parts, collapse = ''))
}

throw_error <- function(...) {
  stop(args_to_text(...))
}

throw_warning <- function(...) {
  warning(args_to_text(...))
}

throw_message <- function(...) {
  message(args_to_text(...))
}

package_available <- function(package) {
  # find.package returns a string of length 0 if the package is not installed
  return(length(find.package(package = package, quiet = TRUE)) != 0)
}

get_path <- function(...) {
  return(normalizePath(file.path(...), mustWork = FALSE))
}

check_file <- function(file, stop = FALSE) {
  if (! file.exists(file)) {
    message <- paste0("Cannot find file \"", file,"\".")
    if (stop) {
      stop(message)
    } else {
      warning(message)
    }
    return(FALSE)
  }
  return(TRUE)
}

release_questions <- function() {
  c(
    "devtools::revdep_check()?",
    "devtools::build_win()?",
    "Are you feeling good?"
  )
}
