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

package_available <- function(package) {
  # find.package returns a string of length 0 if the package is not installed
  return(length(find.package(package = package, quiet = TRUE)) != 0)
}

get_path <- function(...) {
  return(normalizePath(file.path(...), mustWork = FALSE))
}

check_file <- function(file, report = "none") {

  if (! file.exists(file)) {
    message <- paste0("Cannot find file \"", file,"\".")
    if (report == "stop") {
      stop(message)
    } else if (report == "warning") {
      warning(message)
    } else if (report == "message") {
      message(message)
    }
    return(FALSE)
  }
  return(TRUE)
}

release_questions <- function() {
  c(
    "devtools::revdep_check()?",
    "devtools::build_win()?",
    "rhub::check_with_rdevel()?",
    "Are you feeling good?"
  )
}
