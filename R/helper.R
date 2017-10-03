
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
