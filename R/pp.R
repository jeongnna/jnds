#' Create a pp module.
#'
#' @param ... Any object the module should contain.
#' @param subclass A character value. If `NULL`, the module will have class 'pp'.
#'   If not `NULL`, the module will have subclass of 'pp'.
pp <- function(..., subclass = NULL) {
  object <- list(...)
  class(object) <- c(subclass, "pp")
  object
}


is_pp <- function(x) {
  inherits(x, "pp")
}


print.pp <- function(object) {
  cat(object$desc, "\n")
}


predict.pp <- function(object, data) {
  data
}
