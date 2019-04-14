#' Create a pp module which converts numerical features to principal components.
#'
#' @param cols A character vector. Specify the names of features to remove.
#' @param desc (optional) Description of the module.
pp_remove <- function(cols, desc = NULL, data = NULL, standby = TRUE) {
  if (standby) {
    return (function(data) pp_remove(cols, desc, data, standby = FALSE))
  }
  pp(
    cols = cols,
    desc = desc,
    subclass = "pp_remove"
  )
}


predict.pp_remove <- function(object, data) {
  cols <- object$cols
  select(data, -cols)
}
