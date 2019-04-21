#' Create a pp module which converts columns to numeric.
#'
#' @param cols Names of features to convert.
#' @param desc (optional) Description of the module.
pp_numeric <- function(cols = NULL, desc = NULL, data = NULL, standby = TRUE) {
  if (standby) {
    return (function(data) pp_numeric(cols, desc, data, standby = FALSE))
  }
  pp(
    cols = cols,
    desc = desc,
    subclass = "pp_numeric"
  )
}


predict.pp_numeric <- function(object, data) {
  mutate_at(data, vars(object$cols), as.numeric)
}
