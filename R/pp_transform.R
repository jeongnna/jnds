#' Create a pp module which transform features.
#'
#' @param cols A character vector. Specify the names of features to convert.
#' @param fun Transformation function such as `log`, `sqrt`, etc.
#' @param desc (optional) Description of the module.
pp_transform <- function(cols, fun, desc = NULL, data = NULL, standby = TRUE) {
  if (standby) {
    return (function(data) pp_transform(cols, fun, desc, data, standby = FALSE))
  }
  pp(
    cols = cols,
    fun = fun,
    desc = desc,
    subclass = "pp_transform"
  )
}


predict.pp_transform <- function(object, data) {
  for (col in object$cols) {
    data[[col]] <- object$fun(data[[col]])
  }
  data
}
