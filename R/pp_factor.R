#' Create a pp module which converts columns to factor.
#'
#' @param cols A character vector. Specify the names of features to convert.
#' @param desc (optional) Description of the module.
pp_factor <- function(cols = NULL, desc = NULL, data = NULL, standby = TRUE) {
  if (standby) {
    return (function(data) pp_factor(cols, desc, data, standby = FALSE))
  }
  if (is.null(cols)) {
    char_mask <- sapply(data, is.character)
    cols <- names(data)[char_mask]
  }
  cat_levels <- lapply(data[cols], function(x) levels(factor(x)))
  names(cat_levels) <- cols
  pp(
    cat_levels = cat_levels,
    desc = desc,
    subclass = "pp_factor"
  )
}


predict.pp_factor <- function(object, data) {
  cat_levels <- object$cat_levels
  for (i in seq_along(cat_levels)) {
    col <- names(cat_levels[i])
    lv <- cat_levels[[i]]
    data[[col]] <- factor(data[[col]], levels = lv)
  }
  data
}
