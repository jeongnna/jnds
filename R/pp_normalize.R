normalize_mean_sd_ <- function(cols, data) {
  center <- sapply(data[cols], function(x) mean(x, na.rm = TRUE))
  scale <- sapply(data[cols], function(x) sd(x, na.rm = TRUE))
  list(
    center = center,
    scale = scale
  )
}


normalize_zero_one_ <- function(cols, data) {
  center <- sapply(data[cols], function(x) min(x, na.rm = TRUE))
  scale <- sapply(data[cols], function(x) {max(x, na.rm = TRUE) - min(x, na.rm = TRUE)})
  list(
    center = center,
    scale = scale
  )
}


#' Create a pp module which normalizes numerical features.
#'
#' @param cols A character vector. Specify the names of features to normalize.
#' @param method A character, one of ("mean-sd", "zero-one").
#'   If `"mean-sd"`, subtract by mean and then divide by standard deviation.
#'   If `"zero-one"`, fit range into [0, 1].
#' @param desc (optional) Description of the module.
pp_normalize <- function(cols = NULL, method = c("mean-sd", "zero-one"),
                         desc = NULL, data = NULL, standby = TRUE) {
  if (standby) {
    return (function(data) pp_normalize(cols, method, desc, data, standby = FALSE))
  }
  if (is.null(cols)) {
    num_mask <- sapply(data, is.numeric)
    cols <- names(data)[num_mask]
  }
  if (method == "mean-sd") {
    norm <- normalize_mean_sd_(cols, data)
  } else if (method == "zero-one") {
    norm <- normalize_zero_one_(cols, data)
  } else {
    stop('Error: Unexpected `method`. Try "mean-sd" or "zero-one".')
  }
  pp(
    cols = cols,
    center = norm$center,
    scale = norm$scale,
    desc = desc,
    subclass = "pp_normalize"
  )
}


predict.pp_normalize <- function(object, data) {
  cols <- intersect(object$cols, names(data))
  for (i in seq_along(cols)) {
    col <- cols[i]
    data[[col]] <- (data[[col]] - object$center[i]) / object$scale[i]
  }
  data
}
