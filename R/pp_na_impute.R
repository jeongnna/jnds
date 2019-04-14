reduce_mean_ <- function(data) {
  sapply(data, function(x) mean(x, na.rm = TRUE))
}


reduce_median_ <- function(data) {
  sapply(data, function(x) median(x, na.rm = TRUE))
}


reduce_frequent_ <- function(data) {
  frequent <- function(x) {
    fq <- first(names(sort(table(x), decreasing = TRUE)))
    if (is.numeric(x)) {
      fq <- as.numeric(fq)
    }
    fq
  }
  sapply(data, function(x) frequent(x))
}


#' Create a pp module which imputes missing data.
#'
#' @param num_cols A character vector. Specify the names of numerical features.
#' @param cat_cols A character vector. Specify the names of categorical features.
#' @param num_method A character. Method for imputing numerical features.
#'   Choose one of ("mean", "median", "frequent").
#' @param cat_method A character. Method for imputing categorical features.
#'   Only "frequent" is available.
#' @param desc (optional) Description of the module.
pp_na_impute <- function(num_cols, cat_cols,
                         num_method = "mean", cat_method = "frequent",
                         desc = NULL, data = NULL, standby = TRUE) {
  if (standby) {
    return (function(data) pp_na_impute(num_cols, cat_cols, num_method, cat_method, desc, data, standby = FALSE))
  }
  reduce_funs <- list(
    mean = reduce_mean_,
    median = reduce_median_,
    frequent = reduce_frequent_
  )
  num_fun <- reduce_funs[[num_method]]
  cat_fun <- reduce_funs[[cat_method]]
  num_replace <- num_fun(data[num_cols])
  names(num_replace) <- num_cols
  cat_replace <- cat_fun(data[cat_cols])
  names(cat_replace) <- cat_cols
  pp(
    num_replace = num_replace,
    cat_replace = cat_replace,
    desc = desc,
    subclass = "pp_na_impute"
  )
}


predict.pp_na_impute <- function(object, data) {
  num_replace <- object$num_replace
  cat_replace <- object$cat_replace
  for (i in seq_along(num_replace)) {
    col <- names(num_replace[i])
    na_mask <- is.na(data[[col]])
    data[[col]][na_mask] <- num_replace[i]
  }
  for (i in seq_along(cat_replace)) {
    col <- names(cat_replace[i])
    na_mask <- is.na(data[[col]])
    data[[col]][na_mask] <- cat_replace[i]
  }
  data
}
