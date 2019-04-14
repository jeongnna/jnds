#' Create a pp module which converts numerical features to principal components.
#'
#' @param cols A character vector. Specify the names of features to convert.
#' @param threshold A numeric value in [0, 1).
#' @param desc (optional) Description of the module.
pp_pca <- function(cols, threshold = .8,
                   desc = NULL, data = NULL, standby = TRUE) {
  if (standby) {
    return (function(data) pp_pca(cols, threshold, desc, data, standby = FALSE))
  }
  prcomp_fitted <- 
    data %>%
      select(cols) %>%
      na.omit() %>%
      prcomp()
  pve <- prcomp_fitted$sdev^2 %>% (function(x) {x / sum(x)})
  n_pc <- first(which(cumsum(pve) > threshold))
  loading <- prcomp_fitted$rotation[, 1:n_pc]
  pp(
    cols = cols,
    loading = loading,
    desc = desc,
    subclass = "pp_pca"
  )
}


predict.pp_pca <- function(object, data) {
  cols <- object$cols
  x_origin <-
    data %>% 
    select(cols) %>%
    as.matrix()
  x_pc <- as_tibble(x_origin %*% object$loading)
  data %>%
    select(-cols) %>%
    bind_cols(x_pc)
}
