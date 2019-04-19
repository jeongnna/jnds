gbm <- function(x, y, params) {
  if (is.null(params$n_iters)) params$n_iters <- 100
  if (is.null(params$depth)) params$depth <- 1
  if (is.null(params$min_child)) params$min_child <- 10
  if (is.null(params$learning_rate)) params$learning_rate <- 0.1
  if (is.null(params$bag_fraction)) params$bag_fraction <- 0.5
  
  if (!is.null(params$seed)) {
    set.seed(params$seed)
  }
  gbm::gbm(
    y ~ ., data = x,
    distribution = params$dist,
    n.trees = params$n_iters,
    interaction.depth = params$depth,
    n.minobsinnode = params$min_child,
    shrinkage = params$learning_rate,
    bag.fraction = params$bag_fraction
  )
}


model_predict.gbm <- function(object, newdata, params) {
  pred <- predict(
    object, newdata,
    n.trees = params$n_iters
  )
  pred[, , 1]
}
