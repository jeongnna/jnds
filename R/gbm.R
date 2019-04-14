gbm <- function(x, y, params) {
  require(gbm)
  
  if (is.null(params$n_trees)) {
    params$n_trees <- 100
  }
  if (is.null(params$depth)) {
    params$depth <- 1
  }
  if (is.null(params$node_size)) {
    params$node_size <- 10
  }
  if (is.null(params$learning_rate)) {
    params$learning_rate <- 0.1
  }
  if (is.null(params$bag_fraction)) {
    params$bag_fraction <- 0.5
  }
  
  set.seed(params$seed)
  gbm::gbm(
    y ~ ., data = x,
    distribution = params$dist,
    n.trees = params$n_trees,
    interaction.depth = params$depth,
    n.minobsinnode = params$node_size,
    shrinkage = params$learning_rate,
    bag.fraction = params$bag_fraction
  )
}


model_predict.gbm <- function(object, newdata, params) {
  pred <- predict(
    object, newdata,
    n.trees = params$n_trees
  )
  pred[, , 1]
}
