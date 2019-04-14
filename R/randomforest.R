randomforest <- function(x, y, params) {
  require(randomForest)
  
  set.seed(params$seed)
  randomForest::randomForest(
    y ~ ., data = x,
    ntree = params$n_trees
  )
}


model_predict.randomForest <- function(object, newdata, params) {
  if (is.null(params$predict_type)) {
    if (object$type == "regression") {
      params$predict_type <- "response"
    } else if (object$type == "classification") {
      params$predict_type <- "prob"
    }
  }
  
  predict(
    object, newdata,
    type = params$predict_type
  )
}