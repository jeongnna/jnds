randomforest <- function(x, y, params = list(), x_val = NULL, y_val = NULL) {
  if (is.null(params$n_iters)) params$n_iters <- 200
  
  if (!is.null(params$seed)) {
    set.seed(params$seed)
  }
  randomForest::randomForest(
    y ~ ., data = x,
    ntree = params$n_iters
  )
}


model_predict.randomForest <- function(object, newdata, params = list()) {
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
