lasso <- function(x, y, params = list(), x_val = NULL, y_val = NULL) {
  if (is.null(params$alpha)) params$alpha <- 1
  
  x_mat <- model.matrix(y ~ ., data = x)
  glmnet::glmnet(
    x_mat, y,
    family = params$dist,
    alpha = 1,
    lambda = params$lambda
  )
}


model_predict.glmnet <- function(object, newdata, params = list()) {
  newdata_mat <- model.matrix(~ ., data = newdata)
  predict(
    object, newdata_mat,
    type = "response"
  )
}


cv_lasso <- function(x, y, params = list(), x_val = NULL, y_val = NULL) {
  if (is.null(params$alpha)) params$alpha <- 1
  
  x_mat <- model.matrix(y ~ ., data = x)
  glmnet::cv.glmnet(
    x_mat, y,
    type.measure = params$type_measure,
    alpha = 1
  )
}


model_predict.cv.glmnet <- function(object, newdata, params = list()) {
  if (is.null(params$lambda)) params$lambda <- object$lambda.min
  
  newdata_mat <- model.matrix(~ ., data = newdata)
  predict(
    object, newdata_mat,
    s = params$lambda,
    type = "response"
  )
}
