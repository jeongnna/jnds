xgboost <- function(x, y, params, x_test = NULL, y_test = NULL) {
  if (is.null(params$objective)) {
    if (is.numeric(y)) {
      params$objective <- "reg:squarederror"
    } else if (length(levels(y)) == 2) {
      params$objective <- "binary:logistic"
    } else {
      params$objective <- "multi:softprob"
    }
  }
  if (is.null(params$depth)) params$depth <- 6
  if (is.null(params$min_child)) params$min_child <- 1
  if (is.null(params$min_split_loss)) params$min_split_loss <- 0
  if (is.null(params$learning_rate)) params$learning_rate <- .3
  if (is.null(params$max_delta_step)) params$max_delta_step <- 0
  if (is.null(params$n_iters)) params$n_iters <- 1
  if (is.null(params$row_fraction)) params$row_fraction <- 1
  if (is.null(params$col_fraction)) params$col_fraction <- 1
  if (is.null(params$n_cores)) params$n_cores <- 1
  
  if (!is.null(params$seed)) {
    set.seed(params$seed)
  }
  x_mat <- model.matrix(y ~ . - 1, data = x)
  dtrain <- xgb.DMatrix(data = x_mat, label = y)
  watchlist <- list()
  if (!is.null(x_test)) {
    x_mat_test <- model.matrix(y_test ~ . - 1, data = x_test)
    dtest <- xgb.DMatrix(data = x_mat_test, label = y_test)
    watchlist$train <- dtrain
    watchlist$test <- dtest
  }
  xgboost::xgboost(
    data = dtrain,
    objective = params$objective,
    max_depth = params$depth,
    min_child_weight = params$min_child,
    gamma = params$min_split_loss,
    eta = params$learning_rate,
    max_delta_step = params$max_delta_step,
    nrounds = params$n_iters,
    subsample = params$row_fraction,
    colsample_bytree = params$col_fraction,
    nthread = params$n_cores,
    watchlist = watchlist
  )
}


model_predict.xgb.Booster <- function(object, newdata, params) {
  predict(object, newdata)
}
