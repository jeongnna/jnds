xgboost <- function(x, y, params = list(), x_val = NULL, y_val = NULL) {
  if (is.null(params$objective)) {
    if (is.numeric(y)) {
      params$objective <- "reg:linear"
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
  if (is.null(params$n_iters)) params$n_iters <- 100
  if (is.null(params$row_fraction)) params$row_fraction <- 1
  if (is.null(params$col_fraction)) params$col_fraction <- 1
  if (is.null(params$n_cores)) params$n_cores <- 1
  if (is.null(params$print_every_n)) params$print_every_n <- 10
  if (is.null(params$early_stopping_iters)) params$early_stopping_iters <- 10
  if (is.null(params$watch)) params$watch <- "train"
  
  if (!is.null(params$seed)) {
    set.seed(params$seed)
  }
  x_mat <- model.matrix(~ . - 1, data = x)
  dtrain <- xgb.DMatrix(data = x_mat, label = y)
  watchlist <- list(train = dtrain)
  if (!is.null(x_val)) {
    x_mat_val <- model.matrix(~ . - 1, data = x_val)
    dval <- xgb.DMatrix(data = x_mat_val, label = y_val)
    watchlist$val <- dval
  }
  watchlist <- watchlist[params$watch]
  xgboost::xgb.train(
    params = list(
      objective = params$objective,
      max_depth = params$depth,
      min_child_weight = params$min_child,
      gamma = params$min_split_loss,
      eta = params$learning_rate,
      max_delta_step = params$max_delta_step,
      subsample = params$row_fraction,
      colsample_bytree = params$col_fraction,
      nthread = params$n_cores
    ),
    data = dtrain,
    nrounds = params$n_iters,
    print_every_n = params$print_every_n,
    early_stopping_rounds = params$early_stopping_iters,
    watchlist = watchlist
  )
}


model_predict.xgb.Booster <- function(object, newdata, params = list()) {
  d_mat <- model.matrix(~ . - 1, data = newdata)
  dnewdata <- xgb.DMatrix(data = d_mat)
  predict(object, dnewdata, ntreelimit = object$best_iteration)
}
