mae <- function(pred, y) {
  mean(abs(pred - y))
}


mse <- function(pred, y) {
  mean((pred - y)^2)
}


rmse <- function(pred, y) {
  sqrt(mean((pred - y)^2))
}
