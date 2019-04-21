library(tidyverse)
library(jnds)
library(xgboost)
library(pbmcapply)


cv_idx_generate <- function(n, n_folds, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  full_idx <- 1:n
  cv_idx <- list()
  for (i in seq_len(n_folds - 1)) {
    temp_idx <- sample(full_idx, n / n_folds, replace = FALSE)
    cv_idx[[i]] <- temp_idx
    full_idx <- setdiff(full_idx, temp_idx)
  }
  cv_idx[[n_folds]] <- sample(full_idx, length(full_idx), replace = FALSE)
  cv_idx
}


# data -----

train_raw <- as_tibble(iris)
train_raw$Species <- as.factor(train_raw$Species)
train_raw$Species <- as.numeric(train_raw$Species)

n_folds <- 2
cv_idx <- cv_idx_generate(nrow(train_raw), n_folds, seed = 42)

data_list <- list()
for (k in seq_len(n_folds)) {
  idx <- cv_idx[[k]]
  x_train <- select(train_raw[idx, ], -Species)
  y_train <- train_raw$Species[idx]
  x_val <- select(train_raw[idx, ], -Species)
  y_val <- train_raw$Species[idx]
  data_list[[k]] <- list(
    x_train = x_train,
    y_train = y_train,
    x_val = x_val,
    y_val = y_val
  )
}


# config -----

false_rate <- function(pred, y) {
  response <- colnames(pred)[apply(pred, 1, which.max)]
  mean(response != y)
}

params <- list(
  list(name = "learning_rate", range = c(-3, 0), expand = 0.5),
  list(name = "max_delta_step", range = c(0, 2), expand = 0.5)
)

more_params <- list(
  depth = 4
)

num_search = 4
max_iters = 3
n_cores = 1
seed = 42
model = jnds::xgboost
loss_fn = rmse


# tune -----

history <- model_tune(
  params = params,
  num_search = num_search,
  data_list = data_list,
  model = jnds::xgboost,
  loss_fn = rmse,
  more_params = more_params,
  max_iters = max_iters,
  n_cores = n_cores,
  seed = seed
)


# vis ----

vis_tunegrid(history$case_grid[[1]], c("Learning rate" = "learning_rate"), loss_reduce = "min")
vis_tunegrid(history$case_grid[[1]], "learning_rate", loss_reduce = "mean")
vis_tunegrid(history$case_grid[[1]], c("Learning rate" = "learning_rate", "Number of trees" = "n_iters"), loss_reduce = "min")
vis_tunegrid(history$case_grid[[1]], c("learning_rate", "n_iters"), loss_reduce = "mean")
