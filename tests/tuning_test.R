library(tidyverse)
source("../R/model_predict.R")
source("../R/gbm.R")
source("../R/expand_grid.R")
source("../R/model_tune.R")


# data -----

set.seed(123)
train <- iris %>% sample_frac(0.7)
val <- setdiff(iris, train)
x_train <- train %>% select(-Species)
y_train <- train$Species
x_val <- val %>% select(-Species)
y_val <- val$Species


# config -----

false_rate <- function(pred, y) {
  response <- colnames(pred)[apply(pred, 1, which.max)]
  mean(response != y)
}

params <- list(
  list(name = "learning_rate", range = c(-3, 1), expand = 0.5),
  list(name = "n_trees", range = c(1, 3), expand = FALSE)
)

more_params <- list(
  dist = "multinomial",
  depth = 2
)


# tune -----

history <- model_tune(
  params = params,
  num_search = 30,
  x_train = x_train,
  y_train = y_train,
  x_val = x_val,
  y_val = y_val,
  model = gbm,
  loss_fn = false_rate,
  more_params = more_params,
  max_iters = 3,
  mc_cores = 4,
  seed = 123
)


# vis ----

vis_tunegrid(history$case_grid[[1]], c("Learning rate" = "learning_rate"), loss_reduce = "min")
vis_tunegrid(history$case_grid[[1]], "learning_rate", loss_reduce = "mean")
vis_tunegrid(history$case_grid[[1]], c("Learning rate" = "learning_rate", "Number of trees" = "n_trees"), loss_reduce = "min")
vis_tunegrid(history$case_grid[[1]], c("learning_rate", "n_trees"), loss_reduce = "mean")
