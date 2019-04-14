library(tidyverse)
source("../R/pp.R")
source("../R/pp_factor.R")
source("../R/pp_grid.R")
source("../R/pp_na_impute.R")
source("../R/pp_normalize.R")
source("../R/pp_pca.R")
source("../R/pp_remove.R")
source("../R/pp_instant.R")
source("../R/pp_sequential.R")


# Preparation -------------------------------------------------------------

df <- as_tibble(iris)
df$Species <- as.character(df$Species)
for (i in 1:5) {
  set.seed(i)
  s <- sample(1:nrow(df), 10)
  df[s, i] <- NA
}


# Each pp module ----------------------------------------------------------

# cat -> factor
cat_cols <- "Species"
cat2factor <- pp_factor(
  cols = cat_cols,
  desc = "Convert categorical variables to factor"
)

# NA imputing
num_cols <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
na_imputing <- pp_na_impute(
  num_cols = num_cols,
  cat_cols = cat_cols,
  desc = "Fill NA's: numeric -> mean, categorical -> mode"
)

# Feature transformation
new_petal <- pp_instant(expr(
  data <- mutate(data, Petal = Petal.Length + Petal.Width)
),
desc = "New feature: Petal = Petal.Length + Petal.Width"
)

# Remove feature
remove_petal_rels <- pp_remove(
  cols = c("Petal.Length", "Petal.Width"),
  desc = "Remove Petal.Length, Petal.Width"
)


# Normalizing
norm_cols <- c("Sepal.Length", "Sepal.Width", "Petal")
normalizing <- pp_normalize(
  norm_cols,
  method = "mean-sd",
  desc = "Normalize numeric features by mean-sd"
)

# PCA
pca <- pp_pca(
  cols = norm_cols,
  threshold = .8,
  desc = "PCA with thresholde 0.8"
)


# Sequential --------------------------------------------------------------

pp1 <- pp_sequential(
  cat2factor,
  na_imputing,
  new_petal,
  remove_petal_rels,
  data = df
)

pp2 <- pp_sequential(
  pp1,
  normalizing,
  pca,
  data = df
)

pp3 <- pp_switch(
  new_petal,
  remove_petal_rels,
  default = pp_sequential(
    cat2factor,
    na_imputing,
    data = df
  ),
  data = df
)

pp4 <- pp_step(
  new_petal,
  remove_petal_rels,
  default = pp_sequential(
    cat2factor,
    na_imputing,
    data = df
  ),
  data = df
)

pp5 <- pp_grid(
  new_petal,
  remove_petal_rels,
  default = pp_sequential(
    cat2factor,
    na_imputing,
    data = df
  ),
  data = df
)


# Run ---------------------------------------------------------------------

df
pp1
predict(pp1, data = df)
pp2
predict(pp2, data = df)
pp3
predict(pp3, data = df)
pp4
predict(pp4, data = df)
pp5
predict(pp5, data = df)

