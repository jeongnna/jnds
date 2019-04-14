library(dplyr)
library(ggplot2)


gg_jitter_box <- function(x, y, config = list()) {
  if (is.null(config$xlab)) {
    config$xlab <- "x"
  }
  if (is.null(config$ylab)) {
    config$xlab <- "y"
  }
  if (is.null(config$alpha)) {
    config$alpha <- .5
  }
  tibble(x = x, y = y) %>%
    ggplot(aes(x, y)) +
    geom_jitter(alpha = config$alpha) +
    geom_boxplot(alpha = config$alpha) +
    labs(title = config$title, x = config$xlab, y = config$ylab)
}


gg_ycut_xprop <- function(x, y, config = list()) {
  if (is.null(config$xlab)) {
    config$xlab <- "x"
  }
  if (is.null(config$ylab)) {
    config$xlab <- "y"
  }
  if (is.null(config$cut)) {
    config$cut <- 20
  }
  # reverse levels of x
  x <- factor(x, levels = rev(levels(x)))
  # partition y
  y <- cut(y, config$cut)
  tibble(x = x, y = y) %>%
    ggplot(aes(y, fill = x)) +
    geom_bar(position = "fill") +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    labs(title = config$title, y = config$xlab, x = config$ylab)
}
