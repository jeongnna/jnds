eachpar_idx_ <- function(idx, n_case, n_par) {
  res <- integer(n_par)
  d <- n_case^n_par
  for (i in 1:n_par) {
    tmp <- idx %% d
    if (tmp == 0) tmp <- d
    d <- n_case^(n_par - i)
    q <- tmp %/% d
    r <- tmp %% d
    res[i] <- q + (r > 0)
  }
  res
}


model_tune <- function(params, n_search, data_list,
                       model, loss_fn, more_params = list(),
                       max_iters = 1, n_cores = 1, seed = 0,
                       early_stop_rate = 0.01) {
  # initialize
  for (k in seq_along(params)) {
    if (is.null(params[[k]]$expand)) {
      message(str_c("`expand` is null in ", params[[k]]$name, ". `FALSE` is used as default."))
      params[[k]]$expand <- FALSE
    }
    if (is.null(params[[k]]$power)) {
      message(str_c("`power` is null in ", params[[k]]$name, ". `TRUE` is used as default."))
      params[[k]]$power <- TRUE
    }
    if (is.null(params[[k]]$search_type)) {
      message(str_c("`search_type` is null in ", params[[k]]$name, ". \"random\" is used as default."))
      params[[k]]$search_type <- "random"
    }
    if (is.null(params[[k]]$dtype)) {
      message(str_c("`dtype` is null in ", params[[k]]$name, ". \"float\" is used as default."))
      params[[k]]$dtype <- "float"
    }
  }

  history <- list()
  prev_best_params <- rep(0, length(params))
  y_val_full <- unlist(map(data_list, "y_val"))

  # tune parameters
  for (iter in seq_len(max_iters)) {
    cat(iter, "th tuning\n", sep = "")

    # create parameter grid
    cat("search range (in before-power scale) :\n")
    for (k in seq_along(params)) {
      set.seed(seed)
      range <- params[[k]]$range
      if (params[[k]]$search_type == "grid") {
        params[[k]]$cand <- seq(range[1], range[2], length.out = n_search)
      } else if (params[[k]]$search_type == "random") {
        params[[k]]$cand <- sort(runif(n_search, range[1], range[2]))
      }
      
      if (params[[k]]$power) {
        params[[k]]$cand_pow <- 10^params[[k]]$cand
      } else {
        params[[k]]$cand_pow <- params[[k]]$cand
      }
      if (params[[k]]$dtype == "integer") {
        params[[k]]$cand_pow <- floor(params[[k]]$cand_pow)
        if (params[[k]]$power) {
          params[[k]]$cand <- log10(params[[k]]$cand_pow)
        } else {
          params[[k]]$cand <- params[[k]]$cand_pow
        }
      }
      
      cat("    ", params[[k]]$name, ": (", range[1], ", ", range[2], ")\n", sep = "")
      seed <- seed + 1
    }

    # loss for each parameter
    params_grid <- expand_grid(transpose(params)$cand_pow)
    names(params_grid) <- unlist(transpose(params)$name)
    process_ <- function(i) {
      all_params <- more_params
      for (k in seq_along(params)) {
        all_params[[params[[k]]$name]] <- pull(params_grid[i, k])
      }
      pred <- NULL
      for (data in data_list) {
        x_train <- data$x_train
        y_train <- data$y_train
        x_val <- data$x_val
        y_val <- data$y_val
        model_fitted <- model(x_train, y_train, all_params, x_val, y_val)
        pred <- c(pred, model_predict(model_fitted, x_val, params = all_params))
      }
      loss_fn(pred, y_val_full)
    }
    losses <- unlist(pbmclapply(1:nrow(params_grid), process_, mc.cores = n_cores))
    #########################################################################
    # losses <- NULL                    # When pbmclapply in the above line #
    # for (i in 1:nrow(params_grid)) {  # encounters an error, replace with #
    #   losses[i] <- process_(i)        # this block and then track the     #
    # }                                 # error point.                      #
    #########################################################################
    
    # find the optimal point
    best <- which.min(losses)
    best_params <- unlist(params_grid[best, ])
    cat("best case:\n")
    for (k in seq_along(params)) {
      cat("    ", params[[k]]$name, ": ", best_params[k], "\n", sep = "")
    }
    cat("loss: ", losses[best], "\n\n", sep = "")

    # update history
    params_grid <- bind_cols(params_grid, loss = losses)
    history[[iter]] <- params_grid

    # early stop when update is small
    if (all(abs((best_params - prev_best_params) / best_params) < early_stop_rate)) {
      message(str_c("Early stopped in iteration ", iter))
      break
    }
    prev_best_params <- best_params

    # update parameter ranges
    best_eachpar <- eachpar_idx_(best, n_case = n_search, n_par = length(params))

    for (k in seq_along(params)) {
      b <- best_eachpar[k]
      range <- params[[k]]$range
      expand <- params[[k]]$expand
      cand <- params[[k]]$cand
      # When the optimal point is ...
      if (b == 1) {                    # (1) left boundary
        next_min <- range[1] - expand * diff(range)
        next_max <- cand[b + 1]
      } else if (b == length(cand)) {  # (2) right boundary
        next_min <- cand[b - 1]
        next_max <- range[2] + expand * diff(range)
      } else {                         # (3) an interior point
        next_min <- cand[b - 1]
        next_max <- cand[b + 1]
        params[[k]]$expand <- FALSE
      }
      params[[k]]$range <- c(next_min, next_max)
    }
  }

  history
}


best_cases <- function(history) {
  history %>%
    map(function(df) df[which.min(df$loss), ]) %>%
    bind_rows() %>%
    add_column(iteration = 1:nrow(.), .before = 1)
}


vis_grid <- function(grd, par, loss_reduce = c("min", "mean")) {
  if (!length(par) %in% 1:2) {
    stop("`par` must have length 1 or 2.")
  }
  if (typeof(loss_reduce) == "character") {
    reduce <- get(loss_reduce[1])
  } else if (typeof(loss_reduce) == "closure") {
    reduce <- loss_reduce
  } else {
    stop(str_c("Unexpected type of `loss_reduce`: ", typeof(loss_reduce)))
  }

  if (is.null(names(par))) {
    par_names <- par
  } else {
    par_names <- names(par)
    par <- unname(par)
  }

  best <- which.min(grd$loss)
  x_best <- grd[[par[1]]][best]
  if (length(par) == 2) {
    y_best <- grd[[par[2]]][best]
  }

  grd <-
    grd %>%
    group_by_at(par) %>%
    summarize(loss = reduce(loss))

  if (length(par) == 1) {
    ggplot() +
      geom_point(aes(x = grd[[par[1]]], y = grd[["loss"]]), alpha = .8) +
      geom_line(aes(x = grd[[par[1]]], y = grd[["loss"]]), alpha = .5) +
      geom_vline(xintercept = x_best, lty = 2) +
      scale_x_log10() +
      labs(x = par_names[1], y = "Loss") +
      theme_bw()

  } else {
    ggplot() +
      geom_point(aes(x = grd[[par[1]]], y = grd[[par[2]]], col = grd[["loss"]]), alpha = .8) +
      geom_vline(xintercept = x_best, lty = 2) +
      geom_hline(yintercept = y_best, lty = 2) +
      scale_x_log10() +
      scale_y_log10() +
      scale_color_distiller(name = "Loss", palette = "RdBu") +
      labs(x = par_names[1], y = par_names[2]) +
      theme_bw()
  }
}
