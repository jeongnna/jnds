library(tidyverse)
library(parallel)


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


model_tune <- function(params, num_search,
                       x_train, y_train, x_val, y_val,
                       model, loss_fn, more_params = NULL,
                       max_iters = 5, mc_cores = 1, seed = 0) {
  
  # 초기화
  if (is.null(more_params)) {
    more_params <- list()
  }
  for (k in seq_along(params)) {
    if (is.null(params[[k]]$expand)) {
      params[[k]]$expand <- 0.5
    }
  }
  
  history <- list(case_grid = list(), best_case = list())
  prev_best_params <- rep(0, length(params))
  
  # 파라미터 튜닝
  for (iter in seq_len(max_iters)) {
    cat(iter, "th tuning\n", sep = "")
    
    # 새로운 파라미터 서치
    cat("search range:\n")
    for (k in seq_along(params)) {
      range <- params[[k]]$range
      set.seed(seed)
      cand <- sort(runif(num_search, range[1], range[2]))
      params[[k]]$cand <- cand
      params[[k]]$cand_pow <- 10^cand
      
      cat("    ", params[[k]]$name, ": (", range[1], ", ", range[2], ")\n", sep = "")
      
      seed <- seed + 1
    }
    
    # 파라미터별 loss 계산
    params_grid <- expand_grid(transpose(params)$cand_pow)
    names(params_grid) <- unlist(transpose(params)$name)
    
    process_ <- function(i) {
      all_params <- more_params
      for (k in seq_along(params)) {
        all_params[[params[[k]]$name]] <- params_grid[[k]][i]
      }
      model_fitted <- model(x_train, y_train, all_params)
      pred <- model_predict(model_fitted, x_val, all_params)
      loss_fn(pred, y_val)
    }
    losses <- unlist(mclapply(1:nrow(params_grid), process_))
    
    best <- which.min(losses)
    best_params <- unlist(params_grid[best, ])
    
    cat("best case:\n")
    for (k in seq_along(params)) {
      cat("    ", params[[k]]$name, ": ", best_params[k], "\n", sep = "")
    }
    cat("loss: ", losses[best], "\n\n", sep = "")
    
    # 히스토리 업데이트
    params_grid <- bind_cols(params_grid, loss = losses)
    history$case_grid[[iter]] <- params_grid
    history$best_case[[iter]] <- params_grid[best, ]
    
    # 파라미터의 변화가 작으면 종료
    if (all(abs((best_params - prev_best_params) / best_params) < 0.01)) {
      break
    }
    prev_best_params <- best_params
    
    # 파라미터 범위 업데이트
    best_eachpar <- eachpar_idx_(best, n_case = num_search, n_par = length(params))
    
    for (k in seq_along(params)) {
      b <- best_eachpar[k]
      range <- params[[k]]$range
      expand <- params[[k]]$expand
      cand <- params[[k]]$cand
      
      if (b == 1) {  # 왼쪽 경계일 때
        next_min <- range[1] - expand * diff(range)
        next_max <- cand[b + 1]
        
      } else if (b == length(cand)) {  # 오른쪽 경계일 때
        next_min <- cand[b - 1]
        next_max <- range[2] + expand * diff(range)
        
      } else {  # 내부점일 때
        next_min <- cand[b - 1]
        next_max <- cand[b + 1]
        params[[k]]$expand <- FALSE
      }
      
      params[[k]]$range <- c(next_min, next_max)
    }
  }
  
  history
}


vis_tunegrid <- function(grd, par, loss_reduce = c("min", "mean")) {
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
