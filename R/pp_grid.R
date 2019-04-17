#' Create pp_grid module.
#'
#' @param ... pp modules.
#' @param data A data frame. It should be a training data.
#' @param default The baseline pp module. It will precede
#'   other modules in all cases in grid.
#' @param type If `"grid"`, all possible subsets will be considered.
#'   If `"switch"`, each module will be added to default indepentently.
#'   If `"step"`, modules will be added to default one by one.
pp_grid <- function(..., data, default = pp(), type = "grid") {
  if (!is_pp(default)) {
    stop("`default` must be a pp object.")
  }
  n <- length(args <- list(...))
  data <- predict(default, data)
  if (type == "switch") {
    idx_list <- 1:n
  } else if (type == "step") {
    idx_list <- lapply(1:n, function(x) (1:n)[1:x])
  } else if (type == "grid") {
    idx_list <- unlist(lapply(1:n, combn, x = n, simplify = FALSE), recursive = FALSE)
  }
  object <- lapply(
    idx_list,
    function(idx) pp_sequential(pplist = args[idx], data = data)
  )
  class(object) <- "pp_grid"
  object
}


#' @rdname pp_grid
pp_switch <- function(..., data, default = pp()) {
  pp_grid(..., data = data, default = default, type = "switch")
}


#' @rdname pp_grid
pp_step <- function(..., data, default = pp()) {
  pp_grid(..., data = data, default = default, type = "step")
}


is_pp_grid <- function(x) {
  inherits(x, "pp_grid")
}


print.pp_grid <- function(object, n = 7) {
  len <- length(object)
  message("## pp_grid module of ", len, " cases")
  count <- 0
  for (pp in object) {
    count <- count + 1
    cat("\n## case ", count, "\n", sep = "")
    print(pp)
    if (count >= n) {
      message("\n## ... with ", len - count, " more cases")
      break
    }
  }
}


predict.pp_grid <- function(object, data) {
  lapply(object, function(x) predict(x, data))
}
