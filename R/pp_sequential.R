#' Create pp_sequential module.
#'
#' @param ... pp modules.
#' @param data A data frame. It should be a training data.
#' @param pplist A list contains pp modules.
pp_sequential <- function(..., data, pplist = NULL) {
  args <- append(list(...), pplist)
  object <- list()
  count = 0
  for (arg in args) {
    count = count + 1
    if (is.function(arg)) {
      arg <- arg(data)
    }
    if (!is_pp(arg)) {
      stop(str_c("Argument ", count, " is not a pp object."))
    }
    data <- predict(arg, data)
    if (!is_pp_sequential(arg)) {
      arg <- list(arg)
    }
    object <- append(object, arg)
  }
  class(object) <- c("pp_sequential", "pp")
  object
}


is_pp_sequential <- function(x) {
  inherits(x, "pp_sequential")
}


print.pp_sequential <- function(object, n = 10) {
  len <- length(object)
  message("## pp_sequential module of length ", len)
  count <- 0
  for (pp in object) {
    count <- count + 1
    cat("# ", count, ": ", sep = "")
    print(pp)
    if (count >= n) {
      message("# ... with ", len - count ," more modules")
      break
    }
  }
}


predict.pp_sequential <- function(object, data) {
  for (pp in object) {
    data <- predict(pp, data)
  }
  data
}

