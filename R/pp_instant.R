#' Create a pp module which evaluates expression specified in its argument.
#'
#' @param expr Expression.
#' @param sub_object Any object necessary to evaluate `expr`.
#' @param desc (optional) Description of the module.
pp_instant <- function(expr, sub_object = NULL,
                       desc = NULL, data = NULL, standby = TRUE) {
  if (standby) {
    return (function(data) pp_instant(expr, sub_object, desc, data, standby = FALSE))
  }
  pp(
    expr = expr,
    sub_object = sub_object,
    desc = desc,
    subclass = "pp_instant"
  )
}


predict.pp_instant <- function(object, data) {
  eval(object$expr)
  data
}
