expand_grid <- function(...) {
  grd <- expand.grid(...) %>%
    as_tibble()
  nc <- ncol(grd)
  text <- 
    str_c(
      "grd %>% arrange(",
      str_c(str_c(".[[", 1:nc, "]]"), collapse = ", "),
      ")"
    )
  expr <- parse(text = text)
  eval(expr) %>% mutate_if(is.factor, as.character)
}
