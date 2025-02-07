`%notin%` <- Negate(`%in%`)

`%!|%` <- function(lhs, rhs) if (is.null(lhs)) lhs else rhs

`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

`%|?%` <- function(lhs, rhs) if (rlang::is_missing(lhs)) rhs else lhs

map <- function(.x, .f, ...) {
  lapply(X = .x, FUN = .f, ...)
}

map_lgl <- function(.x, .f, ...) {
  vapply(.x, FUN = .f, FUN.VALUE = logical(1L), ...)
}

map_chr <- function(.x, .f, ...) {
  vapply(.x, FUN = .f, FUN.VALUE = character(1L), ...)
}

compact <- function(.x) {
  .x[!map_lgl(.x, is.null)]
}

is_formula <- function(x) {
  rlang::is_formula(x, scoped = TRUE)
}

is_two_sided_formula <- function(x) {
  rlang::is_formula(x, scoped = TRUE, lhs = TRUE)
}

is_one_sided_formula <- function(x) {
  rlang::is_formula(x, scoped = TRUE, lhs = FALSE)
}

# Inspired by: https://stackoverflow.com/questions/33756754/sorting-a-list-of-unequal-size-vectors-in-r
# Orders a list of vectors of varying lengths:
#> sort_ragged <- \(x) x[order_ragged(x)]`
#> sort_ragged(list(c(1, 2), 1, c(2, 4), c(2, 1)))
#> list(1, c(1, 2) c(2, 3), c(2, 4))
order_ragged <- function(x) {
  order2 <- function(...) order(..., na.last = FALSE)
  x_unragged <- do.call(rbind.data.frame, lapply(x, `length<-`, max(lengths(x))))
  do.call(order2, x_unragged)
}
