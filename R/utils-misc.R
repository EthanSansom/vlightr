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

zip <- function(...) {
  dots <- rlang::list2(...)
  if (!rlang::is_dictionaryish(dots)) {
    stop_internal("`...` must have unique names.")
  } else if (rlang::is_empty(dots)) {
    stop_internal("`...` is missing.")
  } else if (length(unique(lengths(dots))) > 1) {
    stop_internal("Elements of `...` must be of equal length.")
  }
  nms <- names(dots)
  .mapply(
    dots = dots,
    FUN = \(...) rlang::set_names(list(...), nms),
    MoreArgs = list()
  )
}

unzip <- function(x) {
  if (!is.list(x)) {
    stop_internal("`x` must be a list, not {.obj_type_friendly {x}}.")
  } else if (rlang::is_empty(x)) {
    return(x)
  } else if (!all(map_lgl(x, \(x_i) is.list(x_i) && rlang::is_dictionaryish(x_i)))) {
    stop_internal("Elements of `x` must be uniquely named lists.")
  } else if (!have_same_names(x)) {
    stop_internal("Elements of `x` must have the same names.")
  }
  if (length(x) == 1) {
    return(map(x[[1]], list))
  }
  nms <- names(x[[1]])
  rlang::set_names(map(nms, \(nm) map(x, `[[`, nm)), nms)
}

have_same_names <- function(x) {
  if (length(x) < 2) {
    return(TRUE)
  }
  x_1 <- x[[1]]
  all(
    map_lgl(
      x[-1],
      \(x_i) length(x_1) == length(x_i) && setequal(names(x_1), names(x_i))
    )
  )
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

# Use `%%` as you would in a zero-indexed language. E.g. the following call
# `letters[mod_index(seq(52), length(letters))]` repeats the alphabet
# twice starting from "a".
mod_index <- function(i, n) {
  (i - 1) %% n + 1
}
