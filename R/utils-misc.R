`%notin%` <- Negate(`%in%`)

`%!|%` <- function(lhs, rhs) if (is.null(lhs)) lhs else rhs

`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

`%|?%` <- function(lhs, rhs) if (rlang::is_missing(lhs)) rhs else lhs

# Inspired by: https://stackoverflow.com/questions/33756754/sorting-a-list-of-unequal-size-vectors-in-r
# Orders a list of vectors of varying lengths:
#> sort_ragged <- \(x) x[order_ragged(x)]`
#> sort_ragged(list(c(1, 2), 1, c(2, 4), c(2, 1)))
#> list(1, c(1, 2) c(2, 3), c(2, 4))
order_ragged <- function(x) {
  x_unragged <- do.call(rbind.data.frame, lapply(x, `length<-`, max(lengths(x))))
  do.call(order2, x_unragged)
}

# Allows shorter vectors to be ordered first in `sort_ragged`
order2 <- function(...) order(..., na.last = FALSE)

