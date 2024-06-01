`%notin%` <- Negate(`%in%`)

`%!|%` <- function(lhs, rhs) if (is.null(lhs)) lhs else rhs

`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

# Inspired by: https://stackoverflow.com/questions/33756754/sorting-a-list-of-unequal-size-vectors-in-r
order_ragged <- function(x) {
  x_unragged <- do.call(rbind.data.frame, lapply(x, `length<-`, max(lengths(x))))
  do.call(order2, x_unragged)
}

# Allows shorted vectors to be ordered first in `sort_ragged`
order2 <- function(...) order(..., na.last = FALSE)
