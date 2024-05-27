`%notin%` <- Negate(`%in%`)

`%!|%` <- function(lhs, rhs) if (is.null(lhs)) lhs else rhs

`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs
