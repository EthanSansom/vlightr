#' Split a highlighted vector by conditions
#'
#' @description
#'
#' [condition_split()] divides a highlighted vector `x` into the groups defined
#' by `attr(x, "conditions")`. Elements of `x` which receive the same conditional
#' formatting are grouped together.
#'
#' @param x `[vlightr_highlight]`
#'
#' A highlighted vector to split.
#'
#' @param unlit `[logical(1)]`
#'
#' Whether elements of the output list are un-highlighted. Defaults to `TRUE`.
#'
#' @examples
#' x <- highlight(1:5)
#'
#' @family condition utilities
#' @export
condition_split <- function(x, unlit = TRUE) {

}

condition_id <- function(x, no_id = NA_character_) {

}

condition_met <- function(
    x,
    conds = NULL,
    meets = c("all", "any", "none")
  ) {

  x <- check_is_highlight(x)
  meets <- rlang::arg_match(meets)
  conditions <- attr(x, "conditions")

  # If no conditions were supplied, use all of them
  if (!is.null(conds)) {
    # TODO: Add ability to reference conditions by index OR by description
    conditions <- vctrs::vec_slice(conditions, conds)
  }

  # Occurs after slicing, so invalid `conds` still errors for empty conditions.
  if (rlang::is_empty(conditions)) {
    return(rep(TRUE, length(x)))
  }

  # TODO: Use `evalidate_format_fn` here
  out <- conditions[[1]](x)
  op <- if (meets == "any") `|` else `&`
  for (condition in conditions[-1]) {
    out <- op(out, condition(x))
    out[is.na(out)] <- FALSE
  }

  if (meets == "none") !out else out
}

condition_filter <- function(x, conds = NULL, meets = c("all", "any", "none")) {
  x[condition_met(x, conds = conds, meets = meets)]
}
