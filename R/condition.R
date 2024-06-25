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
#' @param no_cond `[character(1)]`
#'
#' The name given to the group of elements which meet no condition.
#' Defaults to `NA`.
#'
#' @returns
#'
#' A list of vectors containing the values for each group of elements in `x`. The
#' elements of the list are named using the description (`attr(x, "description"`)
#' corresponding to the conditions met by the group.
#'
#' If `unlit = FALSE`, the vectors in the list are highlighted (with the same
#' conditional formatting as `x`). Otherwise, they are un-highlighted.
#'
#' @examples
#' x <- highlight(1:5)
#'
#' @family condition utilities
#' @export
condition_split <- function(x, unlit = TRUE, no_cond = NA_character_) {
  # TODO: Just use `condition_id` here.
}

#' Identify elements of a highlighted vector which meet a condition(s)
#'
#' @description
#'
#' [condition_id()] identifies elements of a highlighted vector `x` by the
#' conditions (`attr(x, "conditions")`) that each element meets. Each element's
#' identifier is a comma separated string of descriptions
#' (`attr(x, "description")`) of the conditions met by the element.
#'
#' @param x `[vlightr_highlight]`
#'
#' A highlighted vector to ID.
#'
#' @param no_id `[logical(1)]`
#'
#' The identifier assigned to elements for which no condition returns `TRUE`.
#' Defaults to `NA`.
#'
#' @examples
#' x <- highlight(1:5)
#'
#' @family condition utilities
#' @export
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
  op <- if (meets != "all") `|` else `&`
  for (condition in conditions[-1]) {
    out <- op(out, condition(x))
    out[is.na(out)] <- FALSE
  }

  if (meets == "none") !out else out
}

condition_filter <- function(x, conds = NULL, meets = c("all", "any", "none")) {
  x[condition_met(x, conds = conds, meets = meets)]
}
