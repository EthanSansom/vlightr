# TODO:
# - Implement some utilities for working with the highlight's conditions
# - Do the documentation

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
    conds = 1L,
    meets = c("all", "any", "none"),
    na_as = FALSE
  ) {

}

condition_filter <- function(x, conds = 1L, meets = c("all", "any", "none")) {
  x[condition_met(x, conds = conds, meets = meets, na_as = FALSE)]
}
