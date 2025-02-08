# templight --------------------------------------------------------------------

#' Conditionally format a vector by location
#'
#' @description `templight()` creates a vector with a conditional [format()]
#' and [print()] method. The function takes an input vector `.x`, a vector
#' of locations `.at`, and a formatter function `.f`. When the result of
#' `templight(.x, .at, .f)` is printed, elements of `.x` at indices specified
#' by `.at` are transformed by `.f` before they are printed.
#'
#' `.at` and `.f` may be equal length lists of indexes and functions. Elements
#' of `.x` at locations `.at[[i]]` are transformed using `.f[[i]]`. Conditional
#' formats are applied to the highlighted vector in the order that they are
#' applied to `.at` and `.f`.
#'
#' `templight_mult()` and `templight_case()` allow these pairs of locations and
#' functions to be supplied as two-sided formulas `.at ~ .f` using [dplyr::case_when()]
#' style syntax.
#'
#' `tl()` and `templight()` are synonyms, as are `tl_mult()` and
#' `templight_mult()`, `tl_case()` and `templight_case()`.
#'
#' @details
#'
#' Vectors highlighted by `templight()` are said to be "temporarily"
#' conditionally formatted because the association between `.at` and
#' the elements of `.x` is likely to exist only temporarily.
#'
#' For example, the code below creates a temp-lighted vector `minimum_tl`
#' which conditionally formats the minimum element of `x`.
#'
#' ```
#' x <- c(4, 3, 0, 2, 1)
#' minimum_tl <- templight(x, .at = x == min(x), .f = wrap("<", ">"))
#' print(minimum_tl) # 4 3 <0> 2 1
#' ```
#'
#' When the vector `minimum_tl` is altered, by sorting it for example, the
#' conditionally formatted element is no longer the minimum value of `x`,
#' it is whatever happens to be the third element of the temp-lighted vector.
#'
#' ```
#' print(sort(minimum_tl)) # 0 1 <2> 3 4
#' ```
#'
#' This is in contrast to vectors created using `highlight()`, which maintain
#' an association between specific elements of the data `x` and a conditional
#' format `.f`.
#'
#' ```
#' # Highlight the minimum element of `x` (i.e. 0)
#' minimum_hl <- highlight(x, .t = ~ .x == min(.x), .f = wrap("<", ">"))
#'
#' # The minimum element of `x` is still highlighted after sorting
#' print(sort(minimum_hl)) # <0> 1 2 3 4
#' ```
#'
#' @inheritParams highlight
#'
#' @param .at `[logical / integerish / list]`
#'
#'  Locations of elements in `.x` to conditionally format, supplied as a logical,
#'  integer, or whole-numbered (e.g. `1.0`, `2.00`) numeric vector. `.at` may
#'  also be a list of such vectors.
#'
#'  For example, `.at = c(TRUE, FALSE, TRUE)` corresponds to elements `1` and `3`
#'  of `.x` (if they exist). As does `.at = c(1, 3)` and `.at = c(1.0, 3.0)`.
#'
#'  By default, `.at` is an empty logical vector.
#'
#' @param ... `[formula / vlightr_highlighter]`
#'
#'  For `templight_mult()` and `templight_case()`, a two sided formula with
#'  locations on the left-hand-side and a formatter on the right-hand-side. This
#'  argument replaces the `.at` and `.f` arguments of `templight()`. The ith dot
#'  `..i` is roughly equivalent to `.at[[i]] ~ .f[[i]]`.
#'
#'  The left-hand-side of the formula may be:
#'  * A logical vector, e.g. `c(TRUE, FALSE)`, `letters == "r"`
#'  * Numeric indices, e.g. `c(1, 2, 3)`, `which(letters %in% c("q", "r"))`
#'
#'  The right-hand-side of the formula may be:
#'  * A named function, e.g. `cli::style_hidden`
#'  * An anonymous function, e.g. `\(x) tolower(trimws(x))`
#'  * A purrr-style lambda expression, e.g. `paste(seq_along(.x), "is", .x)`
#'
#'  A temp-lighter, e.g. `templighter(c(1, 2, 3), color("blue"))`, may be
#'  supplied instead of a formula, in which case the arguments `.at` and `.f`
#'  of the temp-lighter are spliced into `...`.
#'
#'  In the case of `templight_case()`, elements of `.x` can be conditionally
#'  formatted at most once, similar to the behavior of [dplyr::case_when()].
#'
#' @return
#'
#' A vector of class `vlightr_templight`. For `templight_case()`, the a
#' vector of class `vlightr_templight/vlightr_highlight_case`.
#'
#' @seealso
#'
#' [highlight()] for conditionally formatting elements of a vector `.x`
#' for which a test `.t` returns `TRUE`.
#'
#' [is_highlightable()] for testing whether an object can be highlighted.
#'
#' [un_highlight()] for converting a vector `highlight(.x)` back to `.x`.
#'
#' [tests()], [formatters()], [highlight_functions()] for setting and
#' getting the values of `.t` (i.e. tests) and `.f` (i.e. formatters) of a
#' highlighted vector.
#'
#' [color()] and friends for generating formatter functions for use in `.f`.
#'
#' @examples
#' # TODO
#'
#' @export
templight <- function(
    .x = logical(),
    .at = logical(),
    .f = getOption("vlightr.default_formatter")
  ) {
  x <- check_is_highlightable(.x)
  tests <- check_is_list_of_index(.at) |> map(as_index_test)
  formatters <- check_is_list_of_functionish(.f)
  assert_same_length(tests, formatters, x_name = ".at", y_name = ".f")

  validate_highlight(
    x = new_highlight(x, tests, formatters, subclass = "vlightr_templight"),
    x_name = rlang::caller_arg(.x)
  )
}

#' @rdname templight
#' @export
tl <- templight

#' @rdname templight
#' @export
templight_mult <- function(.x, ...) {
  rlang::check_required(.x)
  x <- check_is_highlightable(.x)
  funs <- prepare_templight_functions(...)

  validate_highlight(
    new_highlight(
      x,
      tests = funs$test,
      formatters = funs$formatter,
      subclass = "vlightr_templight"
    ),
    x_name = rlang::caller_arg(.x)
  )
}

#' @rdname templight
#' @export
tl_mult <- templight_mult

#' @rdname templight
#' @export
templight_case <- function(.x, ...) {
  rlang::check_required(.x)
  x <- check_is_highlightable(.x)
  funs <- prepare_templight_functions(...)

  validate_highlight(
    new_highlight(
      x,
      tests = funs$test,
      formatters = funs$formatter,
      subclass = c("vlightr_templight", "vlightr_highlight_case")
    ),
    x_name = rlang::caller_arg(.x)
  )
}

#' @rdname templight
#' @export
tl_case <- templight_case

#' Test if the object is a temp-light vector
#'
#' @description
#'
#' This function returns `TRUE` for temp-lighted vectors (class `vlightr_templight`)
#' or subclasses thereof, and returns returns `FALSE` otherwise.
#'
#' @param x An object to test.
#' @return `TRUE` if `x` is a `vlightr_templight`, `FALSE` otherwise.
#'
#' @examples
#' # The following are `FALSE`
#' is_templight(12)
#' is_templight(highlight(12))
#'
#' # The following are `TRUE`
#' is_templight(templight(12))
#' is_templight(templight_case(12))
#'
#' @export
is_templight <- function(x) {
  inherits(x, "vlightr_templight")
}

#' Test if the object is a temp-light-case vector
#'
#' @description
#'
#' This function returns `TRUE` for vectors highlighted using `templight_case()`
#' (class `vlightr_templight/vlightr_highlight_case`), and returns `FALSE`
#' otherwise.
#'
#' @param x An object to test.
#' @return `TRUE` if `x` is a `vlightr_templight/vlightr_highlight_case`, `FALSE` otherwise.
#'
#' @examples
#' # The following are `FALSE`
#' is_templight_case(10)
#' is_templight_case(highlight(10))
#' is_templight_case(highlight_case(10))
#'
#' # The following are `TRUE`
#' is_templight_case(templight_case(10))
#'
#' # Vectors highlighted using the `templigher_case()` or highlighter are
#' class `vlightr_templight/vlightr_highlight_case`.
#' templighter <- templighter_case()
#' is_templight_case(templighter(10))
#'
#' @export
is_templight_case <- function(x) {
  rlang::inherits_all(x, c("vlightr_templight", "vlightr_highlight_case"))
}

# templighter ------------------------------------------------------------------

new_templighter <- function(tests, formatters, subclass = character()) {
  highlight_subclass <- gsub("highlighter", "highlight", subclass)
  force(tests)
  force(formatters)

  out <- function(.x) {
    rlang::check_required(.x)
    x <- check_is_highlightable(.x)
    validate_highlight(
      x = new_highlight(
        .x,
        tests = tests,
        formatters = formatters,
        subclass = c("vlightr_templight", highlight_subclass)
      ),
      x_name = rlang::caller_arg(.x)
    )
  }
  attr(out, "tests") <- tests
  attr(out, "formatters") <- formatters
  class(out) <- c(
    "vlightr_templighter",
    subclass,
    "vlightr_highlighter",
    "function"
  )
  out
}

# TODO: Document and export

#' Make a temporary highlighter
#'
#' @description
#'
#' Makes a temporary highlighter.
#'
#' @export
templighter <- function(.t, .at) {
  rlang::check_required(.t)
  rlang::check_required(.at)
  tests <- check_is_list_of_index(.at) |> map(as_index_test)
  formatters <- check_is_list_of_functionish(.f)
  assert_same_length(tests, formatters, x_name = ".at", y_name = ".f")

  new_templighter(tests = tests, formatters = formatters)
}

#' @rdname templighter
#' @export
templighter_mult <- function(...) {
  funs <- prepare_templight_functions(...)
  new_templighter(tests = funs$test, formatters = funs$formatter)
}

#' @rdname templighter
#' @export
templighter_case <- function(...) {
  funs <- prepare_templight_functions(...)
  new_templighter(
    tests = funs$test,
    formatters = funs$formatter,
    subclass = "vlightr_highlighter_case"
  )
}

#' Test if the object is a temp-lighter function
#'
#' @description
#'
#' This function returns `TRUE` for functions created using `templighter()`
#' or `templighter_case()` (class `vlightr_templighter`), and returns `FALSE`
#' otherwise.
#'
#' @param x An object to test.
#' @return `TRUE` if `x` is a `vlightr_templighter`, `FALSE` otherwise.
#' @export
is_templighter <- function(x) {
  inherits(x, "vlightr_templighter")
}

#' Test if the object is a temp-lighter function
#'
#' @description
#'
#' This function returns `TRUE` for functions created using `templighter_case()`
#' (class `vlightr_templighter/vlightr_highlighter_case`), and returns `FALSE`
#' otherwise.
#'
#' @param x An object to test.
#' @return `TRUE` if `x` is a `vlightr_templighter/vlightr_highlighter_case`, `FALSE` otherwise.
#' @export
is_templighter_case <- function(x) {
  rlang::inherits_all(x, c("vlightr_templighter", "vlighter_highlighter_case"))
}

# templight helpers ------------------------------------------------------------

prepare_templight_functions <- function(...) {

  error_call <- rlang::caller_env()
  dots <- rlang::list2(...)
  formulas_at <- map_lgl(dots, is_formula)
  templighters_at <- map_lgl(dots, is_templighter)

  if (!all(formulas_at | highlighters_at)) {
    invalid_at <- which.min(formulas_at | templighters_at)
    cli::cli_abort(
      c(
        "Arguments to `...` must be a formula or {.cls vlightr_templightr} function.",
        x = "`..{invalid_at}` is {.obj_type_friendly {dots[invalid_at]}}."
      ),
      call = error_call,
      class = "vlighter_error"
    )
  }

  # Each highlighter may contain multiples pairs of tests and formatters which
  # need to be split apart without altering the order of user-supplied `...`.
  if (any(highlighters_at)) {
    # The second `map(mapply())` makes all elements of `out` a list of lists, which
    # we flatten uniformly with `vctrs::list_unchop()` so that every element of
    # `out` is a list.
    out <- vector("list", length(dots))
    out[formulas_at] <- map(mapply(
      frm = dots[formulas_at],
      frm_name = paste0("..", which(formulas_at)),
      FUN = split_templight_formula,
      MoreArgs = list(error_call = rlang::caller_env()),
      SIMPLIFY = FALSE
    ), list)
    out[highlighters_at] <- map(dots[highlighters_at], get_highlight_functions)
    out <- vctrs::list_unchop(out)
  }
  else {
    out <- mapply(
      frm = dots,
      frm_name = paste0("..", seq_along(dots)),
      FUN = split_templight_formula,
      MoreArgs = list(error_call = rlang::caller_env()),
      SIMPLIFY = FALSE
    )
  }
  unzip(out)
}

split_templight_formula <- function(frm, frm_name, error_call) {
  if (is_one_sided_formula(frm)) {
    test <- true
    formatter <- new_formatter_function(rlang::f_rhs(frm), rlang::f_env(frm))
  }
  else {
    env <- rlang::f_env(frm)
    lhs_result <- rlang::try_fetch(
      eval(rlang::f_lhs(frm), env),
      error = function(cnd) {
        cli::cli_abort(
          c(
            "Can't convert left-hand-side of formula {.arg {frm_name}} to a function.",
            i = paste(
              "The left-hand-side of {.arg {frm_name}} must be a logical",
              "or integer-like vector."
            )
          ),
          parent = cnd,
          call = error_call,
          class = "vlightr_error"
        )
      }
    )
    if (!(is.logical(lhs_result) || rlang::is_integerish(lhs_result))) {
      cli::cli_abort(
        c(
          paste(
            "The left-hand-side of formula {.arg {frm_name}} must be a logical",
            "or integer-like vector."
          ),
          x = "The left-hand-side of {.arg {frm_name}} is {.obj_type_friendly {lhs_result}}."
        ),
        call = error_call,
        class = "vlightr_error"
      )
    }
    test <- as_index_test(lhs_result)
    formatter <- new_formatter_function(rlang::f_rhs(frm), env)
  }
  list(test = test, formatter = formatter)
}

as_index_test <- function(indices) {
  if (is.logical(indices)) {
    indices[is.na(indices)] <- FALSE
    indices <- which(indices)
  }
  else {
    indices <- unique(indices)
  }
  function(x) {
    seq_along(x) %in% indices
  }
}
