# todo -------------------------------------------------------------------------

#### Fix tibble() formatting:
#
# You'll probably want to use `pillar()` directly. Take a look at what
# `haven::labelled` does in `pillar()`. I particularly want to support
# nicer formatting for simply highlighted numbers in a tibble. Calling
# `tibble(x = hl(1:10, ~ x > 5, color("yellow"))` should look nice by
# default.

# constructor ------------------------------------------------------------------

new_highlight <- function(x, tests, formatters, subclass = character()) {
  # `vctrs::new_vctr(.data)` un-classes `.data`, so using a record instead.
  # For example, try `vctrs::new_vctr(.data = ivs::iv(1, 2))`.
  vctrs::new_rcrd(
    fields = list(.data = x),
    tests = tests,
    formatters = formatters,
    class = c(subclass, "vlightr_highlight")
  )
}

validate_highlight <- function(
    x,
    x_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error",
    error_message = NULL
) {
  if (!is_highlight(x)) {
    stop_internal("`x` must be a {.cls vlightr_highlight} vector.")
  }

  # Set the context for newly created highlights within `dplyr::across()` so we
  # can add an `across_error_hint()` for common failure cases. If `format()` is
  # successful we return `x`.
  context$in_across <- in_across()
  rlang::try_fetch(
    {
      format(x, .x_name = x_name)
      x
    },
    error = function(cnd) {
      error_message <- error_message %||% paste(
        "Attempted to create a <{vctrs::vec_ptype_full(x)}> vector with a",
        "malformed {.fn format} method."
      )
      cli::cli_abort(
        error_message,
        parent = cnd,
        call = error_call,
        class = error_class
      )
    }
  )
}

# highlight --------------------------------------------------------------------

#' Conditionally format a vector
#'
#' @description `highlight()` creates a vector with a conditional [format()]
#' and [print()] method. The function takes an input vector `.x`, a test
#' function `.t`, and a formatter function `.f`. When the result of
#' `highlight(.x, .t, .f)` is printed, elements of `.x` for which `.t` returns
#' `TRUE` are transformed by `.f` before they are printed.
#'
#' `.t` and `.f` may be equal length lists of functions. Elements of `.x` for
#' which `.t[[i]]` returns true are transformed using `.f[[i]]`.
#'
#' `highlight_mult()` and `highlight_case()` allow these pairs of functions to
#' be supplied as two-sided formulas `.t ~ .f` using [dplyr::case_when()]
#' style syntax.
#'
#' `hl()` and `highlight()` are synonyms, as are `hl_mult()` and
#' `highlight_mult()`, `hl_case()` and `highlight_case()`.
#'
#' @details
#'
#' The `highlighter_mult()` and `highlight_case()` formula syntax can conflict
#' with `dplyr::across()` in-lined formulas and other purrr-style-lambdas. In
#' particular, when the following is executed:
#'
#' ```
#' dplyr::across(col, ~highlight_mult(.x, is.na(.x) ~ color("red")))
#' ```
#'
#' The formula `is.na(.x) ~ color("red")` will be replaced with
#' `is.na(y) ~ color("red")` by `dplyr::across()`. When the expression `is.na(col)`
#' is converted to a test function by `highlight_mult()`, the object `col` will
#' not exist in the environment of the test function, causing an error (or worse,
#' a difficult to diagnose bug) when the test function is called.
#'
#' To avoid this behavior, vlightr allows the use of `.h` in it's
#' purrr-style-lambdas. Replacing `is.na(.x)` with `is.na(.h)`, as in the
#' following snippet, will work as expected:
#'
#' ```
#'  dplyr::across(y, ~highlight_mult(.x, is.na(.h) ~ color("red")))
#' ```
#'
#' All purrr-style-lambdas used by vlightr accept the symbols `.x`, `.h`, and `.`
#' as aliases for their first argument.
#'
#' @param .x `[vector]`
#'
#'  A vector to highlight. Conceptually, a vector is a collection of objects of
#'  size 1.
#'
#'  `.x` is considered a vector if:
#'  * `.x` is not a [data.frame]
#'  * `.x` is not a bare [list]
#'  * `.x` is otherwise a vector, as defined by [vctrs::obj_is_vector()]
#'
#'  Atomic vector types `"logical"`, `"integer"`, `"double"`, `"complex"`,
#'  `"character"`, and `"raw"` meet these criteria. As do many common vector
#'  classes such as `POSIXct`, `lubridate::interval`, or `ivs::iv`.
#'
#' @param .t `[function / list]`
#'
#'  Vectorized test functions that indicate which elements of `.x`
#'  to conditionally format. `.t` may be:
#'  * A named function, e.g. `is.na`
#'  * An anonymous function, e.g. `\(x) 0 <= x & x <= 1`
#'  * A purrr-style lambda, e.g. `~ nchar(.x) > 0`, `~ .h == 1`, `~ TRUE`
#'  * A list of functions or lambdas, e.g. `list(~ .x < mean(.x), is.finite)`
#'
#'  Each function in `.t` will receive `.x` as it's input and must return
#'  a logical vector the same length as `.x` or of length 1 (in which case the
#'  result will be recycled to the length of `.x`).
#'
#'  By default `.t` is the function `false_along()` which returns `FALSE` for
#'  every element of it's input. You can modify this default by setting the
#'  `vlightr.default_test` in [`options()`].
#'
#' @param .f `[function / list]`
#'
#'  Vectorized character manipulation functions used to format `.x`. Can be a:
#'  * A named function, e.g. [cli::style_bold]
#'  * An anonymous function, e.g. `\(words) gsub("hi", "hey", words)`
#'  * A purrr-style lambda, e.g. `~ paste0(.h, "!")`, `~ "fizz"`
#'  * A list of functions or lambdas, e.g. `list(~ cli::col_red(.x), toupper)`
#'
#'  Each function in `.f` will receive a character vector (of variable length)
#'  as it's only argument. A formatter must return a character vector the same
#'  length as it's input or of length 1 (in which case the result is recycled to
#'  the length of the input character).
#'
#'  ANSI string vectors (class `cli_ansi_string`) are also supported (see
#'  [cli::ansi-styles] for details).
#'
#'  By default `.f` is the function `[cli::bg_br_yellow()]` which changes the
#'  background color of it's input text to bright yellow. You can modify this
#'  default by setting the `vlightr.default_formatter` in [`options()`].
#'
#' @param ... `[formula]`
#'
#'  For `highlighter_mult()` and `highlighter_case()`, a two sided formula with
#'  a test on the left-hand-side and a formatter on the right-hand-side. This
#'  argument replaces the `.t` and `.f` arguments of `highlight()`. The ith dot
#'  `..i` is roughly equivalent to `.t[[i]] ~ .f[[i]]`.
#'
#'  The left-hand-side and right-hand-side of the formula may be:
#'  * A named function, e.g. `is.numeric`, [cli::style_bold]
#'  * An anonymous function, e.g. `\(x) is.nan(x)`, `\(x) ifelse(x == "", "empty", x)`
#'  * A purrr-style lambda expression, e.g. `paste0(.h, "?")`, `"fizzbuzz"`
#'
#'  Additionally, the left-hand-side of the formula may be a scalar atomic object,
#'  supplied by name e.g. `1`, `NaN`, `"Hello"`. This is a shorthand for
#'  a test of equality. For example:
#'  * `10` corresponds to `.x == 10`, `"Word"` to `.x == "Word"`
#'  * `NaN` corresponds to `is.nan(.x)`
#'  * `NA` corresponds to `is.na(.x)`, `NA_int_` to `is.na(.x) & is.integer(.x)`
#'
#'  A one-sided formula (e.g. `~ tolower`) may also be supplied, in which case
#'  every element of `.x` is formatted using the right-hand-side function
#'  (e.g. `tolower`).
#'
#'  Examples of arguments to `...` include:
#'  * Color `NA` values red: `is.na ~ color("red")`
#'  * Add an exclamation mark: `toupper(.x) == .x ~ paste0(.x, "!")`
#'  * Label the number 1 as "Yes": `1 ~ "Yes"`
#'  * Color the background yellow by default: `~ cli::cli_bg_yellow(.x)`
#'
#'  In the case of `highlight_case()`, elements of `.x` can be conditionally
#'  formatted at most once. Each element of `.x` is formatted using the formatter
#'  corresponding to the first test which returns `TRUE` for that element.
#'
#' @return
#'
#' A vector of class `<vlighter_highlight>` for `highlight()` and
#' `highlight_mult()`. For  `highlight_case()` a vector of class
#' `<vlighter_highlight_case/vlighter_highlight>`.
#'
#' @seealso
#'
#' [is_highlightable()] for testing whether an object can be highlighted.
#'
#' [un_highlight()] for converting a vector `highlight(.x)` back to `.x`.
#'
#' [tests()], [formatters()], [highlight_functions()] for setting and
#' getting the values of `.t` (i.e. tests) and `.f` (i.e. formatters) of a
#' highlighted vector.
#'
#' [as_highlighter()] to generate a [highlighter()] function which applies the
#' same conditional formatting as the input highlighted vector.
#'
#' [color()] and friends for generating formatter functions for use in `.f`.
#'
#' @examples
#' # Emphasize NA values when `x_hl` is printed
#' x <- c(1, 0, NA, 1, 0)
#' x_hl <- highlight(x, is.na, ~paste("[", .x, "]"))
#' print(x)
#' print(x_hl)
#'
#' # Add labels to an indicator variable
#' indicator <- highlight_mult(
#'   c(0, 1, NA, 5),
#'   0 ~ label("No"),
#'   1 ~ label("Yes"),
#'   is.na ~ color("red"),
#'   !(.x %in% c(0, 1, NA)) ~ label("?")
#' )
#' print(indicator)
#'
#' # Simplify using `dplyr::case_when()` style case matching.
#' # Elements are conditionally formatted using the first case
#' # where the left-hand-side returns `TRUE`.
#' highlight_case(
#'   c(0, 1, NA, 5),
#'   0 ~ label("No"),
#'   1 ~ label("Yes"),
#'   is.na ~ color("red"),
#'   true ~ label("?") # `true()` is a function which returns `TRUE`
#' )
#'
#' # Make a `highlighter()` to add the formatting of `indicator`
#' # to other vectors.
#' indicator_highlighter <- as_highlighter(indicator)
#' indicator_highlighter(c(1, 0, 1, NA, -9))
#'
#' # Apply multiple formats to the same element
#' highlight_mult(
#'   1:6,
#'   .x %% 2 == 0 ~ wrap("<", ">"),
#'   .x >= 3 ~ wrap("[", "]")
#' )
#'
#' # Apply a formatter to every element of `.x` by
#' # supplying a one-sided formula.
#' upper_letters <- highlight_mult(letters[1:10], ~ toupper)
#' print(upper_letters)
#'
#' # Note that highlighting does not alter the underlying data
#' un_highlight(upper_letters)
#'
#' # A one-sided formula supplied to `highlight_case()` will
#' # format all elements, even if they have already been formatted,
#' # over-riding the default matching behavior.
#' highlight_case(
#'   c(1, 1, 0),
#'   0 ~ "No",
#'   1 ~ "Yes",
#'   ~ toupper # `true ~ toupper` wouldn't format any elements
#' )
#'
#' # By default no formatting is applied to a highlighted vector.
#' highlight(1:5) # No conditional formatting
#'
#' # The default formatter `.f` colors the background of the
#' # formatted vector yellow. If you are reading this is in
#' # an environment which doesn't support ANSI coloring, you
#' # may not see the yellow background.
#' highlight(1:5, ~ .x > 3) # Yellow background
#'
#' # Change the default test or formatter using `options()`
#' opts <- options(vlightr.default_formatter = \(x) paste("{", x, "}"))
#' highlight(-2:2, ~ .x < 0)
#'
#' options(vlightr.default_test = \(x) x > 0)
#' highlight(-2:2)
#'
#' options(opts)
#' @export
highlight <- function(
    .x = logical(),
    .t = getOption("vlightr.default_test"),
    .f = getOption("vlightr.default_formatter")
  ) {
  x <- check_is_highlightable(.x)
  tests <- check_is_list_of_functionish(.t)
  formatters <- check_is_list_of_functionish(.f)
  assert_same_length(tests, formatters, x_name = ".t", y_name = ".f")

  validate_highlight(
    x = new_highlight(x, tests = tests, formatters = formatters),
    x_name = rlang::caller_arg(.x)
  )
}

#' @rdname highlight
#' @export
hl <- highlight

#' @rdname highlight
#' @export
highlight_mult <- function(.x, ...) {
  rlang::check_required(.x)
  x <- check_is_highlightable(.x)
  funs <- prepare_highlight_functions(...)

  validate_highlight(
    x = new_highlight(x, tests = funs$tests, formatters = funs$formatters),
    x_name = rlang::caller_arg(.x)
  )
}

#' @rdname highlight
#' @export
hl_mult <- highlight_mult

#' @rdname highlight
#' @export
highlight_case <- function(.x, ...) {
  rlang::check_required(.x)
  x <- check_is_highlightable(.x)
  funs <- prepare_highlight_functions(...)

  validate_highlight(
    new_highlight(
      x,
      tests = funs$tests,
      formatters = funs$formatters,
      subclass = "vlightr_highlight_case"
    ),
    x_name = rlang::caller_arg(.x)
  )
}

#' @rdname highlight
#' @export
hl_case <- highlight_case

#' Test if the object is a highlight
#'
#' @description
#'
#' This function returns `TRUE` for highlighted vectors (class `vlightr_highlight`)
#' or subclasses thereof, and returns returns `FALSE` otherwise.
#'
#' @param x
#'
#' An object to test.
#'
#' @return
#'
#' `TRUE` if `x` is a `vlightr_highlight`, `FALSE` otherwise.
#'
#' @export
is_highlight <- function(x) {
  inherits(x, "vlightr_highlight")
}

# TODO: Document and export
is_highlight_case <- function(x) {
  inherits(x, "vlightr_highlight_case")
}

# highlighter ------------------------------------------------------------------

new_highlighter <- function(tests, formatters, subclass = character()) {
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
        subclass = highlight_subclass
      ),
      x_name = rlang::caller_arg(.x)
    )
  }
  attr(out, "tests") <- tests
  attr(out, "formatters") <- formatters
  class(out) <- c("vlightr_highlighter", subclass, "function")
  out
}

#' Generate a highlight function with custom default arguments
#'
#' @description
#'
#' Produces a partially applied version of the [highlight()] function, with
#' specified arguments pre-filled. The resulting function can be used to highlight
#' a vector `x` using a default conditional format.
#'
#' Assuming that arguments `x`, `conditions`, and `formatters` are valid, the
#' following are equivalent:
#'  * `highlight(x = x, conditions = conditions, formatters = formatters)`
#'  * `highlighter(conditions = conditions, formatters = formatters)(x)`
#'
#' The output function has the same arguments as `highlight()`. If arguments
#' `conditions`, `formatters`, `description`, or `precedence` are supplied to
#' a highlighter function, then the supplied arguments are appended to the pre-filled
#' arguments prior to highlighting.
#'
#' If arguments `format_once`, `init_formatter`, or `last_formatter` are supplied
#' to a highlighter, then the supplied arguments override the pre-filled defaults.
#'
#' @inheritParams highlight
#'
#' @param ... `[formula]`
#'
#'  This argument replaces the `formatters` and `conditions` arguments of
#'  `highlighter()`. The i-th dot supplied is roughly equivalent to
#'  `conditions[[i]] ~ formatters[[i]]`.
#'
#'  The left-hand-side and right-hand-side of the formula may be a:
#'  * Function, e.g. `rlang::is_string`, `toupper`
#'  * A purrr-style lambda expression, e.g. `TRUE`, `paste(.x, "?")`
#'
#'  The left-hand-side and righ-hand-side of the formula may not be a call to a
#'  generator function (i.e. a function which returns another function). Examples
#'  include [wrap()], [color()], and [bg()]. To use such a function, call it
#'  within a lambda expression instead, e.g. `color("blue")(.x)`.
#'
#'  Examples of arguments to `...` include:
#'  * Colour `NA` values red: `is.na ~ cli::col_red`
#'  * Add an exclamation mark: `toupper(.x) == .x ~ paste0(.x, "!")`
#'  * Replace 1's with 2's: `.x == 1 ~ "2"`
#'  * Colour the background yellow by default: `TRUE ~ bg("yellow")(.x)`
#'
#' @examples
#' # Mimic an existing highlighted vector
#' dummy <- c(1L, NA, 0L, 1L, NA)
#' dummy_hl <- highlight_case(
#'  dummy,
#'  .x == 0 ~ paste0(.x, "[No]"),
#'  .x == 1 ~ paste0(.x, "[Yes]"),
#'  is.na ~ cli::col_red
#' )
#' dummy_hlghtr <- highlighter_case(
#'  .x == 0 ~ paste0(.x, "[No]"),
#'  .x == 1 ~ paste0(.x, "[Yes]"),
#'  is.na ~ cli::col_red
#' )
#' print(dummy_hl)
#' print(dummy_hlghtr(dummy))
#'
#' # Apply a higlighter to a new vector
#' dummy_hlghtr(c(-2L, -1L, 0L, 1L, 2L, NA))
#'
#' # Provide additional arguments to a highlighter
#' dummy_hlghtr(
#'   x = c(1L, NA, 2L, 0L, 1L, NA, 2L),
#'   conditions = ~ .x == 2,
#'   formatters = ~ paste(.x, "[Double Yes]")
#' )
#'
#' # Override default highlighter arguments
#' secret <- highlighter(
#'   ~ grepl("^<.*>$", .x),
#'   ~ strrep("x", nchar(.x) - 2L),
#'   last_formatter = toupper
#' )
#' message <- c("<A>", "Super", "<Secret>", "Message")
#' secret(message)
#' secret(message, last_formatter = tolower)
#' @export
highlighter <- function(.t, .f) {
  rlang::check_required(.t)
  rlang::check_required(.f)
  tests <- check_is_list_of_functionish(.t)
  formatters <- check_is_list_of_functionish(.f)
  assert_same_length(tests, formatters, x_name = ".t", y_name = ".f")

  new_highlighter(tests = tests, formatters = formatters)
}

# TODO: export and document
highlighter_mult <- function(...) {
  funs <- prepare_highlight_functions(...)
  new_highlighter(tests = funs$tests, formatters = funs$formatters)
}

# TODO: export and document
highlighter_case <- function(...) {
  funs <- prepare_highlight_functions(...)
  new_highlighter(
    tests = funs$tests,
    formatters = funs$formatters,
    subclass = "vlightr_highlighter_case"
  )
}

#' Test if the object is a highlighter
#'
#' @description
#'
#' This function returns `TRUE` for highlighter functions (class `vlightr_highlighter`)
#' or subclasses thereof, and returns returns `FALSE` otherwise.
#'
#' @param x
#'
#' An object to test.
#'
#' @return
#'
#' `TRUE` if `x` is a `vlightr_highlighter`, `FALSE` otherwise.
#'
#' @export
is_highlighter <- function(x) {
  inherits(x, "vlightr_highlighter")
}

# TODO: Export and Document
is_highlighter_case <- function(x) {
  inherits(x, "vlightr_highlighter_case")
}

#' Convert a highlighted vector into a highlighter
#'
#' @description
#'
#' Convert a highlighted vector `x` (class `vlightr_highlight`) into a highlighter
#' function (class `vlightr_highligher`) which applies the same conditional
#' formatting as that of `x`.
#'
#' @param x `[vlightr_highlight]`
#'
#' A highlighted vector to convert to a highlighter (class `vlightr_highlighter`)
#' function. See [highlighter()] for details.
#'
#' @return
#'
#' A highlighter function with the same `conditions`, `formatters`, `description`,
#' `precedence`, `format_once`, `init_formatter`, and `last_formatter` as `x`.
#'
#' @examples
#' # Highlight a vector
#' dummy <- highlight_case(
#'  x = c(1L, NA, 0L, 1L, NA),
#'  .x == 0 ~ paste0(.x, "[No]"),
#'  .x == 1 ~ paste0(.x, "[Yes]"),
#'  is.na ~ cli::col_red,
#'  .description = c(
#'     "Responded No",
#'     "Responded Yes",
#'     "No Response"
#'   )
#' )
#' print(dummy)
#' describe_highlight(dummy)
#'
#' # Convert the highlighted vector into a highlighter function
#' dummy_hlghtr <- as_highlighter(dummy)
#' print(dummy_hlghtr(c(0L, 1L, NA, 2L)))
#' describe_highlight(dummy_hlghtr(c(0L, 1L, NA, 2L)))
#' @export
as_highlighter <- function(.x) {
  rlang::check_required(.x)
  x <- check_is_highlight(.x)
  new_highlighter(
    tests = get_tests(x),
    formatters = get_formatters(x),
    subclass = if (is_highlight_case(x)) {
      "vlightr_highlighter_case"
    } else {
      character()
    }
  )
}

# methods ----------------------------------------------------------------------

# TODO: We should not allow the merging of `vlightr_templight` and
# `vlightr_highlight` or, at the very least, we should coerce to
# a `vlightr_templight`.

#' @export
vec_ptype_abbr.vlightr_highlight <- function(x) {
  inner <- vctrs::vec_ptype_abbr(get_data(x))
  paste0("hl<", inner, ">")
}

#' @export
vec_ptype_full.vlightr_highlight <- function(x) {
  inner <- vctrs::vec_ptype_full(get_data(x))
  paste0("highlight<", inner, ">")
}

#' @export
vec_ptype2.vlightr_highlight.vlightr_highlight <- function(x, y, ...) {
  # We want to create a highlight of a size-0 slice of `x`/`y` but:
  #
  # 1. Some (valid) formatters and tests will error on a zero-length vector.
  #   - Ex. the test `~.x == min(.x)` will warn if `.x` is an empty numeric.
  #   - I don't want to prevent the caller from combining two non-empty
  #     vectors `x` and `y` because the highlight will fail in the empty case.
  #
  # 2. Formatters and tests aren't applied to size-0 vectors by `format()`,
  #    meaning that `validate_highlight()` doesn't even check these vectors.
  #
  # To resolve this, we first combine the underlying data via `vctrs::vec_c()`.
  # If `x`, `y` are incompatible, an error will be thrown as in `vec_ptype2()`.
  #
  # Then, we validate the resulting highlight using the combined `x` and `y`
  # data. Only after validation is the resulting highlight sliced to size-0 for
  # the prototype.
  .data <- rlang::try_fetch(
    vctrs::vec_c(get_data(x), get_data(y)),
    vctrs_error_ptype2 = function(cnd) {
      vctrs::stop_incompatible_type(
        x, y,
        x_arg = "",
        y_arg = "",
        call = cnd$call,
        class = c("vlightr_ptype2_error", "vlighter_error")
      )
    }
  )
  out <- restore_highlight(
    .data, x, y,
    .error_message = c(
      "Combination produced a malformed {.cls vlightr_highlight} vector.",
      i = "Combined objects may have incompatible {.fn tests} or {.fn formatters}."
    ),
    .error_class = c("vlightr_ptype2_error", "vlightr_error")
  )
  vctrs::vec_slice(out, 0L)
}

#' @export
vec_cast.vlightr_highlight.vlightr_highlight <- function(x, to, ...) {
  .data <- rlang::try_fetch(
    vctrs::vec_cast(get_data(x), get_data(to)),
    vctrs_error_cast = function(cnd) {
      vctrs::stop_incompatible_cast(
        x, to,
        x_arg = "",
        to_arg = "",
        call = cnd$call,
        class = c("vlightr_cast_error", "vlighter_error")
      )
    }
  )
  restore_highlight(
    .data, to,
    .error_message = "Conversion produced a malformed {.cls vlightr_highlight} vector.",
    .error_class = c("vlightr_cast_error", "vlightr_error"),
    .error_call = rlang::call2("vec_cast")
  )
}

#' @export
#' @method vec_arith vlightr_highlight
vec_arith.vlightr_highlight <- function(op, x, y, ...) {
  UseMethod("vec_arith.vlightr_highlight", y)
}

#' @export
#' @method vec_arith.vlightr_highlight default
vec_arith.vlightr_highlight.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(
    op, x, y,
    class = c("vlightr_arithmatic_error", "vlightr_error")
  )
}

#' @export
#' @method vec_arith.vlightr_highlight vlightr_highlight
vec_arith.vlightr_highlight.vlightr_highlight <- function(op, x, y, ...) {
  # Re-raise `stop_incompatible_op` on an error, so that the `ptype` of the
  # highlights, and not their data, is shown in the error message.
  .data <- rlang::try_fetch(
    vctrs::vec_arith(op, get_data(x), get_data(y), ...),
    vctrs_error_incompatible_op = function(cnd) {
      vctrs::stop_incompatible_op(
        op, x, y,
        call = rlang::call2("vec_arith"),
        class = c("vlightr_arithmatic_error", "vlightr_error")
      )
    }
  )

  restore_highlight(
    .data, x, y,
    .error_message = c(
      paste0("Binary call to {.code ", op, "} produced a malformed {.cls vlightr_highlight} vector."),
      i = paste0("Arguments to {.code ", op, "} may have incompatible {.val formatters} or {.val tests} attributes.")
    ),
    .error_call = rlang::call2("vec_arith"),
    .error_class = "vlightr_arithmatic_error"
  )
}

#' @export
#' @method vec_arith.vlightr_highlight MISSING
vec_arith.vlightr_highlight.MISSING <- function(op, x, y, ...) {
  # vctrs::vec_arith(op, get_data(x), MISSING()) doesn't allow operations "+"
  # or "-" for numeric vectors. Instead, performing the operation manually on the
  # underlying data and re-throwing any errors, assuming it's an incompatible op.
  op_call <- rlang::call2(op, get_data(x))
  .data <- rlang::try_fetch(
    eval(op_call),
    error = function(cnd) {
      cli::cli_abort(
        "<{vctrs::vec_ptype_full(x)}> {op} <{vctrs::vec_ptype_full(y)}> is not permitted.",
        parent = cnd,
        class = "vlightr_arithmatic_error",
        call = rlang::call2("vec_arith")
      )
    }
  )
  restore_highlight(
    .data, x,
    .error_message = paste0(
      "Unary call to {.code ", op, "} produced a malformed {.cls vlightr_highlight} vector."
    ),
    .error_call = rlang::call2("vec_arith"),
    .error_class = "vlightr_arithmatic_error"
  )
}

#' @export
vec_math.vlightr_highlight <- function(.fn, x, ...) {
  .data <- rlang::try_fetch(
    vctrs::vec_math(.fn, get_data(x), ...),
    # `vctrs::vec_math` isn't implemented for all base types (i.e. integers), so
    # falling back to base if necessary. This does allow for weird coercions like
    # <chr> to <dbl> that wouldn't normally be allowed.
    vctrs_error_unimplemented = function(cnd) {
      vctrs::vec_math_base(.fn, get_data(x), ...)
    },
    error = function(cnd) {
      cli::cli_abort(
        paste0("Can't evaluate call with input of type <", vctrs::vec_ptype_full(x), ">."),
        parent = cnd,
        class = c("vlightr_math_error", "vlightr_error"),
        call = rlang::call2(.fn)
      )
    }
  )

  restore_highlight(
    .data, x,
    .error_message = "Call produced a malformed {.cls vlightr_highlight} vector.",
    .error_call = rlang::call2(.fn),
    .error_class = "vlightr_math_error"
  )
}

restore_highlight <- function(
    x,
    ...,
    .x_name = rlang::caller_arg(x),
    .error_call = rlang::caller_env(),
    .error_class = NULL,
    .error_message = NULL
  ) {

  x <- check_is_highlightable(
    arg = x,
    arg_name = .x_name,
    error_call = .error_call,
    error_class = .error_class
  )

  highlights <- rlang::list2(...)
  validate_highlight(
    new_highlight(
      x = x,
      t = vctrs::list_unchop(map(highlights, get_tests)),
      f = vctrs::list_unchop(map(highlights, get_formatters)),
    ),
    x_name = .x_name,
    error_call = .error_call,
    error_class = c(.error_class, "vlightr_error"),
    error_message = .error_message
  )
}

# highlight helpers ------------------------------------------------------------

get_data <- function(x) {
  vctrs::field(x, ".data")
}

get_tests <- function(x) {
  attr(x, "tests")
}

get_formatters <- function(x) {
  attr(x, "formatters")
}

get_highlight_functions <- function(x) {
  mapply(
    attr(x, "tests"),
    attr(x, "formatters"),
    FUN = \(test, formatter) list(test = test, formatter = formatter),
    SIMPLIFY = FALSE
  )
}

prepare_highlight_functions <- function(...) {

  error_call <- rlang::caller_env()
  dots <- rlang::list2(...)
  formulas_at <- map_lgl(dots, is_formula)
  highlighters_at <- map_lgl(dots, is_highlighter)

  if (!all(formulas_at | highlighters_at)) {
    invalid_at <- which.min(formulas_at | highlighters_at)
    cli::cli_abort(
      c(
        "Arguments to `...` must be a formula or {.cls vlightr_highlighter} function.",
        x = "`..{invalid_at}` is {.obj_type_friendly {dots[invalid_at]}}."
      ),
      call = error_call,
      class = "vlighter_error"
    )
  }

  out <- vector("list", length(dots))
  out[formulas_at] <- map(dots[formulas_at], split_highlight_formula)
  out[highlighters_at] <- map(dots[highlighters_at], get_highlight_functions)

  # `get_highlight_functions()` returns a list of lists, which are of the form
  # `list(test = test, formatter = formatter)`. Thus, `out[highlighters_at]` is
  # a list of lists of lists - which we flatten into a list of lists here. We do
  # this after assigning to `out[highlighters_at]` so that we maintain the order
  # of the formulas and highlighters supplied to `...` by the caller.
  if (any(highlighters_at)) {
    out <- vctrs::list_unchop(out)
  }
  list(
    tests = map(out, `[[`, "test"),
    formatters = map(out, `[[`, "formatter")
  )
}

split_highlight_formula <- function(frm) {
  if (is_one_sided_formula(frm)) {
    test <- format_all
    formatter <- new_formatter_function(rlang::f_rhs(frm), rlang::f_env(frm))
  }
  else {
    env <- rlang::f_env(frm)
    test <- new_test_function(rlang::f_lhs(frm), env)
    formatter <- new_formatter_function(rlang::f_rhs(frm), env)
  }
  list(test = test, formatter = formatter)
}

new_formatter_function <- function(expr, env) {
  expr_to_function(expr, env)
}

new_test_function <- function(expr, env) {
  if (rlang::is_syntactic_literal(expr)) {
    literal_to_function(expr)
  } else {
    expr_to_function(expr, env)
  }
}

expr_to_function <- function(expr, env) {
  evaluated <- try_eval(expr, env)
  if (is.function(evaluated)) {
    return(evaluated)
  }
  args <- list(
    ... = rlang::missing_arg(),
    .x = quote(..1),
    . = quote(..1),
    .h = quote(..1)
  )
  rlang::new_function(
    args = args,
    body = expr,
    env = env
  )
}

literal_to_function <- function(literal) {
  if (is.nan(literal)) {
    expr <- quote(base::is.nan(.x))
    return(expr_to_function(expr, env = rlang::global_env()))
  }
  if (is.na(literal)) {
    expr <- switch(
      typeof(literal),
      character = quote(base::is.na(.x) & base::is.character(x)),
      integer = quote(base::is.na(.x) & base::is.integer(x)),
      complex = quote(base::is.na(.x) & base::is.complex(x)),
      double = quote(base::is.na(.x) & base::is.numeric(x)),
      quote(base::is.na(.x))
    )
    return(expr_to_function(expr, env = rlang::global_env()))
  }
  function(x) x == literal
}

try_eval <- function(expr, env) {
  rlang::try_fetch(eval(expr, env), error = function(cnd) NULL)
}

# When this is used as the `test` function for a highlighted vector, it's
# `format()` method will apply the corresponding `formatter` function to every
# element of the highlighted vector (even when it wouldn't otherwise, e.g. to
# already formatted elements in a <vlightr_highlight_case> vector). Users can
# create such a test by supplying a one-sided formula to `highlight_case(...)`
# or `highlight_mult(...)`.
format_all <- function(x) {
  TRUE
}


