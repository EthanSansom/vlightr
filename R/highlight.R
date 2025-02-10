# todo -------------------------------------------------------------------------

#### Fix tibble() formatting:
#
# You'll probably want to use `pillar()` directly. Take a look at what
# `haven::labelled` does in `pillar()`. I particularly want to support
# nicer formatting for simply highlighted numbers in a tibble. Calling
# `tibble(x = hl(1:10, ~ x > 5, color("yellow"))` should look nice by
# default.

#### Attempt to intercept bad expressions supplied to `.f` and `.t`.
#
# If you forget the `~` then you get a cryptic error message.
# `.x > 10` instead of `~ .x > 10`, I think it's an easy mistake to make.

#### Attributes handling
#
# Change the `attr()` method to attach attributes to the underlying data,
# this way we let {vctrs} handle their maintenance.

# highlight --------------------------------------------------------------------

new_highlight <- function(x, tests, formatters, subclass = character()) {
  if (!rlang::is_empty(subclass)) {
    assert_arg_match_internal(
      subclass,
      values = c("vlightr_templight", "vlightr_highlight_case"),
      subset = TRUE
    )
  }
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
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = character(),
    error_message = NULL
) {
  if (!is_highlight(x)) {
    stop_internal("`x` must be a {.cls vlightr_highlight} vector.")
  }

  # Set the context for newly created highlights within `dplyr::across()` so we
  # can add an `across_error_hint()` for common failure cases. If `format()` is
  # successful we return `x`.
  context$in_across <- isTRUE(in_across())
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
        class = c(error_class, "vlightr_error")
      )
    }
  )
}

#' Conditionally format a vector
#'
#' @description `highlight()` creates a vector with a conditional [format()]
#' and [print()] method. The function takes an input vector `.x`, a test
#' function `.t`, and a formatter function `.f`. When the result of
#' `highlight(.x, .t, .f)` is printed, elements of `.x` for which `.t` returns
#' `TRUE` are transformed by `.f` before they are printed.
#'
#' `.t` and `.f` may be equal length lists of functions. Elements of `.x` for
#' which `.t[[i]]` returns true are transformed using `.f[[i]]`. Conditional
#' formats are applied to the highlighted vector in the order that they are
#' applied to `.t` and `.f`.
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
#'  By default `.x` is an empty logical vector.
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
#'  By default `.t` is the function `false()` which returns `FALSE` for any
#'  input. You can modify this default by setting the `vlightr.default_test`
#'  in [`options()`].
#'
#' @param .f `[function / list]`
#'
#'  Vectorized character manipulation functions used to format `.x`. `.f` may be:
#'  * A named function, e.g. [cli::style_bold]
#'  * An anonymous function, e.g. `\(words) gsub("hi", "hey", words)`
#'  * A purrr-style lambda, e.g. `~ paste0(.h, "!")`, `~ "fizz"`
#'  * A list of functions or lambdas, e.g. `list(~ cli::col_red(.x), toupper)`
#'
#'  Each formatter function in `.f` will receive a character vector (of variable length)
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
#' @param ... `[formula / vlightr_highlighter]`
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
#'  Finally, a highlighter, e.g. `highlighter(is.na, color("red"))`, may be
#'  supplied instead of a formula. Every test (e.g. `.t`) and formatter
#'  (e.g. `.f`) associated with the highlighter is inserted as a test
#'  and formatter of the returned vector.
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
#' A vector of class `vlightr_highlight`. For `highlight_case()`, the a
#' vector of class `vlightr_highlight/vlightr_highlight_case`.
#'
#' @seealso
#'
#' [templight()] for conditionally formatting elements of a vector `.x`
#' by location. Replaces the test `.t` with a vector of positions `.at`.
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
#' # Emphasize NA values when `x_hl` is printed
#' x <- c(1, 0, NA, 1, 0)
#' x_hl <- highlight(x, is.na, ~paste("[", .x, "]"))
#' print(x)
#' print(x_hl)
#'
#' # Track the maximum of `values`
#' values <- highlight(c(1, 5, 7, 3), ~ .x == max(.x), wrap("[", "]"))
#' print(values)
#' print(sort(values))
#' print(hl(-1) * values)
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
#' indicator <- highlight_case(
#'   c(0, 1, NA, 5),
#'   0 ~ label("No"),
#'   1 ~ label("Yes"),
#'   is.na ~ color("red"),
#'   true ~ label("?") # `true()` is a function which returns `TRUE`
#' )
#' print(indicator)
#'
#' # Make a `highlighter()` to add the formatting of `indicator`
#' # to other vectors.
#' indicator_highlighter <- as_highlighter(indicator)
#' indicator_highlighter(c(1, 0, 1, NA, -9))
#'
#' # A highlighter can be supplied to `highlight_mult()` or
#' # `highlight_case()`. This is an easy way to append new
#' # options to an existing highlighter.
#' highlight_case(
#'   c(1, 2, 0, NA, -9),
#'   2 ~ label("Maybe"),
#'   indicator_highlighter
#' )
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
#' opts <- options() # Save previous options
#' options(vlightr.default_formatter = \(x) paste("{", x, "}"))
#' highlight(-2:2, ~ .x < 0)
#'
#' options(vlightr.default_test = \(x) x > 0)
#' highlight(-2:2)
#' options(opts) # Reset previous options
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
    x = new_highlight(x, tests = funs$test, formatters = funs$formatter),
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
      tests = funs$test,
      formatters = funs$formatter,
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
#' @param x An object to test.
#' @return `TRUE` if `x` is a `vlightr_highlight`, `FALSE` otherwise.
#'
#' @examples
#' # The following are `FALSE`
#' is_highlight(10)
#'
#' # The following are `TRUE`
#' is_highlight(highlight(10))
#' is_highlight(templight(10))
#' is_highlight(highlight_case(10))
#'
#' @export
is_highlight <- function(x) {
  inherits(x, "vlightr_highlight")
}

#' Test if the object is a highlight-case vector
#'
#' @description
#'
#' This function returns `TRUE` for vectors highlighted using `highlight_case()`
#' or `templight_case()` (class `vlightr_highlight_case`), and returns
#' `FALSE` otherwise.
#'
#' @param x An object to test.
#' @return `TRUE` if `x` is a `vlightr_highlight_case`, `FALSE` otherwise.
#'
#' @examples
#' # The following are `FALSE`
#' is_highlight_case(10)
#' is_highlight_case(highlight(10))
#'
#' # The following are `TRUE`
#' is_highlight_case(highlight_case(10))
#' is_highlight_case(templight_case(10))
#'
#' # Vectors highlighted using `highlighter_case()` or
#' # `templigher_case()` highlighter are class `vlightr_highlight_case`
#' lighter <- highlighter_case()
#' is_highlight_case(lighter(10))
#'
#' @export
is_highlight_case <- function(x) {
  inherits(x, "vlightr_highlight_case")
}

# highlighter ------------------------------------------------------------------

new_highlighter <- function(tests, formatters, subclass = character()) {
  if (!rlang::is_empty(subclass)) {
    assert_arg_match_internal(subclass, "vlightr_highlighter_case")
  }
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
  class(out) <- c(subclass, "vlightr_highlighter", "function")
  out
}

#' Generate a re-usable highlight function
#'
#' @description
#'
#' Generates a partially applied version of the [highlight()] function, with
#' pre-supplied tests `.t` and formatters `.f`.
#'
#' The following calls produce equivalent highlighted vectors:
#'  * `highlight(.x = .x, .t = .t, .f = .f)`
#'  * `highlighter(.t = .t, .f = .f)(.x = .x)`
#'
#' This is useful for creating and storing a re-usable conditional format
#' to apply to vectors.
#'
#' `highlighter_mult()` and `highlighter_case()` (corresponding to `highlight_mult()`
#' and `hightlight_case()`) allow `.t` and `.f` to be supplied as two-sided
#' formulas of the form `.t ~ .f`.
#'
#' @param .t `[function / list]`
#'
#'  Vectorized test functions specified as:
#'  * A named function, e.g. `is.na`
#'  * An anonymous function, e.g. `\(x) 0 <= x & x <= 1`
#'  * A purrr-style lambda, e.g. `~ nchar(.x) > 0`, `~ .h == 1`, `~ TRUE`
#'  * A list of functions or lambdas, e.g. `list(~ .x < mean(.x), is.finite)`
#'
#'  In the generated function, each function in `.t` will receive a vector `.x`
#'  as it's input and must return a logical vector the same length as `.x` or of
#'  length 1 (in which case the result will be recycled to the length of `.x`).
#'
#' @param .f `[function / list]`
#'
#'  Vectorized character manipulation functions specified as:
#'  * A named function, e.g. [cli::style_bold]
#'  * An anonymous function, e.g. `\(words) gsub("hi", "hey", words)`
#'  * A purrr-style lambda, e.g. `~ paste0(.h, "!")`, `~ "fizz"`
#'  * A list of functions or lambdas, e.g. `list(~ cli::col_red(.x), toupper)`
#'
#'  In the generated function, each formatter function in `.f` will receive a
#'  character vector (of variable length) as it's only argument. A formatter must
#'  return a character vector the same length as it's input or of length 1 (in
#'  which case the result is recycled to the length of the input character).
#'
#' @param ... `[formula / vlightr_highlighter]`
#'
#'  For `highlighter_mult()` and `highlighter_case()`, a two sided formula with
#'  a test on the left-hand-side and a formatter on the right-hand-side. This
#'  argument replaces the `.t` and `.f` arguments of `highlighter()`. The ith dot
#'  `..i` is roughly equivalent to `.t[[i]] ~ .f[[i]]`.
#'
#'  See the `...` argument of [highlight()] for more details on valid arguments
#'  to supply to `...`.
#'
#' @return
#'
#' A function of class `vlightr_highlighter`. For `highlighter_case()`, a
#' function of class `vlightr_highlighter/vlightr_highlighter_case`.
#'
#' @examples
#' # Mimic an existing highlighted vector
#' indicator <- c(1, 0, 1, 0)
#' indicator_hl <- highlight_mult(
#'  indicator,
#'  0 ~ label("No"),
#'  1 ~ label("Yes")
#' )
#' indicator_hltr <- highlighter_mult(
#'  0 ~ label("No"),
#'  1 ~ label("Yes")
#' )
#'
#' # These print the same result
#' print(indicator_hl)
#' print(indicator_hltr(indicator))
#'
#' # You can add functionality to an existing highlighter by
#' # providing it as an argument to `highlighter_mult()`.
#' new_indicator_hltr <- highlighter_mult(
#'   indicator_hltr,
#'   5 ~ "Maybe",
#'   is.na ~ "?"
#' )
#' x <- c(0, 1, NA, 5)
#' indicator_hltr(x) # NA and 5 are un-formatted
#' new_indicator_hltr(x) # NA and 5 are formatted
#'
#' # This is useful for composing highlighters
#' exclaim <- highlighter(~ .x == toupper(.x), ~ paste0(.x, "!"))
#' question <- highlighter(~ .x == tolower(.x), ~ paste0(.x, "?"))
#' punctuate <- highlighter_mult(exclaim, question)
#'
#' # `punctuate()` applies the formatting of `exclaim()` and `question()`
#' phrases <- c("hi all", "FANTASTIC", "I'm Dave")
#' exclaim(phrases)
#' question(phrases)
#' punctuate(phrases)
#'
#' # `highlighter_case()` uses the same matching behavior as
#' # `highlight_case()`.
#' fullstop <- highlighter(~ TRUE, ~ paste0(.x, "."))
#' punctuate_mult <- highlighter_mult(punctuate, fullstop)
#' punctuate_case <- highlighter_case(punctuate, fullstop)
#'
#' # A period is added to every phrase, since the `fullstop()`
#' # test always returns `TRUE`
#' punctuate_mult(phrases)
#'
#' # A period is only added to elements of phrase not already
#' # matched by a previous test.
#' punctuate_case(phrases)
#'
#' @export
highlighter <- function(.t, .f) {
  rlang::check_required(.t)
  rlang::check_required(.f)
  tests <- check_is_list_of_functionish(.t)
  formatters <- check_is_list_of_functionish(.f)
  assert_same_length(tests, formatters, x_name = ".t", y_name = ".f")

  new_highlighter(tests = tests, formatters = formatters)
}

#' @rdname highlighter
#' @export
highlighter_mult <- function(...) {
  funs <- prepare_highlight_functions(...)
  new_highlighter(tests = funs$test, formatters = funs$formatter)
}

#' @rdname highlighter
#' @export
highlighter_case <- function(...) {
  funs <- prepare_highlight_functions(...)
  new_highlighter(
    tests = funs$test,
    formatters = funs$formatter,
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

#' @rdname is_highlighter
#' @export
is_highlighter_case <- function(x) {
  inherits(x, "vlightr_highlighter_case")
}

# TODO: Document!!!

#' Convert a highlighted vector into a highlighter
#'
#' @description
#'
#' Convert a highlighted vector `x` (class `vlightr_highlight`) into a highlighter
#' function (class `vlightr_highligher`) which applies the same conditional
#' formatting as that of `x`.
#'
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
vec_ptype_abbr.vlightr_highlight_case <- function(x) {
  inner <- vctrs::vec_ptype_abbr(get_data(x))
  paste0("hlc<", inner, ">")
}

#' @export
vec_ptype_full.vlightr_highlight_case <- function(x) {
  inner <- vctrs::vec_ptype_full(get_data(x))
  paste0("highlight_case<", inner, ">")
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
        class = c("vlightr_ptype2_error", "vlightr_error")
      )
    }
  )
  out <- restore_highlight(
    .data, x, y,
    .error_message = c(
      "Combination produced a malformed {.cls vlightr_highlight} vector.",
      i = "Combined objects may have incompatible {.fn tests} or {.fn formatters}."
    ),
    .error_class = "vlightr_ptype2_error"
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
        class = c("vlightr_cast_error", "vlightr_error")
      )
    }
  )
  restore_highlight(
    .data, to,
    .error_message = "Conversion produced a malformed {.cls vlightr_highlight} vector.",
    .error_class = "vlightr_cast_error",
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

# restore highlight ------------------------------------------------------------

# This is the work-horse underlying all of the {vctrs} methods for highlights.
# We allow {vctrs} to take care of coercion and casting for the data contained
# in a highlight and `restore_highlight()` to control coercion and casting of
# the highlights themselves.
#
# This includes:
# - coercing `highlight` -> `highlight_case` (`templight` -> `templight_case`)
# - coercing `highlight` -> `templight` (`highlight_case` -> `templight_case`)
#
# - combining highlighter functions:
#   - `tests(c(x, y))` is roughly `unique(c(tests(x), tests(y)))`
#   - `formatters(c(x, y))` is roughly `unique(c(formatters(x), formatters(y)))`
#   - the same is true for arithmetic (e.g. `x + y`)
#
# The class options currently look like this. All objects have base class
# `vlightr_highlight` and any case highlights inherit `vlightr_highlight_case`.
#
# - templights: c(templight, [highlight_case], highlight, vctrs_rcrd)
# - highlights: c([highlight_case], highlight, vctrs_rcrd)

# TODO: Explore maintaining attributes. `c()` strips them and `+` takes the first.
# x <- c(1, 2, 3)
# y <- c(4, 5, 6)
# attr(x, "labels") <- "hi"
# attr(y, "labels") <- "you"
#
# c(x, y)
# x + y

restore_highlight <- function(
    .x,
    ...,
    .x_name = rlang::caller_arg(x),
    .error_call = rlang::caller_env(),
    .error_class = character(),
    .error_message = NULL
  ) {

  x <- check_is_highlightable(
    x = .x,
    x_name = .x_name,
    error_call = .error_call,
    error_class = .error_class
  )

  # `unique()` prevents duplicating the tests and formatters of a highlight
  # combined with itself (e.g. via `c(x, x)` or `x + x`).
  highlights <- unique(rlang::list2(...))

  # This does the work of coercing any combined <highlight> and <highlight_case>
  # vectors into a <highlight_case>, which has stricter rules. Likewise, we
  # subclass with <templight> if any are present (i.e. the order below matters!)
  subclass <- character()
  if (any(map_lgl(highlights, is_highlight_case))) {
    subclass <- c("vlightr_highlight_case", subclass)
  }
  if (any(map_lgl(highlights, is_templight))) {
    subclass <- c("vlightr_templight", subclass)
  }

  # Prepares a list of the form `list(test = tests, formatter = formatters)`
  #
  # `unique()` here prevents the duplication of (test, formatter) combinations
  # in the combined highlights. Duplicated tests (formatters) with distinct
  # formatters (tests) are permitted.
  funs <-
    highlights |>
    map(get_highlight_functions) |>
    vctrs::list_unchop() |>
    unique() |>
    unzip()

  validate_highlight(
    new_highlight(
      x = x,
      tests = funs$test,
      formatters = funs$formatter,
      subclass = subclass
    ),
    x_name = .x_name,
    error_call = .error_call,
    error_class = .error_class,
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
  zip(test = attr(x, "tests"), formatter = attr(x, "formatters"))
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
      class = "vlightr_error"
    )
  }

  # Each highlighter may contain multiples pairs of tests and formatters which
  # need to be split apart without altering the order of user-supplied `...`.
  if (any(highlighters_at)) {
    # The second `map(map())` makes all elements of `out` a list of lists, which
    # we flatten uniformly with `vctrs::list_unchop()` so that every element of
    # `out` is a list.
    out <- vector("list", length(dots))
    out[formulas_at] <- map(map(dots[formulas_at], split_highlight_formula), list)
    out[highlighters_at] <- map(dots[highlighters_at], get_highlight_functions)
    out <- vctrs::list_unchop(out)
  }
  else {
    out <- map(dots, split_highlight_formula)
  }
  unzip(out)
}

# TODO: This should be responsible for wrapping the `test` and `formatter` function
# in some kind of class, so we can control their formatting!
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
format_all <- structure(
  function(x) { TRUE },
  class = c("vlightr_format_all", "function")
)
