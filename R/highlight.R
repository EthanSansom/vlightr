# todo -------------------------------------------------------------------------

#### Transition to github TODOS and use that instead if it makes sense

#### Fix tibble() formatting:
#
# You'll probably want to use `pillar()` directly. Take a look at what
# `haven::labelled` does in `pillar()`. I particularly want to support
# nicer formatting for simply highlighted numbers in a tibble. Calling
# `tibble(x = hl(1:10, ~ x > 5, color("yellow"))` should look nice by
# default.

#### Split up this script. It's too long. Do just highlight and it's methods!

# highlight --------------------------------------------------------------------

#' Conditionally format a vector
#'
#' @description `highlight()` creates a vector with a conditional [format()]
#' method. The highlighted vector `highlight(1:5)` can (with limited legwork) be
#' treated exactly the same as the integer vector `1:5`, but has a different
#' `format()` and `print()` method.
#'
#' `highlight_case()` provides an alternative [dplyr::case_when()] style syntax
#' to `highlight()`. Arguments (other than `x`) to `highlight_case()` are prefixed
#' with a dot.
#'
#' `hl()` and `highlight()` are synonyms, as are `hl_case()` and
#' `highlight_case()`.
#'
#' @details
#'
#' The `highlighter_case()` formula syntax is not compatible with
#' `dplyr::across()` in-lined formulas. In particular, in the call below:
#'
#' ```
#'  across(
#'    y,
#'    ~highlight_case(.x, is.na(.x) ~ paste0('<', .x, '>'))
#'  )
#' ```
#'
#' The expression `is.na(.x) ~ paste0('<', .x, '>')` will be replaced with
#' `is.na(y) ~ paste0('<', y, '>')` by `across()`. Because vlightr expects
#' the case formula to be in terms of `.x` (and not `y`), this creates an invalid
#' condition and formatter function within the highlighted vector (column) `y`.
#'
#' To avoid this behavior, either use functions (e.g. `\(x) is.na(x)`) in
#' the `...` argument of `highlight_case()`, or use a lambda function instead
#' of a formula within `across()`, like so:
#'
#' ```
#'  across(
#'    y,
#'    \(col) highlight_case(col, is.na(.x) ~ paste0('<', .x, '>'))
#'  )
#' ```
#'
#' Note that this is not an issue within other `dplyr` verbs, such as
#' `dplyr::summarize()` or `dplyr::mutate()`. The following will work as
#' expected:
#'
#' ```
#'  mutate(
#'    y = highlight_case(y, is.na(.x) ~ paste0('<', .x, '>'))
#'  )
#' ```
#'
#' @param x `[vector]`
#'
#'  A vector to highlight. Conceptually, a vector is a collection of objects of
#'  size 1.
#'
#'  `x` is considered a vector if:
#'  * `x` is not a [data.frame]
#'  * `x` is not a bare [list]
#'  * `x` is otherwise a vector, as defined by [vctrs::obj_is_vector()]
#'
#'  Atomic vector types `"logical"`, `"integer"`, `"double"`, `"complex"`,
#'  `"character"`, and `"raw"` meet these criteria. As do many common vector
#'  classes such as `POSIXct`, `lubridate::interval`, or `ivs::iv`.
#'
#' @param conditions `[function / list]`
#'
#'  Functions that indicate which elements of `x` to format. Can be a:
#'  * Function, e.g. `is.na`
#'  * A purrr-style lambda, e.g. `~ nchar(.x) > 0`, `~ TRUE`
#'  * A list of functions or lambdas, e.g. `list(~ .x < mean(.x), is.infinite)`
#'
#'  Each function in `conditions` will receive `x` as it's input and must return
#'  a logical vector the same length as `x` or of length 1 (in which case the
#'  result will be recycled to the length of `x`).
#'
#'  Elements of `x` for which `conditions[[i]](x)` is `TRUE` are formatted using
#'  the formatter function `formatters[[i]]`. If both `conditions` and `formatters`
#'  are supplied, then they must be the same length.
#'
#' @param formatters `[function / list]`
#'
#'  Character manipulation functions used to format `x`. Can be a:
#'  * Function, e.g. [cli::style_bold]
#'  * A purrr-style lambda, e.g. `~ paste0(.x, "!")`, `~ "Hi"`
#'  * A list of functions or lambdas, e.g. `list(~ cli::col_red, toupper)`
#'
#'  When called, a function in `formatters` will receive a single character vector
#'  (of variable length) as it's only argument. A formatter must return a character
#'  vector the same length as it's input or of length 1 (in which case the result
#'  is recycled to the length of the input character).
#'
#'  ANSI string vectors (class `cli_ansi_string`) are also supported (see
#'  [cli::ansi-styles] for details).
#'
#'  When `formatters` is not supplied a default set of formatters are provided.
#'  By default, the formatters are a list of `cli::bg_br_yellow`, `cli::bg_br_green`,
#'  `cli::bg_br_red`, `cli::bg_br_cyan`, `cli::bg_br_magenta`, and `cli::bg_br_blue`,
#'  repeated to the length of `conditions`.
#'
#'  If the option `vlightr.colorful_default_formatters` is set via [options()] to
#'  anything other than `TRUE`, then the default `formatters` will append numbered
#'  annotations `"[1]"`, `"[2]"`, etc. to conditionally formatted elements instead
#'  of coloring the text background.
#'
#' @param ... `[formula]`
#'
#'  For `highlighter_case()`, a two sided formula with a condition on the
#'  left-hand-side and a formatter on the right-hand-side.
#'
#'  This argument replaces the `formatters` and `conditions` arguments of
#'  `highlight()`. The i-th dot supplied is roughly equivalent to
#'  `conditions[[i]] ~ formatters[[i]]`.
#'
#'  The left-hand-side and right-hand-side of the formula may be a:
#'  * Function, e.g. `rlang::is_string`, `toupper`
#'  * A purrr-style lambda expression, e.g. `TRUE`, `paste(.x, "?")`
#'
#'  The left-hand-side and right-hand-side of the formula may not be a call to a
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
#' @param description,.description `[character / NULL]`
#'
#'  An optional description of the conditional format applied by each function
#'  in `formatters`. This information is used by [describe_highlight()].
#'
#'  If supplied, `description` must be the same length as `conditions`.
#'
#' @param precedence,.precedence `[numeric / NULL]`
#'
#'  A numeric vector indicating the order in which to apply the `formatters`. The
#'  formatter with the lowest corresponding `precedence` value is evaluated first
#'  during formatting. By default `formatters` are applied in the order in which
#'  they were supplied.
#'
#'  If supplied, `precedence` must be the same length as `conditions`.
#'
#' @param format_once,.format_once `[logical(1)]`
#'
#'  A `TRUE` or `FALSE` value. Indicates whether an element of `x` which meets
#'  multiple `conditions` should be formatted only once (using the formatter
#'  corresponding to the first condition met) or formatted multiple times (using
#'  all of the corresponding `formatters`).
#'
#'  `format_once` is `FALSE` by default in `highlight()` and `TRUE` by default in
#'  `highlight_case()`.
#'
#' @param init_formatter,.init_formatter `[function / NULL]`
#'
#'  The first function used to format `x`. When the highlighted vector is formatted,
#'  `init_formatter(x)` is called prior to conditionally formatting `x` (i.e. before
#'  any of the `formatters` are called.
#'
#'  If `NULL`, then `format(x)` is called instead.
#'
#' @param last_formatter,.last_formatter `[function / NULL]`
#'
#'  The last function called to format `x`. The `last_formatter` is applied
#'  after `x` has been conditionally formatted (i.e. after the `init_formatter`
#'  and `formatters` functions have been called).
#'
#'  `last_formatter` will receive a character vector the same length as
#'  `x` as it's only argument. If `NULL`, the conditionally formatted `x`
#'  is returned as is after formatting.
#' @return
#'
#' A highlighted vector (class `vlightr_highlight`) containing the same data as
#' `x`.
#'
#' @seealso
#'
#' [un_highlight()] for converting a vector `highlight(x)` back to `x`.
#'
#' [update_highlight()] for changing any of `conditions`, `formatters`,
#' `descripton`, `precedence`, `format_once`, `init_formatter`, or
#' `last_formater` in an already highlighted vector.
#'
#' [as_highlighter()] to generate a [highlighter()] function which applies the
#' same conditional formatting as the input highlighted vector.
#'
#' @examples
#' # Color NA values red
#' x <- c(1, 0, NA, 1, 0)
#' x_hl <- highlight(x, is.na, color("red"))
#' print(x)
#' print(x_hl)
#' describe_highlight(x_hl)
#'
#' # Label indicators 1 and 0 by adding highlights to `x_hl`
#' x_hl <- highlight(
#'   x_hl,
#'   conditions = list(is.na, ~ .x == 1, ~ .x == 0),
#'   formatters = list(color("red"), ~ paste(.x, "[Yes]"), ~ paste(.x, "[No]"))
#' )
#' print(x_hl)
#' describe_highlight(x_hl)
#'
#' # Using `dplyr::case_when` style syntax.
#' x_hl_case <- highlight_case(
#'   x,
#'   is.na(.x) ~ cli::col_red,
#'   .x == 1 ~ paste(.x, "[Yes]"),
#'   .x == 0 ~ paste(.x, "[No]"),
#'   .description = c(
#'     "Colored Red if NA",
#'     "Labelled Yes if 1",
#'     "Labelled No if 0"
#'   )
#' )
#' print(x_hl_case)
#' describe_highlight(x_hl_case)
#'
#' # Make a `highlighter` to apply the format of `x_hl_case`
#' indicator_highlighter <- as_highlighter(x_hl_case)
#' indicator_highlighter(c(1, 0, 1, NA, 0, 0))
#'
#' # Apply multiple formats to the same element
#' x_multi <- highlight(
#'   x = 1:6,
#'   conditions = list(~ .x %% 2 == 0, ~ .x > 3),
#'   formatters = list(wrap("<", ">"), wrap("[", "]"))
#' )
#' print(x_multi)
#'
#' # Apply a single format to each element with `format_once`
#' update_highlight(x_multi, format_once = TRUE)
#'
#' # Use an `init_formatter` to pre-format a highlight
#' dollar <- highlight_case(
#'   x = c(10, -1.45, 1.046, -8, NA),
#'   is.na(.x) ~ \(x) "NA",
#'   .x > 0 ~ \(x) paste0("-$", x),
#'   TRUE ~ \(x) paste0("$", x),
#'   .init_formatter = \(x) sprintf("%.2f", abs(x))
#' )
#' print(dollar)
#'
#' # Default `formatters`
#' highlight(1:6, conditions = ~ .x %% 2 == 0)
#' highlight(-2:2, conditions = list(~ .x > 0, ~ .x < 0))
#'
#' # Change the default `formatters` using `options()`
#' opts <- options(vlightr.colorful_default_formatters = FALSE)
#' highlight(-2:2, conditions = list(~ .x > 0, ~ .x < 0))
#' options(opts)
NULL

# TODO: Update documentation as you go -> for highlight, templight, etc.

# TODO: Consider adding a `...` extension to `highlight()` and other simple
# friends which is reserved for extensions.

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
  # can add an `across_error_hint()` for common failure cases.
  context$in_across <- in_across()

  rlang::try_fetch(
    format(x, .x_name = x_name),
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

  x
}

# highlight helpers ------------------------------------------------------------

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

# highlighter ------------------------------------------------------------------

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
  class(out) <- c("vlightr_highlighter", subclass, "function")
  out
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
    subclass = if (is_highlight_case(x)) "vlightr_highlighter_case" else character()
  )
}

# internal accessors -----------------------------------------------------------

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
      t = vctrs::vec_c(!!!map(highlights, get_tests)),
      f = vctrs::vec_c(!!!map(highlights, get_formatters)),
    ),
    x_name = .x_name,
    error_call = .error_call,
    error_class = c(.error_class, "vlightr_error"),
    error_message = .error_message
  )
}

# user helpers -----------------------------------------------------------------

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

is_highlight_case <- function(x) {
  inherits(x, "vlightr_highlight_case")
}

#' Test if the object can be highlighted
#'
#' @description
#'
#' This function returns `TRUE` for objects which can be highlighted (e.g. via
#' [highlight()]) and `FALSE` otherwise. An object `x` can be highlighted if:
#'  * `x` is not a [data.frame] or subclass thereof
#'  * `x` is not a bare [list]
#'  * `x` is otherwise a vector, as defined by [vctrs::obj_is_vector()]
#'
#' @param x
#'
#' An object to test.
#'
#' @return
#'
#' `TRUE` if `x` is can be highlighted, `FALSE` otherwise.
#'
#' @export
is_highlightable <- function(x) {
  vctrs::obj_is_vector(x) && !is.data.frame(x) && !rlang::is_bare_list(x)
}

#' Unclass a highlighted vector
#'
#' @description Remove the conditional formatting from a highlighted vector,
#' returning the highlight to it's underlying class. If `x` is a highlightable
#' vector, then `x` and `un_highlight(highlight(x))` are equivalent.
#'
#' This is useful to expose a highlighted vector's data to function which do not
#' accept a `vlightr_highlight` class object.
#'
#' `ul()` and `un_highlight()` are synonyms.
#'
#' @param x
#'
#' A highlighted vector to unclass. If `x` is not a `vlightr_highlight` vector,
#' then `x` is returned as is.
#'
#' @examples
#' # Remove the conditional formatting of a highlighted vector
#' x <- highlight(1:5, ~ .x > 3, wrap())
#' print(x)
#' print(un_highlight(x))
#'
#' # Allow a highlighted vector to be used in functions
#' try(paste(x))
#' paste(ul(x))
#' @export
un_highlight <- function(x) {
  if (!is_highlight(x)) {
    return(x)
  }
  get_data(x)
}

#' @rdname un_highlight
#' @export
ul <- un_highlight

#' Restore a highlighted vector's formatting
#'
#' @description
#'
#' `re_highlight()` adds the conditional formatting of highlighted vectors in
#' `...` to a highlighted or un-highlighted vector `x`.
#'
#' The primary function of `re_highlight()` is to restore the format method of a
#' highlighted vector after un-highlighting it during an intermediate operation.
#' See the *Highlight, Un-Highlight, Re-Highlight* section of
#' `vignette("vlightr")` for more details.
#'
#' `re_highlight(un_highlight(x_hl), x_hl)` and `x_hl` are equivalent for a
#' highlighted vector `x_hl`.
#'
#' `re_highlight()` and `rl()` are synonyms.
#'
#' @param x `[vector]`
#'
#' A vector to highlight with the condition formatting of highlights supplied to
#' `...`. If `x` is already a highlighted vector, then it's conditional
#' formatting is combined with that of the highlights supplied to `...`.
#'
#' @param ... `[vlighter_highlight]`
#'
#' A highlighted vector used to highlight `x`. The `conditions`, `formatters`,
#' `description`, `precedence`, `format_once`, `init_formatter`, and
#' `last_formatter` of each `...` argument are appended to that of `x`, if `x`
#' is a highlighted vector, and are supplied to `highlight(x, ...)` otherwise.
#'
#' The `format_once` attribute of the output vector is set to `TRUE` if `format_once`
#' is `TRUE` for any of `x` or `...`. Otherwise, `format_once` is set to `FALSE`.
#'
#' The `init_formatter` (`last_formatter`) is taken from `x`, if it is defined
#' (i.e. not `NULL`), and is otherwise taken from the first argument in
#' `...` for which an `init_formatter` (`last_formatter`) is defined.
#'
#' @examples
#' # Restore a vector's conditional formatting after manipulating it
#' praise <- c("is cool", "has great vibes", "can do anything")
#' names <- "David|Aretha|Fats|Marvin|Lauryn"
#' praise_names <- highlight(
#'   c("Marvin", "House", "Fats", "Soup"),
#'   conditions = ~ grepl(names, .x),
#'   formatters = ~ paste(.x, rep_len(praise, length(.x)))
#' )
#' print(praise_names)
#'
#' new_names <- paste(
#'   un_highlight(praise_names),
#'   c("Gaye", "MD", "Domino", "Salad")
#' )
#' print(new_names)
#' print(re_highlight(new_names, praise_names))
#'
#' # Add several conditional formats to an unhighlighted vector
#' x <- highlight(1:5, ~ .x > 3, wrap("[", "]"))
#' y <- highlight(1:5, ~ .x < 2, wrap("<", ">"))
#' print(x)
#' print(y)
#' print(re_highlight(1:5, x, y))
#'
#' # Or add conditional formats to an already highlighted vector
#' z <- highlight(1:5, ~ .x %% 2 == 0, wrap("(", ")"))
#' print(z)
#' print(re_highlight(z, x, y))
#'
#' ## Restore the conditional formatting of multiple highlights
#' positive_adjectives <- "(amazing|great|fantastic)"
#' positive_phrases <- highlight(
#'   c("that's amazing", "alright", "you're fantastic"),
#'   conditions = ~ grepl(positive_adjectives, .x),
#'   formatters = toupper
#' )
#'
#' doubts <- "(but|maybe)"
#' doubtful_phrases <- highlight(
#'   c("but I can't go", "I'll be there", "maybe too fantastic"),
#'   conditions = ~ grepl(doubts, .x),
#'   formatters = ~ paste0(.x, "...")
#' )
#'
#' # Highlights separately
#' print(positive_phrases)
#' print(doubtful_phrases)
#'
#' # Highlights combined
#' paste0(ul(positive_phrases), ", ", ul(doubtful_phrases)) |>
#'   re_highlight(positive_phrases, doubtful_phrases)
#' @export
re_highlight <- function(x, ...) {

  dots <- rlang::list2(...)
  if (rlang::is_empty(dots)) {
    if (is_highlight(x)) {
      return(x)
    }
    return(
      new_highlight(
        check_is_highlightable(x),
        tests = list(),
        formatters = list()
      )
    )
  }

  for (i in seq_along(dots)) {
    check_is_highlight(dots[[i]], arg_name = paste0("..", i))
  }
  if (is_highlight(x)) {
    dots <- append(list(x), dots)
    x <- get_data(x)
  } else {
    x <- check_is_highlightable(x)
  }

  # TODO: We'll probably want a more custom error message for this, something
  # that mentions the incopatiblity of `x` and `...`.
  validate_highlight(
    new_highlight(
      x = x,
      tests = vctrs::vec_c(!!!map(dots, get_tests)),
      formatters = vctrs::vec_c(!!!map(dots, get_formatters)),
    ),
    x_name = rlang::caller_arg(x)
  )
}

#' @rdname re_highlight
#' @export
rl <- re_highlight

#' Highlight pipe
#'
#' @description
#'
#' A wrapper around the \code{magrittr::\link[magrittr:pipe]{\%>\%}} pipe which
#' un-highlights the input value using `un_highlight(lhs)` and re-highlights
#' the output value using `re_highlight(..., lhs)`.
#'
#' @param lhs
#'
#' A value, usually a highlighted (class `vlightr_highlight`) vector.
#'
#' @param rhs
#'
#' A function call, using \code{magrittr::\link[magrittr:pipe]{\%>\%}} semantics.
#'
#' @details
#'
#' `lhs %hl>% rhs` is equivalent to `re_highlight(un_highlight(lhs) %>% rhs, lhs)`.
#'
#' @examples
#' x <- highlight(c(1L, 0L, NA, 0L), is.na, color("red"))
#' print(x)
#'
#' # The following are equivalent
#' re_highlight(as.logical(un_highlight(x)), x)
#' x |> un_highlight() |> as.logical() |> re_highlight(x)
#' x %hl>% as.logical()
#' @export
`%hl>%` <- function(lhs, rhs) {
  lhs_expr <- substitute(lhs)
  rhs_expr <- substitute(rhs)

  env <- rlang::caller_env()
  if (is_highlight(lhs)) {
    out <- eval(rlang::expr(un_highlight(!!lhs_expr) %>% !!rhs_expr), env)
    # TODO: Look into using `restore_highlight` here
    re_highlight(out, lhs)
  } else {
    eval(rlang::expr(!!lhs_expr %>% !!rhs_expr), env)
  }
}

# TODO: Export and document
false_along <- function(x) {
  rep_len(FALSE, length(x))
}

# TODO: Export and document
true_along <- function(x) {
  rep_len(TRUE, length(x))
}

# TODO: Export and document
highlight_lambda <- function(x, x_name = rlang::caller_arg(x)) {
  rlang::check_required(x, arg = x_name)

  if (is_one_sided_formula(x)) {
    env <- rlang::f_env(x)
    if (!is.environment(env)) {
      cli::cli_abort(
        c(
          "{.arg {x_name}} must be a one-sided formula carrying an environment.",
          x = "{.arg {x_name}} is not carrying an environment."
        )
      )
    }
    expr <- rlang::f_rhs(x)
    out <- expr_to_function(expr, env)
    class(out) <- c("vlightr_highlight_lambda", "function")
    return(out)
  }

  if (is_formula(x)) {
    stop_must_not(
      x,
      x_name = x_name,
      must = "be a one-sided formula",
      not = "a two-sided formula."
    )
  }
  stop_must_not(x, x_name = x_name, must = "be a one-sided formula")
}
