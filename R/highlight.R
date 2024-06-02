# TODO -------------------------------------------------------------------------

# TODO: Add details on the order of operations in the highlight format function.
#       Make the below into a markdown, which can be used in the `@section Under the Hood:`
#       section of the `highlighter()` documentation. Actually, this would be good
#       to put in the vignette!
#
# Consider the vector `x_hl` defined below. We'll assume that `x` is some vector
# and `conditions`, `formatters`, `precedence`, `init_formatter`, `last_formatter`
# are all valid arguments.
# `
# x_hl <- highlight(
#  x = x,
#  conditions = conditions,
#  formatters = formatters,
#  precedence = precedence,
#  init_formatter = init_formatter,
#  last_formatter = last_formatter
# )
# `
# When `format(x_hl)` is called (e.g. within `print()`) the following
# psuedo-code is executed.
#
# Apply an initial format to `x`, converting `x` into a character vector.
#
# `formatted <- init_formatter(x)`
#
# Apply each format in `formatters` to elements of `x` meet a condition. Do
# so in the order specified by `precedence`.
#
# `
# format_order <- order(precedence)
# for (i in format_order) {
#   condition <- conditions[[i]]
#   formatter <- formatters[[i]]
#   condition_is_met <- condition(x)
#   formatted[condition_is_met] <- formatter(formatted[condition_is_met])
# }
# `
# Apply the last format before returning.
#
# `last_formatter(formatted)`

# TODO: Side-quest, make sub-classes for the base atomic types (or some of them)
#       with the normal coercion and casting rules. I.e. integer, numeric, logical,
#       and character would be good to have. See haven::labelled implementation
#       for numeric vectors. Potentially factors. Also, you could make a `labelled`
#       subclass, which is a shorthand for highlight_case(.x == value ~ \(x) "Value Label").

# constructor ------------------------------------------------------------------

#' Conditionally format a vector
#'
#' @description `highlight()`, `hl()`, and `highlight_case()` create a
#' `vlighter_highlight` vector with a conditional [format()] method.
#'
#' `vlighter_highlight` is a generic vector superclass which maintains
#' the underlying data of it's subclass while changing how the subclass is
#' formatted and printed. In other words, the vector `highlight(1:5)` can (with
#' limited legwork) be treated exactly the same as the integer vector `1:5`,
#' but has a different `format()` and `print()` method.
#'
#' `highlight_case()` provides an alternative [dplyr::case_when()] style syntax
#' to `highlight()` and defaults to `format_once = TRUE`, but is otherwise
#' equivalent to `highlight()`. Arguments (other than `x`) to `highlight_case()`
#' are prefixed with a `.`.
#'
#' `hl()` and `highlight()` are synonyms.
#'
#' @param x `[vector]`
#'
#'  A vector to highlight. Conceptually, a vector is a collection of objects of
#'  size 1.
#'
#'  `x` is considered a vector if:
#'  * `x` is not a [data.frame].
#'  * `x` is not a bare [list].
#'  * `x` is otherwise a vector, as defined by [vctrs::obj_is_vector()].
#'
#'  Atomic vector types `"logical"`, `"integer"`, `"double"`, `"complex"`,
#'  `"character"`, and `"raw"` meet these criteria. As do vectors defined
#'  via the vctrs package and many others.
#'
#' @param conditions `[function / list]`
#'
#'  Functions that indicate which elements of `x` to format. Can be a:
#'  * Function, e.g. `is.na`
#'  * A purrr-style lambda, e.g. `~ nchar(.x) > 0`
#'  * A list of functions or lambdas, e.g. `list(~ .x < mean(.x), is.infinite)`
#'
#'  Each function in `conditions` will receive `x` as it's input and must return
#'  a logical vector the same length as `x` or of length 1 (in which case the
#'  result will be recycled to the length of `x`).
#'
#'  Elements of `x` for which `conditions[[i]](x)` is `TRUE` are formatted using
#'  the formatter function `formatters[[i]]`. Consequently, an equal number of
#'  `conditions` and `formatters` must be supplied.
#'
#' @param formatters `[function / list]`
#'
#'  Character manipulation functions used to format `x`. Can be a:
#'  * Function, e.g. [cli::style_bold]
#'  * A purrr-style lambda, e.g. `~ paste0(.x, "!")`
#'  * A list of functions or lambdas, e.g. `list(~ cli::col_red, toupper)`
#'
#'  When called, a function in `formatters` will receive a single character vector
#'  (of variable length) as it's only argument. A formatter must return a character
#'  vector the same length as it's input. ANSI string vectors (class
#'  `cli_ansi_string`) are also supported (see [cli::ansi-styles] for details).
#'
#'  The same number of `formatters` and `conditions` must be supplied.
#'
#' @param ... `[formula]`
#'
#'  For `highlighter_case()`, a two sided formula with a condition on the
#'  left-hand-side and a formatter function on the right-hand-side.
#'
#'  This argument replaces the `formatters` and `conditions` arguments of
#'  `highlight()`. The i-th dot supplied is roughly equivalent to
#'  `conditions[[i]] ~ formatters[[i]]`.
#'
#'  Examples of arguments to `...` include:
#'  * Colour `NA` values red: `is.na(.x) ~ cli::col_red`
#'  * Replace 1's with 2's: `.x == 1 ~ \(x) "2"`
#'  * Add an exclamation mark: `toupper(.x) == .x ~ \(x) paste0(x, "!")`
#'
#'  The left-hand-side must be the body of a purrr-style lambda (i.e. a lambda
#'  without the prefix `~`) and the right-hand-side must be a function.
#'
#' @param description,.description `[character / NULL]`
#'
#'  An optional description of the format applied by each function in
#'  `formatters`. This information is used by [describe_highlight()].
#'
#'  If supplied, `description` must be the same length as `formatters` and
#'  `conditions`.
#'
#' @param precedence,.precedence `[numeric / NULL]`
#'
#'  A numeric vector indicating the order in which to apply the `formatters`. The
#'  formatter with the lowest corresponding `precedence` value is evaluatted first
#'  during formatting. By default `formatters` are applied in the order in which
#'  they were supplied.
#'
#'  If supplied, `precedence` must be the same length as `formatters` and
#'  `conditions`.
#'
#' @param format_once,.format_once `[logical(1)]`
#'
#'  A `TRUE` or `FALSE` value. Indicates whether an element of `x` which meets
#'  multiple `conditions` should be formatted only once (using the formatter
#'  corresponding to the first condition met) or formatted multiple times (using
#'  all of the corresponding `formatters`).
#'
#'  `format_once` is `FALSE` by default in `highlight()` and `TRUE` by default in
#'  `highlight_case()` (to mimic the behavior of conditions in [dplyr::case_when()]).
#'
#' @param init_formatter,.init_formatter `[function / NULL]`
#'
#'  The first function used to format `x`. `init_formatter(x)` is called prior
#'  to conditionally formatting `x` using the `formatters` functions. If `NULL`,
#'  then `format(x)` is called instead.
#'
#' @param last_formatter,.last_formatter `[function / NULL]`
#'
#'  The last function called to format `x`. The `last_formatter` is applied
#'  after `x` has been conditionally formatted (i.e. after the `init_formatter`
#'  and `formatters` functions have been applied).
#'
#'  `last_formatter` will receive a character vector the same length as
#'  `x` as it's only argument. If `NULL`, the conditionally formatted `x`
#'  is returned as is by `format()`.
#'
#' @return
#'
#' A highlighted vector (class `vlightr_highlight`) containing the same data as
#' `x`.
#'
#' @seealso
#'
#' [un_highlight()] for converting a vector `highlight(x)` back to `x`.
#'
#' [as_highlighter()] to generate a [highlighter()] function using a highlighted
#' vector (e.g. `x_hl <- highlight(x, ...)`) which can highlight other vectors
#' using the arguments used to create `x_hl` as defaults.
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
#'   conditions = list(~ .x == 1, ~ .x == 0),
#'   formatters = list(~ paste(.x, "[Yes]"), ~ paste(.x, "[No]"))
#' )
#' print(x_hl)
#' describe_highlight(x_hl)
#'
#' # Using `dplyr::case_when` style syntax.
#' # The right hand side `formatters` must be explicit functions.
#' x_hl_case <- highlight_case(
#'   x,
#'   is.na(.x) ~ colour("red"),
#'   .x == 1 ~ \(x) paste(x, "[Yes]"),
#'   .x == 0 ~ \(x) paste(x, "[No]"),
#'   .description = c(
#'     "Coloured Red if NA",
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
#' # Apply only a single format with `format_once`
#' x_multi <- set_format_once(x_multi, TRUE)
#' print(x_multi)
#' @export
highlight <- function(
    x = logical(),
    conditions = list(),
    formatters = list(),
    description = NULL,
    precedence = NULL,
    format_once = FALSE,
    init_formatter = NULL,
    last_formatter = NULL
  ) {

  x_name <- rlang::caller_arg(x)
  x <- if (is_highlight(x)) get_data(x) else check_is_highlightable(x)
  conditions <- check_is_list_of_functionish(conditions)
  formatters <- check_is_list_of_functionish(formatters)
  stop_different_length(conditions, formatters)

  if (is.null(description)) {
    description <- sprintf("Format %i", seq_along(conditions))
  } else {
    description <- check_is_vector(description, "character", nas = FALSE)
    stop_different_length(conditions, description)
  }
  if (is.null(precedence)) {
    precedence <- seq_along(conditions)
  } else {
    precedence <- check_is_vector(precedence, "numeric", nas = FALSE)
    stop_different_length(conditions, precedence)
  }
  format_once <- check_is_bool(format_once)
  init_formatter <- init_formatter %!|% check_is_functionish(init_formatter)
  last_formatter <- last_formatter %!|% check_is_functionish(last_formatter)

  validate_highlight(
    new_highlight(
      x = x,
      conditions = conditions,
      formatters = formatters,
      description = description,
      precedence = precedence,
      format_once = format_once,
      init_formatter = init_formatter,
      last_formatter = last_formatter
    ),
    arg_name = x_name
  )
}

#' @rdname highlight
#' @export
hl <- highlight

#' @rdname highlight
#' @export
highlight_case <- function(
    x,
    ...,
    .description = NULL,
    .precedence = NULL,
    .format_once = TRUE,
    .init_formatter = NULL,
    .last_formatter = NULL
) {

  rlang::check_required(x)
  dots <- rlang::list2(...)
  are_formulas <- vapply(dots, rlang::is_formula, logical(1L), lhs = TRUE)
  if (!all(are_formulas)) {
    invalid_at <- which.min(are_formulas)
    invalid_dot <- dots[[invalid_at]]
    not <- if (rlang::is_formula(invalid_dot)) {
      "a one-sided formula"
    } else {
      "{.obj_type_friendly {arg}}"
    }
    stop_must_not(invalid_dot, must = "be a two-sided formula", not = not)
  }

  # TODO: I actually don't see any reason why the lhs couldn't be a function.
  # - if it's a name, evaluate and see if it's a function
  # - if it's a call to function, evaluate and use that function (ex. \(x) x + 1 or function(x) x + 1)
  # - if it's any other expression, assume it's a lamdba turn to a formula
  #
  # You can make something like `evalidate_case_function()` and use that for both
  # the `conditions` (lhs) and `formatters` (rhs) supplied to `highlight_case`
  conditions <- lapply(
    dots,
    \(x) {
      rlang::new_formula(
        lhs = NULL,
        rhs = rlang::f_lhs(x),
        env = rlang::f_env(x)
      )
    }
  )
  # TODO: These too could be lambdas, named functions, or inline functions
  # (e.g. cli::col_red, \(x) paste(x, 10), paste(.x, "!)). Remember, lambdas
  # won't have the first `~`.
  formatters <- lapply(
    dots,
    \(x) eval(rlang::f_rhs(x), envir = rlang::f_env(x))
  )

  highlight(
    x = x,
    conditions = conditions,
    formatters = formatters,
    description = .description,
    precedence = .precedence,
    format_once = .format_once,
    init_formatter = .init_formatter,
    last_formatter = .last_formatter
  )
}

new_highlight <- function(
    x,
    conditions,
    formatters,
    description,
    precedence,
    format_once,
    init_formatter,
    last_formatter
  ) {
  # `new_vctr(.data)` un-classes `.data` (try `new_vctr(ivs::iv(1, 2))`). Using
  # a record instead.
  vctrs::new_rcrd(
    fields = list(.data = x),
    conditions = conditions,
    formatters = formatters,
    description = description,
    precedence = precedence,
    format_once = format_once,
    init_formatter = init_formatter,
    last_formatter = last_formatter,
    class = "vlightr_highlight"
  )
}

validate_highlight <- function(
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error",
    error_message = NULL
  ) {
  if (!is_highlight(arg)) {
    stop_internal("`arg` must be a {.cls vlightr_highlight} vector.")
  }
  rlang::try_fetch(
    format(arg, .x_name = arg_name),
    error = function(cnd) {
      error_message <- error_message %||% c(
        paste(
          "Attempted to create a malformed {.cls vlightr_highlight} vector",
          "of type <{vctrs::vec_ptype_full(arg)}>."
        ),
        x = "Can't evaluate {.cls vlightr_highlight} vector's `format()` method."
      )
      cli::cli_abort(
        error_message,
        parent = cnd,
        call = error_call,
        class = error_class
      )
    }
  )
  arg
}

#' @export
is_highlight <- function(x) {
  inherits(x, "vlightr_highlight")
}

#' @export
is_highlightable <- function(x) {
  vctrs::obj_is_vector(x) && !is.data.frame(x) && !rlang::is_bare_list(x)
}

get_data <- function(x) {
  vctrs::field(x, ".data")
}

get_conditions <- function(x) {
  attr(x, "conditions")
}

get_formatters <- function(x) {
  attr(x, "formatters")
}

get_description <- function(x) {
  attr(x, "description")
}

get_precedence <- function(x) {
  attr(x, "precedence")
}

get_format_once <- function(x) {
  attr(x, "format_once")
}

get_init_formatter <- function(x) {
  attr(x, "init_formatter")
}

get_last_formatter <- function(x) {
  attr(x, "last_formatter")
}

# methods ----------------------------------------------------------------------

#' @export
vec_ptype_abbr.vlightr_highlight <- function(x) {
  inner <- vctrs::vec_ptype_abbr(get_data(x))
  paste0("hlght<", inner, ">")
}

#' @export
vec_ptype_full.vlightr_highlight <- function(x) {
  inner <- vctrs::vec_ptype_abbr(get_data(x))
  paste0("highlight<", inner, ">")
}

#' @export
vec_ptype2.vlightr_highlight.vlightr_highlight <- function(x, y, ...) {
  .data <- rlang::try_fetch(
    vctrs::vec_ptype2(get_data(x), get_data(y)),
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
  restore_highlight(
    .data, x, y,
    .error_message = c(
      "Combination produced a malformed {.cls vlightr_highlight} vector.",
      i = "Combined objects may have incompatible {.val formatters} or {.val conditions} attributes."
    ),
    .error_class = c("vlightr_ptype2_error", "vlightr_error")
  )
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
      i = paste0("Arguments to {.code ", op, "} may have incompatible {.val formatters} or {.val conditions} attributes.")
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

  # Use function only where we can assume that `...` are `vlightr_highlight` objects.
  highlights <- rlang::list2(...)
  init_formatters <- lapply(highlights, get_init_formatter)
  last_formatters <- lapply(highlights, get_last_formatter)
  init_formatter_at <- which.min(vapply(init_formatters, is.null, logical(1L)))
  last_formatter_at <- which.min(vapply(last_formatters, is.null, logical(1L)))

  validate_highlight(
    new_highlight(
      x = x,
      conditions = vctrs::vec_c(!!!lapply(highlights, get_conditions)),
      formatters = vctrs::vec_c(!!!lapply(highlights, get_formatters)),
      description = vctrs::vec_c(!!!lapply(highlights, get_description)),
      precedence = vctrs::vec_c(!!!lapply(highlights, get_precedence)),
      format_once = any(vctrs::vec_c(!!!lapply(highlights, get_format_once))),
      init_formatter = init_formatters[[init_formatter_at]],
      last_formatter = last_formatters[[last_formatter_at]]
    ),
    arg_name = .x_name,
    error_message = .error_message,
    error_call = .error_call,
    error_class = c(.error_class, "vlightr_error")
  )
}

# helpers ----------------------------------------------------------------------

#' @export
update_highlight <- function(
    x,
    conditions = NULL,
    formatters = NULL,
    description = NULL,
    precedence = NULL,
    format_once = NULL,
    init_formatter = NULL,
    last_formatter = NULL
    ) {

  rlang::check_required(x)
  x <- check_is_highlight(x)
  highlight(
    x = get_data(x),
    conditions = conditions %||% get_conditions(x),
    formatters = formatters %||% get_formatters(x),
    description = description %||% get_description(x),
    precedence = precedence %||% get_precedence(x),
    format_once = format_once %||% get_format_once(x),
    init_formatter = init_formatter %||% init_formatter(x),
    last_formatter = last_formatter %||% last_formatter(x)
  )
}

#' @export
un_highlight <- function(x) {
  if (!is_highlight(x)) {
    return(x)
  }
  get_data(x)
}

#' @export
ul <- un_highlight

#' @export
re_highlight <- function(x, ...) {
  dots <- rlang::list2(...)
  for (i in seq_along(dots)) {
    check_is_highlight(dots[[i]], arg_name = paste0("..", i))
  }
  if (is_highlight(x)) {
    dots <- append(dots, x)
    x <- get_data(x)
  } else {
    x <- check_is_highlightable(x)
  }

  # Taking the first defined (non-null) `init_formatter` and `last_formatter`
  init_formatters <- lapply(dots, get_init_formatter)
  last_formatters <- lapply(dots, get_last_formatter)
  init_formatter_at <- which.min(vapply(init_formatters, is.null, logical(1L)))
  last_formatter_at <- which.min(vapply(last_formatters, is.null, logical(1L)))

  validate_highlight(
    new_highlight(
      x = x,
      conditions = vctrs::vec_c(!!!lapply(dots, get_conditions)),
      formatters = vctrs::vec_c(!!!lapply(dots, get_formatters)),
      description = vctrs::vec_c(!!!lapply(dots, get_description)),
      precedence = vctrs::vec_c(!!!lapply(dots, get_precedence)),
      format_once = any(vctrs::vec_c(!!!lapply(dots, get_format_once))),
      init_formatter = init_formatters[[init_formatter_at]],
      last_formatter = last_formatters[[last_formatter_at]]
    ),
    arg_name = rlang::caller_arg(x)
  )
}

#' @export
rl <- re_highlight

#' @export
`%hl>%` <- function(lhs, rhs) {
  lhs_expr <- substitute(lhs)
  rhs_expr <- substitute(rhs)

  envir <- rlang::caller_env()
  if (is_highlight(lhs)) {
    out <- eval(rlang::expr(un_hightlight(!!lhs_expr) %>% !!rhs_expr), envir)
    re_highlight(out, lhs)
  } else {
    eval(rlang::expr(!!lhs_expr %>% !!rhs_expr), envir = envir)
  }
}

# highlighter ------------------------------------------------------------------

#' @export
highlighter <- function(
    conditions = list(),
    formatters = list(),
    description = NULL,
    precedence = NULL,
    format_once = FALSE,
    init_formatter = NULL,
    last_formatter = NULL
  ) {

  hlghtr_conditions <- check_is_list_of_functionish(conditions)
  hlghtr_formatters <- check_is_list_of_functionish(formatters)
  stop_different_length(
    x = hlghtr_conditions,
    y = hlghtr_formatters,
    x_name = "conditions",
    y_name = "formatters"
  )

  if (is.null(description)) {
    hlghtr_description <- sprintf("Format %i", seq_along(hlghtr_conditions))
  } else {
    hlghtr_description <- check_is_vector(description, "character", nas = FALSE)
    stop_different_length(
      x = hlghtr_conditions,
      y = hlghtr_description,
      x_name = "conditions",
      y_name = "description"
    )
  }
  if (is.null(precedence)) {
    hlghtr_precedence <- seq_along(hlghtr_conditions)
  } else {
    hlghtr_precedence <- check_is_vector(precedence, "numeric", nas = FALSE)
    stop_different_length(
      x = hlghtr_conditions,
      y = hlghtr_precedence,
      x_name = "conditions",
      y_name = "precedence"
    )
  }
  hlghtr_format_once <- check_is_bool(format_once)
  hlghtr_init_formatter <- init_formatter %!|% check_is_functionish(init_formatter)
  hlghtr_last_formatter <- last_formatter %!|% check_is_functionish(last_formatter)

  rm(conditions, formatters, description, precedence, format_once, init_formatter, last_formatter)
  out <- function(
    x,
    conditions = list(),
    formatters = list(),
    description = NULL,
    precedence = NULL,
    format_once = NULL,
    init_formatter = NULL,
    last_formatter = NULL
  ) {
    out <- highlight(
      x = x,
      conditions = conditions,
      formatters = formatters,
      description = description,
      precedence = precedence,
      format_once = format_once %||% hlghtr_format_once,
      init_formatter = init_formatter %||% hlghtr_init_formatter,
      last_formatter = last_formatter %||% hlghtr_last_formatter
    )
    attr(out, "conditions") <- vctrs::vec_c(hlghtr_conditions, get_conditions(out))
    attr(out, "formatters") <- vctrs::vec_c(hlghtr_formatters, get_formatters(out))
    attr(out, "description") <- vctrs::vec_c(hlghtr_description, get_description(out))
    attr(out, "precedence") <- vctrs::vec_c(hlghtr_precedence, get_precedence(out))
    validate_highlight(out)
  }

  class(out) <- c("vlightr_highlighter", "function")
  out
}

#' @export
highlighter_case <- function(
    ...,
    .description = NULL,
    .precedence = NULL,
    .format_once = TRUE,
    .init_formatter = NULL,
    .last_formatter = NULL
  ) {

  dots <- rlang::list2(...)
  are_formulas <- vapply(dots, rlang::is_formula, logical(1L), lhs = TRUE)
  if (!all(are_formulas)) {
    invalid_at <- which.min(are_formulas)
    invalid_dot <- dots[[invalid_at]]
    not <- if (rlang::is_formula(invalid_dot)) {
      "a one-sided formula"
    } else {
      "{.obj_type_friendly {arg}}"
    }
    stop_must_not(invalid_dot, must = "be a two-sided formula", not = not)
  }

  # TODO: Update to follow how `highlight_case` does this.
  conditions <- lapply(
    dots,
    \(x) {
      rlang::new_formula(
        lhs = NULL,
        rhs = rlang::f_lhs(x),
        env = rlang::f_env(x)
      )
    }
  )
  formatters <- lapply(
    dots,
    \(x) eval(rlang::f_rhs(x), envir = rlang::f_env(x))
  )

  highlighter(
    conditions = conditions,
    formatters = formatters,
    description = .description,
    precedence = .precedence,
    format_once = .format_once,
    init_formatter = .init_formatter,
    last_formatter = .last_formatter
  )
}

#' @export
is_highlighter <- function(x) {
  inherits(x, "vlightr_highlighter")
}

#' @export
as_highlighter <- function(x) {
  rlang::check_required(x)
  x <- check_is_highlight(x)
  highlighter(
    conditions = get_conditions(x),
    formatters = get_formatters(x),
    description = get_description(x),
    precedence = get_precedence(x),
    format_once = get_format_once(x),
    init_formatter = get_init_formatter(x),
    last_formatter = get_last_formatter(x)
  )
}
