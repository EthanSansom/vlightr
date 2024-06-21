# TODO -------------------------------------------------------------------------

# TODO: Long Term:
# - allow the `init_formatter` and `last_formatter` to take a second argument `width`
# - during `format.vlightr_highlight()`, the `console_width` is supplied
# - during `pillar.vlightr_highlight()`, the column width is supplied
#   - implement some custom pillar methods for the base atomic vectors, namely numbers
#   - don't highlight NA's red (too conditional), but use `pillar's` pretty number formatting
#     and truncation of other classes (if it exists)
#   - the custom methods are only used when no `init_formatter` / `last_formatter` is supplied
#   - do some interactive pillar testing
# - NOTE ^: I think the best thing to do for the `pillar` issue is to provide a
#   `highlight_pillar` method, like `highlight_format`, which receives the highlighted
#   vector `x` as input. That way, you can call `format(x)` as normal to conditionally
#   format, or alter the formatting as you see fit.
#
# - make the `highlight_format` a generic, so users can define it for other classes (like `ivs_format`)

# TODO: Short Term:
# - go through all the error functions and pick how you want to describe things
#   - is the argument `x` and name `x_name`, `arg`, `arg_name`?
#   - start putting together some naming/style conventions (element -> elt, index -> i, etc.)
# - make sure that you run every example, to confirm there are no hidden errors
# - add an `hl_case` shorthand as well
#
# - add a `templight`, `templight_case`, `tl_case`, and `tl` function
#   - these create a temporary highlight (no persistence) which takes as input
#     a logical vector the length of `x`
#   - this allows you to quickly add highlights to a column in a dataframe based
#     on ANOTHER column's value (i.e `templight(income, employed == 0)`)

# TODO Urgent:
# - fix bug in `highlight_case` where `~ .x` lambda is mistaken as the column value in
#   `dplyr::across`. Have to evaluate in the right location...
# - make a reprex for the `knitr::chunk_opts` "collapse" option being ignored when

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
#' to `highlight()`, but is otherwise similar. Arguments (other than `x`) to
#' `highlight_case()` are prefixed with a dot (`.`).
#'
#' `hl()` and `highlight()` are synonyms. `hl_case()` and `highlight_case()` are
#' synonyms.
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
#'  If the option `vlightr.colorful_default_formatters` is set via `options()` to
#'  anything other than `TRUE`, then `formatters` is set to roughly:
#'  `lapply(1:length(conditions), \(idx) paste0("[ ", x, " ][", idx, "]")`
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
  if (rlang::is_empty(formatters) && !rlang::is_empty(conditions)) {
    formatters <- default_formatters(conditions)
  }
  stop_different_length(conditions, formatters)

  if (is.null(precedence)) {
    precedence <- seq_along(conditions)
  } else {
    precedence <- check_is_vector(precedence, "numeric", nas = FALSE)
    stop_different_length(conditions, precedence)
  }
  if (is.null(description)) {
    description <- sprintf("Conditional Format %i", order(precedence))
  } else {
    description <- check_is_vector(description, "character", nas = FALSE)
    stop_different_length(conditions, description)
  }

  validate_highlight(
    new_highlight(
      x = x,
      conditions = conditions,
      formatters = formatters,
      description = description,
      precedence = precedence,
      format_once = check_is_bool(format_once),
      init_formatter = init_formatter %!|% check_is_functionish(init_formatter),
      last_formatter = last_formatter %!|% check_is_functionish(last_formatter)
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
    stop_must_not(
      arg = invalid_dot,
      arg_name = paste0("..", invalid_at),
      must = "be a two-sided formula",
      not = not
    )
  }

  dot_names <- paste0("..", seq_along(dots))
  conditions <- .mapply(
    FUN = evalidate_case_fn,
    dots = list(
      case = dots,
      case_name = dot_names
    ),
    MoreArgs = list(is_lhs = TRUE)
  )
  formatters <- .mapply(
    FUN = evalidate_case_fn,
    dots = list(
      case = dots,
      case_name = dot_names
    ),
    MoreArgs = list(is_lhs = FALSE)
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

#' @rdname highlight
#' @export
hl_case <- highlight_case

#' @export
templight <- function(
    x = logical(),
    conditions = list(),
    formatters = list(),
    description = NULL,
    precedence = NULL,
    format_once = FALSE,
    init_formatter = NULL,
    last_formatter = NULL
) {

  if (!is.list(conditions)) {
    conditions <- list(conditions)
  }
  conditions <- mapply(
    condition = conditions,
    condition_name = paste0("conditions[[", seq_along(conditions), "]]"),
    FUN = \(condition, condition_name) {
      condition <- check_is_vector(
        arg = condition,
        arg_name = condition_name,
        cls = "logical",
        len = length(x),
        arg_must = "be a logical vector the same length as `x`"
      )
      condition[is.na(condition)] <- FALSE

      # Can't just be `function(x) condition`, because it's likely that the
      # formatted version of `x` will be shorter than `x` (e.g. if we're printing
      # a tibble).
      function(x) condition[seq_along(x)]
    },
    SIMPLIFY = FALSE
  )
  highlight(
    x = x,
    conditions = conditions,
    formatters = formatters,
    description = description,
    precedence = precedence,
    format_once = format_once,
    init_formatter = init_formatter,
    last_formatter = last_formatter
  )
}

#' @export
tl <- templight

#' @export
templight_case <- function(
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
    stop_must_not(
      arg = invalid_dot,
      arg_name = paste0("..", invalid_at),
      must = "be a two-sided formula",
      not = not
    )
  }

  conditions <- mapply(
    condition = lapply(dots, rlang::f_lhs),
    condition_name = paste0("rlang::f_lhs(..", seq_along(dots), ")"),
    condition_env = lapply(dots, rlang::f_env),
    FUN = \(condition, condition_name, condition_env) {
      condition <- check_is_vector(
        arg = eval(condition, condition_env),
        arg_name = condition_name,
        cls = "logical",
        len = length(x),
        arg_must = "be a logical vector the same length as `x`"
      )
      condition[is.na(condition)] <- FALSE

      # Can't just be `function(x) condition`, because it's likely that the
      # formatted version of `x` will be shorter than `x` (e.g. if we're printing
      # a tibble).
      function(x) condition[seq_along(x)]
    },
    SIMPLIFY = FALSE
  )

  formatters <- .mapply(
    FUN = evalidate_case_fn,
    dots = list(
      case = dots,
      case_name = paste0("..", seq_along(dots))
    ),
    MoreArgs = list(is_lhs = FALSE)
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

#' @export
tl_case <- templight_case

evalidate_case_fn <- function(
    case,
    is_lhs,
    case_name = rlang::caller_arg(fn),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {

  case_env <- rlang::f_env(case)
  # TODO: Well, this fix didn't work...
  #
  # Remove any binding to the placeholder symbol `.x`, this prevents bugs when
  # used within `dplyr::across(, ~ hl(.x, ...))` for example.
  rlang::env_unbind(case_env, ".x", inherit = TRUE)
  case <- if (is_lhs) rlang::f_lhs(case) else rlang::f_rhs(case)

  # The case could be a namespaced function name (e.g. `rlang::is_bool`) or a
  # namespaced function call (e.g. `rlang::is_string(paste(.x))`).
  # - if name, then evalidated at the next `if ()` block
  # - if call, then converted to a lambda at end (e.g. `~ rlang::is_string(paste(.x))`)
  is_namespaced_fn <- rlang::is_call(case, "::")
  if (is_namespaced_fn) {
    # `::` right-hand-side can be either a string or a symbol. If it's anything
    # else, assume it's a namespaced call instead of a namespaced function.
    fn_arg <- case[[3]]
    is_namespaced_fn <- rlang::is_symbol(fn_arg) || rlang::is_string(fn_arg)
  }

  # Could be a:
  # - a function name (e.g. `is.numeric`)
  # - an in-line function (e.g. `\(x) ...` or `function(x) ...`)
  # - a namespaced function name (e.g. `rlang::is_string`)
  # In any case we can evaluate it and check whether the result is a function.
  if (rlang::is_symbol(case) || rlang::is_call(case, "function") || is_namespaced_fn) {
    fn <- rlang::try_fetch(
      eval(case, case_env),
      error = function(cnd) {
        side <- if (is_lhs) "left-hand-side" else "right-hand-side"
        cli::cli_abort(
          c(
            "Can't evaluate {side} of formula {.arg {case_name}}.",
            i = "The {side} of {.arg {case_name}} must be a function or purrr-style lambda expression."
          ),
          parent = cnd,
          call = error_call,
          class = error_class
        )
      }
    )
    return(
      check_is_functionish(
        arg = fn,
        error_call = error_call,
        error_class = error_class,
        error_msg = glue::glue(
          "The {side} of `{case_name}` must be a function ",
          "or purrr-style lambda expression."
        )
      )
    )
  }

  # Otherwise, put anything in a formula. This allows the last LHS case to be
  # `TRUE ~ ...`, as in `dplyr::case_when()`, or a lambda body (ex. `.x > 0`).
  rlang::new_formula(
    lhs = NULL,
    rhs = case,
    env = case_env
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

default_formatters <- function(conditions) {
  colorful <- getOption("vlightr.colorful_default_formatters", default = TRUE)
  if (isTRUE(colorful)) {
    formatters <- list(
      cli::bg_br_yellow,
      cli::bg_br_green,
      cli::bg_br_red,
      cli::bg_br_cyan,
      cli::bg_br_magenta,
      cli::bg_br_blue
    ) |> rep_len(length(conditions))
  } else {
    idx <- seq_along(conditions)
    formatters <- lapply(
      sprintf(" ][%s]", formatC(idx, width = max(nchar(idx)), flag = "0")),
      \(right) wrap("[ ", right)
    )
  }
  formatters
}

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
  inner <- vctrs::vec_ptype_full(get_data(x))
  paste0("highlight<", inner, ">")
}

#' @export
vec_ptype2.vlightr_highlight.vlightr_highlight <- function(x, y, ...) {
  # We want to create a highlight of a size-0 slice of `x`/`y` but:
  # 1. Some (valid) formatters and conditions will error on a zero-length vector
  #   - Ex. the condition `~.x == min(.x)` will warn if `.x` is an empty numeric
  #   - Don't want to prevent the user from combining (via `c()`) two non-empty
  #     vectors `x` and `y` just because the highlight will fail in the empty case
  # 2. formatters and conditions aren't applied to size-0 vectors by `format()`,
  #    meaning that `validate_highlight()` doesn't even check these vectors.
  #
  # To resolve this, we first combine the underlying data via `vctrs::vec_c`.
  # If they're incompatible, an error will still be thrown like in `vec_ptype2`.
  #
  # Then, we validate the resulting highlight using the combined `x` and `y` data.
  # Only after validation is the resulting highlight sliced to size-0 for the
  # prototype.
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
      i = "Combined objects may have incompatible {.val formatters} or {.val conditions} attributes."
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

#' Update a highlighted vector's format method
#'
#' @description `update_highlight()` sets any of the `conditions`,
#' `formatters`, `description`, `precedence`, `format_once`, `init_formatter`,
#' or `last_formatter` attributes in an already highlighted vector and validates
#' the result.
#'
#' Updated attributes must conform to the existing attributes. For example, a
#' supplied list of `conditions` functions must be the same length as the
#' existing `formatters` attribute of `x`.
#'
#' @param x `[vlightr_highlight]`
#'
#' A highlighted vector to update.
#'
#' @inheritParams highlight
#'
#' @family attribute setters
#'
#' @examples
#' dummies <- c(1L, 0L, NA, 0L, 1L)
#' dummies_hl <- highlight_case(
#'   x = dummies,
#'   .x == 0 ~ paste0(.x, "[No]"),
#'   .x == 1 ~ paste0(.x, "[Yes]"),
#'   TRUE ~ "Don't Know"
#')
#' print(dummies_hl)
#' describe_highlight(dummies_hl)
#'
#' # Add a `description` and `last_formatter`
#' dummies_hl <- update_highlight(
#'   x = dummies_hl,
#'   description = c("Set 0 to No", "Set 1 to Yes", " Otherwise, Don't Know"),
#'   last_formatter = background("yellow")
#' )
#' print(dummies_hl)
#' describe_highlight(dummies_hl)
#'
#' # Change the `formatters`
#' update_highlight(
#'   x = dummies_hl,
#'   formatters = list(~ "No", ~ "Yes", ~ color("red"))
#' )
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
    init_formatter = init_formatter %||% get_init_formatter(x),
    last_formatter = last_formatter %||% get_last_formatter(x)
  )
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
#' paste(un_highlight(x))
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
#' The `format_once`, `init_formatter`, and `last_formatter` attributes are
#' scalar, meaning they cannot be appended to.
#'
#' The `format_once` attribute of the output vector is set to `TRUE` if any of
#' `x` or `...` have `format_once` is `TRUE`. Otherwise, `format_once` is set to
#' `FALSE`.
#'
#' `init_formatter` and `last_formatter` are taken from `x`, if they are defined
#' (i.e. not `NULL`) for `x`, and are otherwise taken from the first argument in
#' `...` with a defined `init_formatter` (`last_formatter`) respectively.
#'
#' @examples
#' # Restore a vector's conditional formatting after manipulating it
#' praise <- c("is cool", "has great vibes", "can do anything")
#' names <- "David|Aretha|Fats|Marvin|Lauryn"
#' praise_names <- highlight(
#'   c("Marvin", "House", "Fats", "Soup"),
#'   ~ grepl(names, .x),
#'   ~ paste(.x, rep_len(praise, length(.x)))
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
#' # Restore the conditional formatting of multiple highlights
#' positive_adjectives <- "(amazing|great|fantastic)"
#' positive_phrases <- highlight(
#'   c("that's amazing", "alright", "you're fantastic"),
#'   ~ grepl(positive_adjectives, .x),
#'   toupper
#' )
#'
#' doubts <- "(but|maybe)"
#' doubtful_phrases <- highlight(
#'   c("but I can't go", "I'll be there", "maybe too fantastic"),
#'   ~ grepl(doubts, .x),
#'   ~ paste0(.x, "...")
#' )
#'
#' print(positive_phrases)
#' print(doubtful_phrases)
#' paste0(ul(positive_phrases), ", ", ul(doubtful_phrases)) |>
#'   re_highlight(positive_phrases, doubtful_phrases)
#' @export
re_highlight <- function(x, ...) {
  dots <- rlang::list2(...)
  if (rlang::is_empty(dots)) {
    return(check_is_highlightable(x))
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

  envir <- rlang::caller_env()
  if (is_highlight(lhs)) {
    out <- eval(rlang::expr(un_highlight(!!lhs_expr) %>% !!rhs_expr), envir)
    # TODO: Look into using `restore_highlight` here
    re_highlight(out, lhs)
  } else {
    eval(rlang::expr(!!lhs_expr %>% !!rhs_expr), envir = envir)
  }
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
#' a highlighter function, then the user arguments are appended to the pre-filled
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
#'  x = dummy,
#'  .x == 0 ~ paste0(.x, "[No]"),
#'  .x == 1 ~ paste0(.x, "[Yes]"),
#'  is.na ~ color("red")
#' )
#' dummy_hlghtr <- highlighter_case(
#'  .x == 0 ~ paste0(.x, "[No]"),
#'  .x == 1 ~ paste0(.x, "[Yes]"),
#'  is.na ~ color("red")
#' )
#' print(dummy_hl)
#' print(dummy_hlghtr(x = x))
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
  if (is.null(description)) {
    hlghtr_description <- sprintf("Conditional Format %i", order(hlghtr_precedence))
  } else {
    hlghtr_description <- check_is_vector(description, "character", nas = FALSE)
    stop_different_length(
      x = hlghtr_conditions,
      y = hlghtr_description,
      x_name = "conditions",
      y_name = "description"
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

#' @rdname highlighter
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

  dot_names <- paste0("..", seq_along(dots))
  conditions <- .mapply(
    FUN = evalidate_case_fn,
    dots = list(
      case = dots,
      case_name = dot_names
    ),
    MoreArgs = list(is_lhs = TRUE)
  )
  formatters <- .mapply(
    FUN = evalidate_case_fn,
    dots = list(
      case = dots,
      case_name = dot_names
    ),
    MoreArgs = list(is_lhs = FALSE)
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
#' @param x [vlightr_highlight]
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
