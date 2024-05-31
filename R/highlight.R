# constructor ------------------------------------------------------------------

#' Conditionally format a vector
#'
#' @description `highlight()` creates a `vlighter_highlight` vector with a
#'  conditional `format()` method.
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
#'  via the vctrs package.
#'
#' @param conditions `[function / list]`
#'
#'  A vectorized predicate function (i.e. a function which returns a logical
#'  vector the same length as it's input) or a list of such functions.
#'
#' @param formatters `[function / list]`
#'
#'  A vectorized formatter function (i.e. a function which returns a character
#'  vector the same length as it's input) or a list of such functions.
#'
#'  A formatter may also return an ANSI colored string (class cli_ansi_string),
#'  used for printing colored, highlighted, and emphasized text to the terminal.
#'  See [cli][ansi-styles] for details.
#'
#' @param description `[character / NULL]`
#'
#'  Hello.
#'
#' @param precedence `[numeric / NULL]`
#'
#'  Hello.
#'
#' @param format_once `[logical(1)]`
#'
#'  Hello.
#'
#' @param init_formatter `[function]`
#'
#'  Hello.
#'
#' @param last_formatter `[function]`
#'
#'  Hello.
#'
#' @return
#'
#' A highlighted vector containing the same data as `x`.
#'
#' @export
#'
#' @examples
#'
#' # Color NA values red
#' x <- c(1, 0, NA, 1, 0)
#' x_hl <- highlight(x, is.na, color("red"))
#' print(x)
#' print(x_hl)
#'
#' # Re-label indicators 1 and 0, by adding additional highlights to `x_hl`
#' x_hl <- highlight(
#'   x_hl,
#'   list(~ .x == 1, ~ .x == 0),
#'   list(~paste(.x, "[Yes]"), ~paste(.x, "[No]")
#' )
#' print(x_hl)
#'
#' # Create an equivalent vector using `dplyr::case_when` style syntax. The right
#' # hand side `formatters` must be explicit functions.
#' x_hl_case <- highlight_case(
#'   x,
#'   is.na(.x) ~ colour("red"),
#'   .x == 1 ~ \(x) paste(x, "[Yes]"),
#'   .x == 0 ~ \(x) paste(x, "[No]")
#' )
#' print(x_hl_case)
#'
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

hl <- highlight

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

is_highlight <- function(x) {
  inherits(x, "vlightr_highlight")
}

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

has_conditions <- function(x) {
  !rlang::is_empty(get_conditions(x))
}

#' @export
set_conditions <- function(x, conditions, at = NULL) {
  x <- check_is_highlight(x)
  conditions <- check_is_list_of_functionish(conditions)
  if (is.null(at)) {
    attr(x, "conditions") <- conditions
  } else {
    at <- if (is.logical(at)) which(at)
    at <- check_is_vector(at, "numeric", nas = FALSE)
    attr(x, "conditions")[at] <- conditions
  }
  validate_highlight(x)
}

#' @export
set_formatters <- function(x, formatters, at = NULL) {
  x <- check_is_highlight(x)
  formatters <- check_is_list_of_functionish(formatters)
  if (is.null(at)) {
    attr(x, "formatters") <- formatters
  } else {
    at <- if (is.logical(at)) which(at)
    at <- check_is_vector(at, "numeric", nas = FALSE)
    attr(x, "formatters")[at] <- formatters
  }
  validate_highlight(x)
}

#' @export
set_description <- function(x, description, at = NULL) {
  x <- check_is_highlight(x)
  description <- check_is_vector(description, "character", nas = FALSE)
  if (is.null(at)) {
    attr(x, "description") <- description
  } else {
    at <- if (is.logical(at)) which(at)
    at <- check_is_vector(at, "numeric", nas = FALSE)
    attr(x, "description")[at] <- description
  }
  validate_highlight(x)
}

#' @export
set_precedence <- function(x, precedence, at = NULL) {
  x <- check_is_highlight(x)
  precedence <- check_is_vector(precedence, "numeric", nas = FALSE)
  at <- at %||% TRUE
  attr(x, "precedence") <- assign_at(
    x = attr(x, "precedence"),
    at = at,
    value = precedence
  )
  validate_highlight(x)
}

#' @export
set_format_once <- function(x, format_once) {
  x <- check_is_highlight(x)
  format_once <- check_is_functionish(format_once)
  at <- at %||% TRUE
  attr(x, "format_once") <- assign_at(
    x = attr(x, "format_once"),
    at = at,
    value = format_once
  )
  validate_highlight(x)
}

#' @export
set_init_formatter <- function(x, init_formatter) {
  x <- check_is_highlight(x)
  init_formatter <- check_is_functionish(init_formatter)
  attr(x, "init_formatter") <- assign_at(
    x = attr(x, "init_formatter"),
    at = at,
    value = init_formatter
  )
  validate_highlight(x)
}

#' @export
set_last_formatter <- function(x, last_formatter) {
  x <- check_is_highlight(x)
  last_formatter <- check_is_functionish(last_formatter)
  attr(x, "last_formatter") <- assign_at(
    x = attr(x, "last_formatter"),
    at = at,
    value = last_formatter
  )
  validate_highlight(x)
}

assign_at <- function(
    x,
    at,
    value,
    x_arg = rlang::caller_arg(x),
    at_arg = rlang::caller_arg(at),
    value_arg = rlang::caller_arg(value),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {

  if (is.character(at) && !rlang::is_named(x)) {
    cli::cli_abort(
      c(
        # Thanks `rlang` for the message
        "Can't use character names to index an unnamed vector.",
        i = "{.arg {at_arg}} is a {.cls character} vector.",
        i = "{.arg {x}} is unnamed."
      ),
      call = error_call,
      class = error_class
    )
  }

  rlang::try_fetch(
    vctrs::vec_assign(
      x = x,
      i = at,
      value = value,
      x_arg = x_arg,
      value_arg = value_arg
    ),
    vctrs_error_incompatible_size = function(cnd) {
      cli::cli_abort(
        c(
          cnd$message,
          i = "{.arg {value_arg}} is size {vctrs::vec_size(value)}.",
          i = "{.arg {at_arg}} is size {vctrs::vec_size(at_arg)}."
        ),
        call = error_call,
        class = error_class
      )
    },
    vctrs_error_subscript_oob = function(cnd) {
      cli::cli_abort(
        "Can't assign {.arg {value_arg}} to {.arg {x_arg}} at {.arg {at_arg}}.",
        parent = cnd,
        call = error_call,
        class = error_class
      )
    }
  )
}

# alternate constructors -------------------------------------------------------

# TODO: A simple highlighter with a single condition and default formatted
highlight_if <- function(
    x,
    condition,
    formatter = emph(),
    description = NULL,
    precedence = NULL,
    format_once = FALSE,
    init_formatter = NULL,
    last_formatter = NULL
) {
  highlight(
    x = x,
    conditions = condition,
    formatters = formatter,
    description = description,
    precedence = precedence,
    format_once = format_once,
    init_formatter = init_formatter,
    last_formatter = last_formatter
  )
}

hl_if <- highlight_if

# For documentation: A `case_when` style interface for assigning highlights. Syntax is:
#> highlight_case(
#>  x,
#>  .x > 10 ~ cli::bg_br_red,
#>  .x < 0 ~ cli::bg_br_blue,
#>  .description = c("Above 10", "Below 0")
#> )
# - `.format_once` is TRUE by default, to make this more case_when-y
highlight_case <- function(
    x,
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

hl_case <- highlight_case

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

un_hightlight <- function(x) {
  if (!is_highlight(x)) {
    return(x)
  }
  get_data(x)
}

ul <- un_hightlight

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

describe_highlight <- function(x) {
  x_name <- rlang::caller_arg(x)
  x <- check_is_highlight(x)
  if (!has_conditions(x)) {
    cli::cli_text("A {.cls {class(get_data(x))}} vector with no conditional formatting.")
    return(invisible(x))
  }

  precedence <- order(get_precedence(x))
  formatters <- get_formatters(x)
  description <- get_description(x)
  for (i in precedence) {
    description[[i]] <- evalidate_highlight_fn(
      x = description[[i]],
      fn = formatters[[i]],
      fn_name = glue::glue('`attr({x_name},"formatters")[[{i}]]`'),
      fn_is = "formatter",
      x_name = glue::glue('`attr({x_name},"description")[[{i}]]`')
    )
  }
  cli::cli_text(
    "A conditionally formatted {.cls {class(get_data(x))}} vector with ",
    "{length(description)} conditional format{?s}:"
  )
  cli::cli_ol(description[precedence])
  return(invisible(x))
}

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

# TODO: Implement a format method for highlighters

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

# TODO: Test
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

is_highlighter <- function(x) {
  inherits(x, "vlightr_highlighter")
}

as_highligher <- function(x) {
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

get_hlghtr_condtions <- function(x) {
  rlang::fn_env(x)$hlghtr_conditions
}

get_hlghtr_formatters <- function(x) {
  rlang::fn_env(x)$hlghtr_formatters
}

get_hlghtr_precedence <- function(x) {
  rlang::fn_env(x)$hlghtr_precedence
}

get_hlghtr_description <- function(x) {
  rlang::fn_env(x)$hlghtr_description
}

#' @export
format.vlightr_highlighter <- function(x, ...) {

  conditions <- get_hlghtr_condtions(x)
  if (rlang::is_empty(conditions)) {
    formatted <- cli::cli_format_method(
      cli::cli_text(
        "A {.cls vlightr_highlighter} which applies no conditional formatting."
      )
    )
    return(formatted)
  }

  # TODO: One problem here is that in some cases, the `formatter` might return
  #       an error for a description which does NOT meet the predicate. I think
  #       it's best to scrap this entire feature honestly.
  precedence <- order(get_hlghtr_precedence(x))
  formatters <- get_hlghtr_formatters(x)
  description <- get_hlghtr_description(x)
  for (i in precedence) {
    description[[i]] <- evalidate_highlight_fn(
      x = description[[i]],
      fn = formatters[[i]],
      fn_name = glue::glue('`formatters[[{i}]]`'),
      fn_is = "formatter",
      x_name = glue::glue('`description[[{i}]]`')
    )
  }
  cli::cli_format_method({
    cli::cli_text(
      "A {.cls vlightr_highlighter} which applies ",
      "{length(description)} conditional format{?s}:"
    )
    cli::cli_ol(description[precedence])
  })
}

#' @export
print.vlightr_highlighter <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}
