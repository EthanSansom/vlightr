# constructor ------------------------------------------------------------------

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
  format_once <- check_is_vector(format_once, "logical", len = 1, nas = FALSE)
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
    caller = rlang::caller_call()
  ) {
  if (!is_highlight(arg)) {
    stop_internal("`arg` must be a {.cls vlightr_highlight} vector.")
  }
  rlang::try_fetch(
    format(arg, .x_name = arg_name),
    error = function(cnd) {
      bullets <- c(
        paste(
          "Attempted to create a malformed {.cls vlightr_highlight} vector",
          "of type <{vctrs::vec_ptype_full(arg)}>."
        ),
        x = "Can't evaluate {.cls vlightr_highlight} vector's `format()` method."
        # TODO: Add a help call or link for a better explanation. It's not super
        # intuitive from the error alone what happened in the case of an error in
        # the users supplied formatter or condition if we're far from the source
        # (i.e. combining highlights with incompatible conditions)
      )
      cli::cli_abort(
        bullets,
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

# alternate constructors -------------------------------------------------------

# TODO: A simple highlighter with a single condition and default formatted
highlight_if <- function(
    x,
    condition,
    formatter = cli::bg_br_yellow,
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

  # TODO: This only works if lhs was an expression like `~ .x == 10`
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
  .data <- vctrs::vec_ptype2(get_data(x), get_data(y))
  re_highlight(.data, x, y)
}

#' @export
vec_cast.vlightr_highlight.vlightr_highlight <- function(x, to, ...) {
  .data <- vctrs::vec_cast(get_data(x), get_data(to))
  re_highlight(.data, to)
}

#' @export
#' @method vec_arith vlightr_highlight
vec_arith.vlightr_highlight <- function(op, x, y, ...) {
  UseMethod("vec_arith.vlightr_highlight", y)
}

#' @export
#' @method vec_arith.vlightr_highlight default
vec_arith.vlightr_highlight.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.vlightr_highlight vlightr_highlight
vec_arith.vlightr_highlight.vlightr_highlight <- function(op, x, y, ...) {
  .data <- vctrs::vec_arith(op, get_data(x), get_data(y), ...)
  re_highlight(.data, x, y)
}

#' @export
#' @method vec_arith.vlightr_highlight MISSING
vec_arith.vlightr_highlight.MISSING <- function(op, x, y, ...) {
  .data <- switch(
    op,
    # This works, but the error message isn't great...
    `-` = `-`(get_data(x)),
    `+` = `+`(get_data(x)),
    vctrs::stop_incompatible_op(op, x, y)
  )
  re_highlight(.data, x)
}

#' @export
vec_math.vlightr_highlight <- function(.fn, x, ...) {
  .data <- vctrs::vec_math_base(.fn, get_data(x), ...)
  re_highlight(.data, x)
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
      fn_name = glue::glue('`attr(,"formatters")[[{i}]](attr(,"description")[[{i}]])`'),
      fn_is = "formatter",
      x_name = glue::glue('`attr(,"description")[[{i}]]`')
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

# TODO: Make this a class, so you can implement a format method.
# Like `describe_highlight`, but will say:
# A vector highlighter with 3 conditional formats:
# 1. Format 1
# 2. Format 2
# 3. Format 3

# TODO: Implement
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
  hlghtr_format_once <- check_is_vector(format_once, "logical", len = 1, nas = FALSE)
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

# TODO: Implement
highlighter_case <- function() {

}
