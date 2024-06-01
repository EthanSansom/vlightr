#' Set an attribute of a highlighted vector
#'
#' @param x `[vlightr_highlight]`
#'
#' A highlighted vector to set an attribute of.
#'
#' @param value
#'
#' In `set_*`, the attribute `*` that you wish to set.
#'
#' For example, in `set_conditions()`, the `value` must be a function,
#' purrr-style lambda, or list of functions or lambda to be used as the
#' `conditions` of `x`.
#'
#' See [highlight()] for details on each attribute.
#'
#' @param at `[logical / numeric]`
#'
#' For `set_conditions()`, `set_formatters()`, `set_description()`, and
#' `set_precedence()`, the indices at which to set the new attribute. Must
#' be a logical or numeric vector.
#'
#' @seealso
#'
#' [update_highlight()] to reset multiple attributes of a highlighted vector at
#' once.
#'
#' @return
#'
#' The vector `x` with one of it's attributes updated.
#'
#' @family Attribute setters
#' @name attr-setters
#' @examples
#' # Let's make these affirmations into exclamations
#' affirmations <- c("COOL", "Good stuff", "WOW", "Way to go")
#' exclamations <- highlight(
#'   affirmations,
#'   conditions = ~ toupper(.x) == .x,
#'   formatters = ~ paste(.x, "!")
#' )
#' print(affirmations)
#' print(exclamations)
#' describe_highlight(exclamations)
#'
#' # Reset `formatters` to quiet down the exclamation instead
#' exclamations <- set_formatters(exclamations, ~ tolower(.x))
#' print(exclamations)
#'
#' # Add a description
#' exclamations <- set_description(exclamations, "Set Uppercase to Lower")
#' describe_highlight(exclamations)
#'
#' # The `at` argument is useful with multiple formatters
#' x_multi <- highlight(
#'   c(13, 4, 6, 1),
#'   conditions = list(
#'     ~ .x %% 2 == 0,
#'     ~ .x %% 2 != 0,
#'     ~ .x > 10
#'    ),
#'   formatters = list(
#'     ~ paste(.x, "is even"),
#'     ~ paste(.x, "is odd"),
#'     ~ paste(.x, "is big and")
#'  )
#' )
#' print(x_multi)
#'
#' # Move the `paste(.x, "is big and")` formatter first
#' x_multi <- set_precedence(x_multi, value = -1, at = 3)
#' print(x_multi)
NULL

# TODO: Add an example using `at` to the above. Maybe change the precedence of
# one of the options.

#' @name attr-setters
#' @export
set_conditions <- function(x, value, at = NULL) {
  rlang::check_required(x)
  rlang::check_required(value)
  x <- check_is_highlight(x)
  conditions <- check_is_list_of_functionish(value)

  if (is.null(at)) {
    attr(x, "conditions") <- conditions
  } else {
    at <- if (is.logical(at)) which(at)
    at <- check_is_vector(at, "numeric", nas = FALSE)
    attr(x, "conditions")[at] <- conditions
  }
  validate_highlight(x)
}

#' @name attr-setters
#' @export
set_formatters <- function(x, value, at = NULL) {
  rlang::check_required(x)
  rlang::check_required(value)
  x <- check_is_highlight(x)
  formatters <- check_is_list_of_functionish(value)

  if (is.null(at)) {
    attr(x, "formatters") <- formatters
  } else {
    at <- if (is.logical(at)) which(at)
    at <- check_is_vector(at, "numeric", nas = FALSE)
    attr(x, "formatters")[at] <- formatters
  }
  validate_highlight(x)
}

#' @name attr-setters
#' @export
set_description <- function(x, value, at = NULL) {
  rlang::check_required(x)
  rlang::check_required(value)
  x <- check_is_highlight(x)
  description <- check_is_vector(value, "character", nas = FALSE)

  if (is.null(at)) {
    attr(x, "description") <- description
  } else {
    at <- if (is.logical(at)) which(at)
    at <- check_is_vector(at, "numeric", nas = FALSE)
    attr(x, "description")[at] <- description
  }
  validate_highlight(x)
}

#' @name attr-setters
#' @export
set_precedence <- function(x, value, at = NULL) {
  rlang::check_required(x)
  rlang::check_required(value)
  x <- check_is_highlight(x)
  precedence <- check_is_vector(value, "numeric", nas = FALSE)

  at <- at %||% TRUE
  attr(x, "precedence") <- assign_at(
    x = attr(x, "precedence"),
    at = at,
    value = precedence
  )
  validate_highlight(x)
}

#' @name attr-setters
#' @export
set_format_once <- function(x, value) {
  rlang::check_required(x)
  rlang::check_required(value)
  x <- check_is_highlight(x)
  attr(x, "format_once") <- check_is_bool(value)
  validate_highlight(x)
}

#' @name attr-setters
#' @export
set_init_formatter <- function(x, value) {
  rlang::check_required(x)
  rlang::check_required(value)
  x <- check_is_highlight(x)
  attr(x, "init_formatter") <- check_is_functionish(value)
  validate_highlight(x)
}

#' @name attr-setters
#' @export
set_last_formatter <- function(x, value) {
  rlang::check_required(x)
  rlang::check_required(value)
  x <- check_is_highlight(x)
  attr(x, "last_formatter") <- check_is_functionish(value)
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
