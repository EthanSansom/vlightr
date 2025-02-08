# un-light and re-light --------------------------------------------------------

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
  # that mentions the incompatibility of `x` and `...`.
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
    re_highlight(out, lhs)
  } else {
    eval(rlang::expr(!!lhs_expr %>% !!rhs_expr), env)
  }
}

# accessors --------------------------------------------------------------------

# TODO: Document and Export
tests <- function(x) {
  rlang::check_required(x)
  get_tests(check_is_highlight(x))
}

`tests<-` <- function(x, value) {
  rlang::check_required(x)
  rlang::check_required(value)
  x <- check_is_highlight(x)
  value <- check_is_list_of_functionish(value)
  assert_same_length(value, get_tests(x), x_name = "value", y_name = "tests(x)")

  attr(x, "tests") <- value
  validate_highlight(x)
}

# TODO: Document and Export
formatters <- function(x) {
  rlang::check_required(x)
  get_formatters(check_is_highlight(x))
}

# TODO: Document and Export
`formatters<-` <- function(x, value) {
  rlang::check_required(x)
  rlang::check_required(value)
  x <- check_is_highlight(x)
  value <- check_is_list_of_functionish(value)
  assert_same_length(value, get_tests(x), x_name = "value", y_name = "tests(x)")

  attr(x, "formatters") <- value
  validate_highlight(x)
}

# TODO: Document and Export
highlight_functions <- function(x, as = c("functions", "highlighters")) {
  rlang::check_required(x)
  x <- check_is_highlight(x)
  as <- rlang::arg_match(as)
  funs <- get_highlight_functions(x)
  switch(
    as,
    functions = funs,
    highlighters = map(funs, \(fun) highlighter(fun$test, fun$formatter))
  )
}

# TODO: Document and Export
`highlight_functions<-` <- function(x, value) {
  rlang::check_required(x)
  rlang::check_required(value)
  x <- check_is_highlight(x)
  highlighters <- check_is_list_of_highlighter(value)

  tests <- vctrs::list_unchop(map(highlighters, get_tests))
  formatters <- vctrs::list_unchop(map(highlighters, get_formatters))
  attr(x, "tests") <- tests
  attr(x, "formatters") <- formatters
  validate_highlight(x)
}

# tests ------------------------------------------------------------------------

#' Return the value `TRUE`
#'
#' @description This function always returns the value `TRUE`.
#' @param x Any object.
#' @return The value `TRUE`.
#'
#' @examples
#' # `true()` is useful for supplying a default formatter
#' # to `highlight_case()`.
#' levels <- highlight_case(
#'   1:5,
#'   1 ~ label("Low"),
#'   2 ~ label("Mid"),
#'   3 ~ label("High"),
#'   true ~ label("?") # Label any value not in 1, 2, 3
#' )
#' print(levels)
#'
#' @export
true <- function(x) {
  TRUE
}

#' Return the value `FALSE`
#'
#' @description This function always returns the value `FALSE`.
#' @param x Any object.
#' @return The value `FALSE`.
#'
#' @examples
#' # `false()` is useful for "turning-off" a conditional format
#' odd_even <- highlight_mult(
#'   1:6,
#'   .x %% 2 == 0 ~ wrap("<", ">"),
#'   .x %% 2 == 1 ~ wrap("[", "]")
#' )
#'
#' # Even values are wrapped in "<>", odd in "[]"
#' odd_even
#'
#' # Turn off the highlighting of odd values
#' tests(odd_even)[[2]] <- false
#' odd_even
#'
#' @export
false <- function(x) {
  FALSE
}

# misc -------------------------------------------------------------------------

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
