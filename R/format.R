# todos ------------------------------------------------------------------------

#### Error Messages:
#
# Test all kinds of error cases and dial in the error messages. We're really
# relying on the error messages of the `format()` method, since that's how
# we validate highlights.

# format -----------------------------------------------------------------------

#' @export
format.vlightr_highlight <- function(x, ..., .x_name = rlang::caller_arg(x)) {

  x_data <- get_data(x)
  if (rlang::is_empty(x_data)) {
    return(character())
  }

  formatted <- highlight_format(x_data)
  env <- rlang::caller_env()
  tests <- get_tests(x)
  formatters <- get_formatters(x)

  for (i in seq_along(tests)) {
    format_at <- call_format_fun(
      x = x_data,
      fun = tests[[i]],
      env = env,
      fun_name = paste0("tests(", .x_name, ")[[", i, "]]"),
      fun_type = "test"
    )
    if (any(format_at)) {
      formatted[format_at] <- call_format_fun(
        x = formatted[format_at],
        fun = formatters[[i]],
        env = env,
        fun_name = paste0("formatters(", .x_name, ")[[", i, "]]"),
        fun_type = "formatter"
      )
    }
  }

  formatted
}

#' @export
format.vlightr_highlight_case <- function(x, ..., .x_name = rlang::caller_arg(x)) {

  x_data <- get_data(x)
  if (rlang::is_empty(x_data)) {
    return(character())
  }

  formatted <- highlight_format(x_data)
  env <- rlang::caller_env()
  tests <- get_tests(x)
  formatters <- get_formatters(x)
  unformatted_at <- rep_len(TRUE, length(formatted))

  for (i in seq_along(tests)) {
    test <- tests[[i]]
    # `format_all()` is the test given when the user passes a one-sided formula
    # to format case to apply a formatter to all elements. This if overrides the
    # default case behavior.
    if (identical(test, format_all)) {
      format_at <- rep_len(TRUE, length(x_data))
    }
    else {
      format_at <- unformatted_at & call_format_fun(
        x = x_data,
        fun = test,
        env = env,
        fun_name = paste0("tests(", .x_name, ")[[", i, "]]"),
        fun_type = "test"
      )
      unformatted_at <- unformatted_at & !format_at
    }

    if (any(format_at)) {
      formatted[format_at] <- call_format_fun(
        x = formatted[format_at],
        fun = formatters[[i]],
        env = env,
        fun_name = paste0("formatters(", .x_name, ")[[", i, "]]"),
        fun_type = "formatter"
      )
    }
  }

  formatted
}

# format helpers ---------------------------------------------------------------

call_format_fun <- function(
    x,
    fun,
    env = rlang::caller_env(),
    fun_name = rlang::caller_arg(fun),
    fun_type = c("test", "formatter"),
    error_call = rlang::caller_env()
  ) {

  result <- try_format_fun(x, fun, env = env, error_call = error_call)
  is_test <- fun_type == "test"
  is_correct_class <- if (is_test) is.logical(result) else is.character(result)

  if (is_correct_class && length(result) %in% c(1, length(x))) {
    if (is_test) result[is.na(result)] <- FALSE
    return(vctrs::vec_recycle(result, length(x)))
  }

  fun_type <- upper1(fun_type)
  if (!is_correct_class) {
    correct_class <- if (is_test) "logical" else "character"
    cli::cli_abort(
      c(
        "{fun_type} function {.arg {fun_name}} must return a {correct_class} vector.",
        x = "{.arg {fun_name}} returned {.obj_type_friendly {result}}.",
        if (context$in_across) across_error_hint()
      ),
      call = error_call
    )
  }
  cli::cli_abort(
    c(
      paste(
        "{fun_type} function {.arg {fun_name}} must return an output",
        "the same length as it's input or length 1."
      ),
      `*` = "{.arg {fun_name}} recieved {a_length_friendly(x)} input.",
      `*` = "{.arg {fun_name}} returned {a_length_friendly(result)} output.",
      if (context$in_across) across_error_hint()
    ),
    call = error_call
  )
}

try_format_fun <- function(
    x,
    fun,
    env = rlang::caller_env(),
    x_name = rlang::caller_arg(x),
    fun_name = rlang::caller_arg(fun),
    error_header = "Can't format highlighted vector {.arg {x_name}}.",
    error_call = rlang::caller_env()
  ) {
  rlang::try_fetch(
    eval(fun(x), envir = env),
    error = function(cnd) {
      cnd$call <- rlang::call2(fun_name)
      cli::cli_abort(
        c(error_header, if (context$in_across) across_error_hint()),
        parent = cnd,
        call = error_call,
        class = "vlightr_error"
      )
    }
  )
}

# HACK: Check whether a function was called from `dplyr::across()`. The two-
# sided function syntax `test ~ formatter` does not play well with `across()`
# and results in confusing error messages.
#
# See this ticket for details: https://github.com/tidyverse/dplyr/issues/6892
# `across()` does not replace symbols within `function()` or `~` when it
# is one-sided, but does replace symbols within `~` if it's two sided.
in_across <- function() {
  rlang::try_fetch(
    {
      dplyr::cur_column()
      TRUE
    },
    error = function(cnd) FALSE
  )
}

across_error_hint <- function() {
  c(
    i = "Did you highlight a vector using a two-sided formula (e.g. `is.na(.x) ~ color('red')`) in {.fn dplyr::across}?",
    `*` = "Try using `.h` instead of `.x` in two-sided formulas within {.fn dplyr::across}.",
    `*` = "Read {.vignette vlightr::highlighting-and-dplyr} for details."
  )
}

context <- rlang::new_environment(list(in_across = FALSE))

# highlight format -------------------------------------------------------------

# TODO:
# - change the `highlight_format.class` methods below as required
# - make sure to credit `ivs` for these
#   - from here: https://github.com/DavisVaughan/ivs/blob/main/R/format.R

highlight_format <- function(x) {
  UseMethod("highlight_format")
}

highlight_format.default <- function(x) {
  format(x)
}

highlight_format.logical <- function(x) {
  format(x, trim = TRUE)
}

highlight_format.integer <- function(x) {
  format(x, trim = TRUE, scientific = FALSE)
}

highlight_format.double <- function(x) {
  format(x, trim = TRUE, scientific = FALSE)
}

highlight_format.character <- function(x) {
  format(x, justify = "none", na.encode = TRUE)
}

highlight_format.factor <- function(x) {
  format(x, justify = "none", na.encode = TRUE)
}

highlight_format.Date <- function(x) {
  format(x, format = "%Y-%m-%d")
}

highlight_format.POSIXt <- function(x) {
  format(x, format = "%Y-%m-%d %H:%M:%S")
}

# TODO: This prints `-1 hours` for a 1 hour difference, but
# we already have `<highlight<duration<hours>>[9]>` hours in
# the ptype. Might be useful to keep for tibble columns however...
highlight_format.difftime <- function(x) {
  format(x, trim = TRUE, scientific = FALSE)
}
