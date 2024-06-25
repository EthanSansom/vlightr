
# TODO: I think `.x_name = rlang::caller_arg(x)` is vestigial at this point. See
#       if it can be safely removed.
#
#' @export
format.vlightr_highlight <- function(x, ..., .x_name = rlang::caller_arg(x)) {

  # Returning early on empty inputs has the added benefit of `validate_highlight`
  # not checking `formatters` or `conditions` when the vector is empty (such as
  # when `vctrs::vec_ptype()` is used).
  x_data <- get_data(x)
  if (rlang::is_empty(x_data)) {
    return(character())
  }

  init_formatter <- get_init_formatter(x)
  last_formatter <- get_last_formatter(x)

  if (is.null(init_formatter)) {
    formatted <- highlight_format(x_data)
  } else {
    formatted <- evalidate_highlight_fun(
      input = x_data,
      fun = init_formatter,
      fun_name = 'attr(,"init_formatter")',
      fun_type = "formatter"
    )
  }

  format_once <- get_format_once(x)
  is_unformatted <- rep(TRUE, length(x))

  prededence <- order(get_precedence(x))
  conditions <- get_conditions(x)
  formatters <- get_formatters(x)
  for (i in prededence) {
    format_at <- evalidate_highlight_fun(
      input = x_data,
      fun = conditions[[i]],
      fun_name = glue::glue('attr(,"conditions")[[{i}]]'),
      fun_type = "condition"
    )
    if (format_once) {
      format_at <- is_unformatted & format_at
      is_unformatted <- is_unformatted & !format_at
    }
    if (any(format_at)) {
      formatted[format_at] <- evalidate_highlight_fun(
        input = formatted[format_at],
        fun = formatters[[i]],
        fun_name = glue::glue('attr(,"formatters")[[{i}]]'),
        fun_type = "formatter"
      )
    }
  }
  if (is.null(last_formatter)) {
    return(formatted)
  }
  evalidate_highlight_fun(
    input = formatted,
    fun = last_formatter,
    fun_name = 'attr(,"last_formatter")',
    fun_type = "formatter"
  )
}

# TODO: I still think this could have better errors
# - might be worth while to create separate functions for "condition" and "formatter"
# - add more specific error messages, with better context
# - can we see whether or not we're within an across statement
#
# TODO:
# - add an informative error for when you're within `dplyr::across()`, used a `*_case()`
#   function, and hit a `formatter` length error
evalidate_highlight_fun <- function(
    input,
    fun,
    fun_name,
    fun_type = c("condition", "formatter"),
    fun_env = rlang::caller_env(),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
) {

  in_across <- in_across()
  result <- rlang::try_fetch(
    eval(fun(input), envir = fun_env),
    error = function(cnd) {
      cnd$call <- rlang::call2(fun_name)
      cli::cli_abort(
        c(
          "Highlighted vector has a malformed {.fn format} method.",
          if (in_across) across_error_hint()
        ),
        parent = cnd,
        call = error_call,
        class = error_class
      )
    }
  )

  is_condition <- rlang::arg_match(fun_type) == "condition"
  target_class <- if (is_condition) "logical" else "character"
  is_tgt_class <- if (is_condition) is.logical(result) else is.character(result)

  input_length <- length(input)
  if (is_tgt_class && length(result) %in% c(1, input_length)) {
    if (is_condition) result[is.na(result)] <- FALSE
    return(vctrs::vec_recycle(result, input_length))
  }

  if (!is_tgt_class) {
    cli::cli_abort(
      c(
        paste(
          "{upper1(fun_type)} function {.fn {fun_name}} must produce a",
          "{target_class} vector, not {.obj_type_friendly {result}}."
        ),
        if (in_across) across_error_hint()
      ),
      call = error_call,
      class = error_class
    )
  }

  cli::cli_abort(
    c(
      paste(
        "{upper1(fun_type)} {.fn {fun_name}} must produce an output of the same",
        "length of it's input."
      ),
      i = "{.fn {fun_name}} recieved a length {input_length} input.",
      i = "{.fn {fun_name}} produced a length {length(result)} output.",
      if (in_across) across_error_hint()
    ),
    call = error_call,
    class = error_class
  )
}

# Hack for checking whether a function is being called from within `dplyr::across`.
# This is only used to provide a hint when a `*_case()` function causes an error
# because of using `.fns = ~` in `dplyr::across()`.
in_across <- function() {
  tryCatch(
    {
      dplyr::cur_column()
      TRUE
    },
    error = function(cnd) FALSE
  )
}

across_error_hint <- function() {
  c(
    i = "Did you highlight a vector using a formula (`.fns = ~...`) in {.fn dplyr::across}?",
    `*` = "vlightr {.fn *_case} functions are incompatible with this syntax.",
    " " = "# Good",
    " " = 'dplyr::across(x, \\(col) highlight_case(col, is.na(.x) ~ color("red")(.x)))',
    " " = "# Bad",
    " " = 'dplyr::across(x, ~highlight_case(.x, is.na(.x) ~ color("red")(.x)))'
  )
}

#' @export
describe_highlight <- function(x) {

  conditions <- get_conditions(x)
  x_data <- get_data(x)
  if (rlang::is_empty(conditions)) {
    cli::cli_text("A {.cls {class(x_data)}} vector with no conditional formatting.")
    return(invisible(x))
  }

  x_name <- rlang::caller_arg(x)
  formatted <- format(x, .x_name = x_name)
  precedence <- order(get_precedence(x))
  description <- get_description(x)[precedence]
  conditions <- conditions[precedence]

  format_once <- get_format_once(x)
  is_unformatted <- rep(TRUE, length(x))
  formatted_using <- vector("list", length(x))
  for (i in seq_along(conditions)) {
    format_at <- evalidate_highlight_fun(
      input = x_data,
      fun = conditions[[i]],
      fun_name = glue::glue('attr({x_name},"conditions")[[{i}]]'),
      fun_type = "condition"
    )
    if (format_once) {
      format_at <- is_unformatted & format_at
    }
    is_unformatted <- is_unformatted & !format_at
    formatted_using[format_at] <- lapply(formatted_using[format_at], `c`, i)
  }

  any_formatted <- !all(is_unformatted)
  if (any_formatted) {
    unique_formats_at <- !duplicated(formatted_using) & !is_unformatted

    fmts <- vapply(
      formatted_using[unique_formats_at],
      oxford,
      character(1L),
      last = "and"
    )
    elms <- format(x_data[unique_formats_at])

    # Each example looks like "1, ..., and n: {element} -> {format(element)}"
    examples <- paste(
      format(paste0(fmts, ":"), width = max(nchar(fmts)) + 1),
      format(elms, width = max(nchar(elms))),
      "->",
      formatted[unique_formats_at]
    )
    examples <- examples[order_ragged(formatted_using[unique_formats_at])]
  }

  cli::cli_text(
    "A conditionally formatted {.cls {class(x_data)}} vector with ",
    "{length(description)} conditional format{?s}:"
  )
  cli::cli_ol(description)
  if (any_formatted) {
    cli::cat_line()
    cli::cli_text("Element formatted using:")
    # Not using `cli::cli_ul` for bulleted list, since it removes whitespace
    # which I don't want here.
    cli::cat_line(paste(cli::symbol$bullet, examples))
  }
  return(invisible(x))
}

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

highlight_format.difftime <- function(x) {
  format(x, trim = TRUE, scientific = FALSE)
}
