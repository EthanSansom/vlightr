#' @export
format.vlightr_highlight <- function(x, ..., .x_name = rlang::caller_arg(x)) {

  x_data <- get_data(x)
  init_formatter <- get_init_formatter(x)
  last_formatter <- get_last_formatter(x)

  if (is.null(init_formatter)) {
    formatted <- highlight_format(x_data)
  } else {
    formatted <- evalidate_highlight_fn(
      x = x_data,
      fn = init_formatter,
      x_name = .x_name,
      fn_name = 'attr(,"init_formatter")',
      fn_is = "formatter"
    )
  }

  format_once <- get_format_once(x)
  is_unformatted <- rep(TRUE, length(x))

  prededence <- order(get_precedence(x))
  conditions <- get_conditions(x)
  formatters <- get_formatters(x)
  for (i in prededence) {
    format_at <- evalidate_highlight_fn(
      x = x_data,
      fn = conditions[[i]],
      x_name = .x_name,
      fn_name = glue::glue('attr(,"conditions")[[{i}]]'),
      fn_is = "condition"
    )
    if (format_once) {
      format_at <- is_unformatted & format_at
      is_unformatted <- is_unformatted & !format_at
    }
    formatted[format_at] <- evalidate_highlight_fn(
      x = formatted[format_at],
      fn = formatters[[i]],
      x_name = .x_name,
      fn_name = glue::glue('attr(,"formatters")[[{i}]]'),
      fn_is = "formatter"
    )
  }
  if (is.null(last_formatter)) {
    return(formatted)
  }
  evalidate_highlight_fn(
    x = formatted,
    fn = last_formatter,
    x_name = .x_name,
    fn_name = 'attr(,"last_formatter")',
    fn_is = "formatter",
    error_class = "vlightr_last_formatter_error"
  )
}

# TODO: Work on the error call here, it can be hard to tell what's going on
#       when the user supplied functions fail.
evalidate_highlight_fn <- function(
    x,
    fn,
    fn_name,
    fn_is = c("condition", "formatter"),
    fn_env = rlang::caller_env(),
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {

  fn_is <- rlang::arg_match(fn_is)
  result <- rlang::try_fetch(
    eval(fn(x), envir = fn_env),
    error = function(cnd) {
      # `cnd$call` won't be a valid call name most of the time, but will appear
      # as something like "attr(,"conditions")[[1]]()", which will provide more
      # context in the error chain.
      cnd$call <- rlang::call2(fn_name)
      cli::cli_abort(
        "{.cls vlightr_highlight} vector has a malformed {.code format()} method.",
        parent = cnd,
        call = error_call,
        class = error_class
      )
    }
  )

  is_valid_class <- switch(
    fn_is,
    condition = is.logical,
    formatter = is.character
  )
  if (is_valid_class(result) && length(result) %in% c(1, length(x))) {
    if (fn_is == "condition") {
      result <- !is.na(result) & result
    }
    return(vctrs::vec_recycle(result, length(x)))
  }

  valid_class <- switch(fn_is, condition = "logical", formatter = "character")
  header <- paste0(
    "{upper1(fn_is)} `", fn_name, "` must produce a {.cls {valid_class}} ",
    "vector of length 1 or `length({x_name})`."
  )
  type_bullet <- if (!is_valid_class(result)) {
    "Produced {.obj_type_friendly {result}}."
  }
  len_bullets <- if (length(result) %notin% c(1, length(x))) {
    c(
      i = "Produced a length {length(result)} result.",
      i = "{.arg {x_name}} has length {length(x)}."
    )
  }
  cli::cli_abort(
    c(
      header,
      x = type_bullet,
      len_bullets
    ),
    class = error_class,
    call = error_call
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
    format_at <- evalidate_highlight_fn(
      x = x_data,
      fn = conditions[[i]],
      x_name = x_name,
      fn_name = glue::glue('attr({x_name},"conditions")[[{i}]]'),
      fn_is = "condition"
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
    examples <- mapply(
      formatted_element = formatted[unique_formats_at],
      formats_used = formatted_using[unique_formats_at],
      \(formatted_element, formats_used) {
        glue::glue(
          "Element formatted using {oxford(formats_used, last = 'and')}: ",
          "{formatted_element}"
        )
      }
    )

    # Re-order according to the earliest formatters used for each example. A
    # `sort_ragged <- \(x) x[order_ragged(x)]` would do the following:
    #> formatted_using <- list(c(1, 2), 1, c(2, 4), c(2, 1))
    #> sort_ragged(formatted_using) -> list(1, c(1, 2) c(2, 3), c(2, 4))
    examples <- examples[order_ragged(formatted_using[unique_formats_at])]
  }

  cli::cli_text(
    "A conditionally formatted {.cls {class(x_data)}} vector with ",
    "{length(description)} conditional format{?s}:"
  )
  cli::cli_ol(description)
  if (any_formatted) {
    cli::cat_line()
    cli::cli_text("Examples:")
    cli::cli_ul(examples)
  }
  return(invisible(x))
}

# TODO:
# - change the `highlight_format.class` methods below as required
# - make sure to credit `ivs` for these
#   - from here: https://github.com/DavisVaughan/ivs/blob/main/R/format.R

#' @export
highlight_format <- function(x) {
  UseMethod("highlight_format")
}

#' @export
highlight_format.default <- function(x) {
  format(x)
}

#' @export
highlight_format.logical <- function(x) {
  format(x, trim = TRUE)
}

#' @export
highlight_format.integer <- function(x) {
  format(x, trim = TRUE, scientific = FALSE)
}

#' @export
highlight_format.double <- function(x) {
  format(x, trim = TRUE, scientific = FALSE)
}

#' @export
highlight_format.character <- function(x) {
  format(x, justify = "none", na.encode = TRUE)
}

#' @export
highlight_format.factor <- function(x) {
  format(x, justify = "none", na.encode = TRUE)
}

#' @export
highlight_format.Date <- function(x) {
  format(x, format = "%Y-%m-%d")
}

#' @export
highlight_format.POSIXt <- function(x) {
  format(x, format = "%Y-%m-%d %H:%M:%S")
}

#' @export
highlight_format.difftime <- function(x) {
  format(x, trim = TRUE, scientific = FALSE)
}
