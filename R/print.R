# TODO: Document this!

#' @export
print.vlightr_highlight <- function(
    x,
    ...,
    n = getOption("max.print"),
    align = c("left", "right", "center"),
    quoted = FALSE
  ) {

  formatted <- format(x, .x_name = rlang::caller_arg(x))
  n <- check_is_count(n)
  align <- rlang::arg_match(align)
  n_fmt <- length(formatted)
  if (n_fmt > n) {
    length(formatted) <- n
    print_more <- cli::format_inline(
      "[ Printed `n = {n}` element{?s}, omitted {n_fmt - n}. ",
      "Use {.code print(n = ...)} to see more. ]"
    )
  } else {
    print_more <- NULL
  }

  vctrs::obj_print_header(x)
  print_ansi(
    x = formatted,
    # TODO: Check which direction the default print method aligns different
    # objects. For example, I think numeric is usually right aligned. Make
    # the default option a function of `get_data(x)` switching on type.
    align = align,
    encode_string = quoted && is.character(get_data(x)),
    suffix = print_more
  )
  invisible(x)
}

# TODO: Refine this a little bit, but I like the idea of using a `tibble()`
print_highlights <- function(x) {

  x <- check_is_highlight(x)
  x_data <- get_data(x)
  tests <- get_tests(x)

  formatted <- format(x, .x_name = rlang::caller_arg(x))
  formatted_with <- map(seq_along(x), \(x) numeric())

  # TODO: We'll want a method for <highlight_case>
  for (i in seq_along(tests)) {
    formatted_at <- call_format_fun(
      x = x_data,
      fun = tests[[i]],
      env = rlang::caller_env(),
      fun_name = paste0("tests(", x_name, ")[[", i, "]]"),
      fun_type = "test"
    )
    formatted_with[formatted_at] <- map(formatted_with[formatted_at], `c`, i)
  }

  unformatted_at <- map_lgl(formatted_with, rlang::is_empty)
  if (all(unformatted_at)) {
    print(
      tibble::tibble(
        location = character(),
        `formatters()` = character(),
        `ul()` = x_data[0],
        `hl()` = x[0]
      ),
      width = cli::console_width()
    )
    return(invisible(x))
  }

  # Find unique combinations of tests used on each element of `x`
  unique_formats_at <- which(!duplicated(formatted_with) & !unformatted_at)
  formats_used <- map_chr(formatted_with[unique_formats_at], commas)

  print(
    tibble::tibble(
      # `location` is more readable left-aligned as a <chr> column
      location = as.character(unique_formats_at),
      `formatters()` = formats_used,
      `ul()` = x_data[unique_formats_at],
      `hl()` = x[unique_formats_at]
    )[order_ragged(formatted_with[unique_formats_at]), ],
    width = cli::console_width()
  )
  return(invisible(x))
}

print_ansi <- function(
    x,
    width = cli::console_width(),
    sep = " ",
    align = "left",
    encode_string = FALSE,
    suffix = NULL,
    error_call = rlang::caller_env()
  ) {

  if (!is.character(x)) {
    stop_internal("`x` must be a character, not {.obj_type_friendly {x}}.")
  }
  if (!rlang::is_scalar_integerish(width) || is.na(width)) {
    stop_internal("`width` must be a count, not {.obj_type_friendly {width}}.")
  }
  if (!rlang::is_string(sep)) {
    stop_internal("`sep` must be a string, not {.obj_type_friendly {sep}}.")
  }
  if (length(align) != 1 || align %notin% c("left", "right", "center")) {
    stop_internal('`align` must be one of {c("left", "right", "center")}.')
  }

  if (rlang::is_empty(x)) {
    return(invisible())
  }

  # `encodeString()` escapes the backslashes in ANSI escape sequences. For now,
  # wrapping ANSI styled text in quotes. See if a better solution exists.
  if (encode_string) {
    if (any(cli::ansi_has_any(x))) {
      x <- paste0('"', x, '"')
    } else {
      x <- encodeString(x, quote = '"')
    }
  }
  # `nchar(NA)` is NA, so treating NA values as the string "NA" instead. Otherwise,
  # NA values are formatted incorrectly.
  x[is.na(x)] <- "NA"
  element_width <- max(cli::ansi_nchar(x))
  prefix_width <- nchar(length(x)) + 2
  sep_width <- nchar(sep)

  # `max()` to prevent `n_per_line <- 0` when really wide elements are present
  n_per_line <- max(1, floor((width - prefix_width + sep_width) / (element_width + sep_width)))
  n <- length(x)

  # Prefix width was set under the assumption that the largest prefix would be
  # "[{length(n)}]". Adjust the width if the actual last prefix is shorter.
  if (nchar(n) != nchar(n - n_per_line + 1)) prefix_width <- prefix_width - 1

  # Each prefix is the "[index]" of the element which starts the line.
  #  [1] "a" "b" "c" "d" "e"
  #  [6] "f" "g" "h" "i" "j"
  # [11] "k" "l"
  prefixes <- formatC(paste0("[", seq(1, n, n_per_line), "]"), width = prefix_width)
  elements <- cli::ansi_align(x, width = element_width, align = align)
  lines <- vapply(chunk(elements, n_per_line), paste, character(1L), collapse = sep)

  writeLines(paste(prefixes, lines, sep = sep))
  if (!is.null(suffix)) cli::cat_line(suffix)
}

# Thanks: https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks
chunk <- function(x, n) split(x, ceiling(seq_along(x) / n))
