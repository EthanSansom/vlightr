

#' @export
print.vlightr_highlight <- function(
    x,
    ...,
    n = getOption("max.print"),
    align = c("left", "right", "center")
  ) {

  # Defining the entire `print` method so that `format(x)` can error before
  # `vctrs::obj_print_header(x)` has printed the header.
  formatted <- format(x, .x_name = rlang::caller_arg(x))
  n <- check_is_count(n)
  n_fmt <- length(formatted)
  if (n_fmt > n) {
    length(formatted) <- n
    print_more <- cli::col_silver(cli::format_inline(
      "[ Omitted {n_fmt - n} element{?s}. ",
      "Use {.code print(n = ...)} to see more. ]"
    ))
  } else {
    print_more <- NULL
  }

  vctrs::obj_print_header(x)
  print_ansi(
    x = formatted,
    align = rlang::arg_match(align),
    encode_string = is.character(get_data(x)),
    suffix = print_more
  )
  invisible(x)
}


# TODO: Fix initial white space!! See below for kind of weird printing. Maybe fix
# spacing as well. Not sure what's going on... OHH. Maybe make left justified for
# none-characters. I think that's the problem.
#
# library(lubridate)
# work_week_starts <- ymd("2020-01-05") + days(c(0, 8, 16))
# work_weeks <- interval(work_week_starts, work_week_starts + days(5:7))
# work_weeks_hl <- highlight_case(
#   work_weeks,
#   weekdays(int_start(.x)) == "Monday" ~ \(x) paste(x, "[Bad Start]"),
#   .x / days(1) > 5 ~ \(x) paste(x, "[Tough Week]"),
#   .format_once = FALSE
# )
# format(work_weeks_hl)
# print(work_weeks_hl)
#
# SEE ALSO
#
# x_multi <- highlight(
#   c(13, 1, 4, 6),
#   conditions = list(
#     ~ .x %% 2 == 0,
#     ~ .x %% 2 != 0,
#     ~ .x > 10
#   ),
#   formatters = list(
#     ~ paste(.x, "is even"),
#     ~ paste(.x, "is odd"),
#     ~ paste(.x, "is big and")
#   ),
#   last_formatter = wrap("[", "]")
# )
# print(x_multi)

print_ansi <- function(
    x,
    width = cli::console_width(),
    sep = " ",
    align = c("left", "right", "center"),
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

  # TODO: `encodeString` escapes the backslashes in ANSI escape sequences. Find
  #       a way to get around this.
  if (encode_string) x <- paste0('"', x, '"')
  # `nchar(NA)` is NA, so treating NA values as the string "NA" instead. This overwrites
  # bare NA values as formatted NA's (ex. `cli::col_red(NA)`, `paste0("<", NA, ">")`)
  # are not themselves NA.
  x[is.na(x)] <- "NA"
  element_width <- max(cli::ansi_nchar(x))
  prefix_width <- nchar(length(x)) + 2
  sep_width <- nchar(sep)

  n_per_line <- floor((width - prefix_width + sep_width) / (element_width + sep_width))
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
