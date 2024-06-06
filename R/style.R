#' Generate a formatter function to style text
#'
#' @description
#'
#' vlightr provides some helpers to quickly generate new formatting functions
#' for use in the `formatters`, `init_formatter`, and `last_formatter` arguments
#' of [highlight()], [highlighter()], and friends.
#'
#' @param x `[character(1)]`
#'
#' For `bg()` and `color()`.
#'
#' @param left,right `[character(1)]`
#'
#' For `wrap()`.
#'
#' @return
#'
#' A function which returns a styled character or ANSI string vector.
#'
#' @family styles
#' @name stylers
#' @examples
#' color_red <- color("red")
#' color_red(1:3)
NULL

# TODO: Grouping these for now (with incomplete documentation), but they should
# be documented seperately. Maybe included in the same family however.
#
# - bg, background, colour, color can be documented together
# By themselves:
# - style
# - wrap
# - emph
# - make_formatter

# cli --------------------------------------------------------------------------

#' @name stylers
#' @export
bg <- function(x) {
  x <- tolower(check_is_string(x))
  switch(
    x,
    black0 = ,
    black = cli::bg_black,
    blue0 = ,
    blue = cli::bg_blue,
    cyan0 = ,
    cyan = cli::bg_cyan,
    green0 = ,
    green = cli::bg_green,
    magenta0 = ,
    magenta = cli::bg_magenta,
    red0 = ,
    red = cli::bg_red,
    white0 = ,
    white = cli::bg_white,
    yellow0 = ,
    yellow = cli::bg_yellow,
    black1 = ,
    bright_black = ,
    black_br = ,
    br_black = cli::bg_br_black,
    blue1 = ,
    bright_blue = ,
    blue_br = ,
    br_blue = cli::bg_br_blue,
    cyan1 = ,
    bright_cyan = ,
    cyan_br = ,
    br_cyan = cli::bg_br_cyan,
    green1 = ,
    bright_green = ,
    green_br = ,
    br_green = cli::bg_br_green,
    magenta1 = ,
    bright_magenta = ,
    magenta_br = ,
    br_magenta = cli::bg_br_magenta,
    red1 = ,
    bright_red = ,
    red_br = ,
    br_red = cli::bg_br_red,
    white1 = ,
    bright_white = ,
    white_br = ,
    br_white = cli::bg_br_white,
    yellow1 = ,
    bright_yellow = ,
    yellow_br = ,
    br_yellow = cli::bg_br_yellow,
    make_fallback_color(x, bg = TRUE)
  )
}

#' @name stylers
#' @export
background <- bg

#' @name stylers
#' @export
color <- function(x) {
  x <- tolower(check_is_string(x))
  switch(
    x,
    black0 = ,
    black = cli::col_black,
    blue0 = ,
    blue = cli::col_blue,
    cyan0 = ,
    cyan = cli::col_cyan,
    green0 = ,
    green = cli::col_green,
    magenta0 = ,
    magenta = cli::col_magenta,
    red0 = ,
    red = cli::col_red,
    white0 = ,
    white = cli::col_white,
    yellow0 = ,
    yellow = cli::col_yellow,
    black1 = ,
    bright_black = ,
    black_br = ,
    br_black = cli::col_br_black,
    blue1 = ,
    bright_blue = ,
    blue_br = ,
    br_blue = cli::col_br_blue,
    cyan1 = ,
    bright_cyan = ,
    cyan_br = ,
    br_cyan = cli::col_br_cyan,
    green1 = ,
    bright_green = ,
    green_br = ,
    br_green = cli::col_br_green,
    magenta1 = ,
    bright_magenta = ,
    magenta_br = ,
    br_magenta = cli::col_br_magenta,
    red1 = ,
    bright_red = ,
    red_br = ,
    br_red = cli::col_br_red,
    white1 = ,
    bright_white = ,
    white_br = ,
    br_white = cli::col_br_white,
    yellow1 = ,
    bright_yellow = ,
    yellow_br = ,
    br_yellow = cli::col_br_yellow,
    make_fallback_color(x)
  )
}

#' @name stylers
#' @export
colour <- color

# misc -------------------------------------------------------------------------

# TODO Implement:
# `x` is a string with instructions on how to make a styled vector.
# `make_formatter` is a highlighter generator like checkmate `qassert`.
# "{_/*[col][bg_col]}"
# - initial symbols are emphasis `_` -> underling, `/` -> italics, `*` -> bold, `-` -> strike-through
# - first [] is color (leave blank for no color) and second [] is background
# "prefix{...}suffix"
# - if [] or []
# - test outside of the opening bracket is pasted around the text
# - "<{}>" -> paste0("<", x, ">"), "<{[blue]}> -> cli::cli_col_blue(paste0("<", x, ">"))

#' @name stylers
#' @export
make_formatter <- function(x) {

}

#' @name stylers
#' @export
style <- function(x) {
  x <- tolower(check_is_string(x))
  switch(
    bold = cli::style_bold,
    dimmed = ,
    dim = cli::style_dim,
    blur = ,
    blurred = cli::style_blurred,
    italicized = ,
    italicize = ,
    italics ,
    italic = cli::style_italic,
    hide = ,
    hidden = cli::style_hidden,
    underline = cli::style_underline,
    strike = ,
    strikethrough = cli::style_strikethrough,
    inverted = ,
    inverse = cli::style_inverse,
    # TODO: Make a function which shows supported styles somewhere. Reference it
    #       in this error.
    cli::cli_abort(
      "Can't create style {.val {x}}.",
      class = "vlighter_error"
    )
  )
}

#' @name stylers
#' @export
wrap <- function(left = "[", right = "]") {
  left <- check_is_string(left)
  right <- check_is_string(right)
  return(function(x) paste0(left, x, right))
}

# Wrap in "[]", bold, and highlight (provide emphasis even if no color is available)
#' @name stylers
#' @export
emph <- function(wrap = TRUE, bg = TRUE, bold = TRUE) {
  call <- rlang::sym("x")
  if (check_is_bool(wrap)) call <- rlang::call2("paste0", "[", call, "]", .ns = "base")
  if (check_is_bool(bold)) call <- rlang::call2("style_bold", call, .ns = "cli")
  if (check_is_bool(bg)) call <- rlang::call2("bg_br_yellow", call, .ns = "cli")
  rlang::new_function(
    args = rlang::pairlist2(x = ),
    body = call,
    env = rlang::caller_env()
  )
}

# helpers ----------------------------------------------------------------------

make_fallback_color <- function(
    x,
    bg = FALSE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {
  # Taking ownership of the the `cli` error, since the message says a matrix can
  # be used, which is not true for these style functions. This is a little
  # dangerous, because there's no "cli_error_bad_color" class.
  rlang::try_fetch(
    cli::make_ansi_style(x, bg = bg),
    rlib_error_3_0 = function(cnd) {
      with_color <- if (bg) "with background color" else "with color"
      cli::cli_abort(
        # Thanks `cli::make_ansi_style` documentation for this message. The reference
        # to matrices is all that was removed, otherwise this is mostly the same.
        c(
          "Can't create a style {with_color} {.val {x}}. {.arg x} must be one of:",
          `*` = "a builtin {.pkg cli} style, e.g. {.val red} or {.val br_blue}",
          `*` = "an R colour name, see {.run grDevices::colors}",
          `*` = "a 6- or 8-digit hexadecimal color string, e.g. ⁠#ff0000"
        ),
        call = error_call,
        class = error_class
      )
    }
  )
}
