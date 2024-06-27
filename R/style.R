#' Generate a formatter function to style text
#'
#' @description
#'
#' Function factors used to generate new formatting functions for use in the
#' `formatters`, `init_formatter`, and `last_formatter` arguments of highlight
#' functions (e.g. [highlight()], [highlighter()]).
#'
#' @param x `[character(1)]`
#'
#' For `background()` and `color()`, the color that the output function applies
#' to the text background or text of a character vector, specified by a:
#'
#' - recognized R color name (see [grDevices::colors()])
#' - `cli` package color (e.g. `"yellow"`, `"br_blue"`)
#' - 6- or 8-digit hexadecimal color string (e.g. "#ff0000")
#'
#' For `style`, the text effect that the output function adds to the text of a
#' character vector, specified by a `cli` package style (e.g. `"bold"`,
#' `"italic"`, `"underline"`).
#'
#' See [cli::style_bold()] and friends for recognized `cli` styles and colors.
#'
#' @param left,right `[character(1)]`
#'
#' For `wrap()`, a prefix (`left`) or suffix `right` that the output function
#' pastes onto a character vector. By default, `left` is `"["` and `right`
#' is `"]"`.
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
#'
#' background_yellow <- background("yellow")
#' background_yellow(LETTERS[1:3])
#'
#' style_bold <- style("bold")
#' style_bold(c(TRUE, FALSE, NA))
#'
#' embrace <- wrap("(", ")")
#' embrace(c(2.2, 3.3, 4.4))
NULL

# cli --------------------------------------------------------------------------

#' @name stylers
#' @export
bg <- function(x) {
  switch(
    check_is_string(x),
    black = cli::col_black,
    blue = cli::col_blue,
    cyan = cli::col_cyan,
    green = cli::col_green,
    magenta = cli::col_magenta,
    red = cli::col_red,
    white = cli::col_white,
    yellow = cli::col_yellow,
    bright_black = ,
    br_black = cli::col_br_black,
    bright_blue = ,
    br_blue = cli::col_br_blue,
    bright_cyan = ,
    br_cyan = cli::col_br_cyan,
    bright_green = ,
    br_green = cli::col_br_green,
    bright_magenta = ,
    br_magenta = cli::col_br_magenta,
    bright_red = ,
    br_red = cli::col_br_red,
    bright_white = ,
    br_white = cli::col_br_white,
    bright_yellow = ,
    br_yellow = cli::col_br_yellow,
    make_fallback_color(x, bg = TRUE)
  )
}

#' @name stylers
#' @export
background <- bg

#' @name stylers
#' @export
color <- function(x) {
  switch(
    check_is_string(x),
    black = cli::col_black,
    blue = cli::col_blue,
    cyan = cli::col_cyan,
    green = cli::col_green,
    magenta = cli::col_magenta,
    red = cli::col_red,
    white = cli::col_white,
    yellow = cli::col_yellow,
    bright_black = ,
    br_black = cli::col_br_black,
    bright_blue = ,
    br_blue = cli::col_br_blue,
    bright_cyan = ,
    br_cyan = cli::col_br_cyan,
    bright_green = ,
    br_green = cli::col_br_green,
    bright_magenta = ,
    br_magenta = cli::col_br_magenta,
    bright_red = ,
    br_red = cli::col_br_red,
    bright_white = ,
    br_white = cli::col_br_white,
    bright_yellow = ,
    br_yellow = cli::col_br_yellow,
    make_fallback_color(x)
  )
}

#' @name stylers
#' @export
colour <- color

# misc -------------------------------------------------------------------------

#' @name stylers
#' @export
style <- function(x) {
  switch(
    check_is_string(x),
    bold = cli::style_bold,
    dim = cli::style_dim,
    blurred = cli::style_blurred,
    italic = cli::style_italic,
    hidden = cli::style_hidden,
    underline = cli::style_underline,
    strikethrough = cli::style_strikethrough,
    inverse = cli::style_inverse,
    cli::cli_abort(
      c(
        "Can't create style {.val {x}}.",
        i = paste(
          "{.arg x} must be the string: {.or {c('bold', 'dim', 'blurred'," ,
          "'italic', 'hidden', 'underline', 'strikethrough', 'inverse')}}."
        )
      ),
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

# helpers ----------------------------------------------------------------------

make_fallback_color <- function(
    x,
    bg = FALSE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {
  # Taking ownership of the the `cli` error, since the message says a matrix
  # can be used, which is not true for these style functions.
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
          `*` = "a 6- or 8-digit hexadecimal color string"
        ),
        call = error_call,
        class = error_class
      )
    }
  )
}
