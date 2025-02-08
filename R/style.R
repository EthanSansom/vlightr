# todos ------------------------------------------------------------------------

#### New helpers
#
# trim(nchar = 20, end = "...") - creates a function to trim x to a limit of
# nchar if it’s above and insert an ending of end (e.g. “this is a long word and
# will…”)
#
# pad(nchar = 10, align = "center", char = " ") - generates a padding function

# stylers ----------------------------------------------------------------------

#' Generate a formatter function to style text
#'
#' @description
#'
#' Function factories used to generate new formatting functions for use in
#' [highlight()], [highlighter()], and friends.
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
#' For `wrap()`, `label()` a prefix (`left`) or suffix `right` that the output function
#' pastes onto a character vector. By default, `left` is `"["` and `right`
#' is `"]"`.
#'
#' @param string `[character(1)]`
#'
#' For `label`, the label (as a word) that the output function pastes onto
#' a character vector.
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
#'
#' yes <- label("Yes")
#' yes(1)
NULL

# todos ------------------------------------------------------------------------

#### Split up the documentation above to just include the `backgroun()` and
# `color()` functions (+ rep and shorthands).

# cli --------------------------------------------------------------------------

#' @name stylers
#' @export
background <- function(x) {
  switch(
    check_is_string(x),
    black = cli::bg_black,
    blue = cli::col_blue,
    cyan = cli::bg_cyan,
    green = cli::bg_green,
    magenta = cli::bg_magenta,
    red = cli::bg_red,
    white = cli::bg_white,
    yellow = cli::bg_yellow,
    bright_black = ,
    br_black = cli::bg_br_black,
    bright_blue = ,
    br_blue = cli::bg_br_blue,
    bright_cyan = ,
    br_cyan = cli::bg_br_cyan,
    bright_green = ,
    br_green = cli::bg_br_green,
    bright_magenta = ,
    br_magenta = cli::bg_br_magenta,
    bright_red = ,
    br_red = cli::bg_br_red,
    bright_white = ,
    br_white = cli::bg_br_white,
    bright_yellow = ,
    br_yellow = cli::bg_br_yellow,
    make_fallback_color(x, bg = TRUE)
  )
}

#' @name stylers
#' @export
bg <- background

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

# TODO: After testing, make a background version
# TODO: Document and export
color_rep <- function(...) {
  # Allowing each `...` to be a character, makes calling with an external vector
  # easier. E.g. `color_rep("orange", "black")` or `color_rep(halloween)`.
  dots <- rlang::list2(...)
  map(
    seq_along(dots),
    \(i) check_must_not(
      x = dots[[i]],
      x_name = dot_name(i),
      test = is.character,
      must = "be a character vector"
    )
  )
  crayons <- map(vctrs::list_unchop(dots), color)
  n_crayons <- length(crayons)
  color_rep_scalar <- function(x) {
    s <- strsplit(cli::ansi_strip(x), "")[[1]]
    color_at <- nchar(trimws(s)) > 0
    s[color_at] <- mapply(
      s[color_at],
      which(color_at),
      FUN = \(letter, i) crayons[[mod_index(i, n_crayons)]](letter)
    )
    paste(s, collapse = "")
  }
  function(x) {
    unname(map_chr(x, color_rep_scalar))
  }
}

colour_rep <- color_rep

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

#' @name stylers
#' @export
label <- function(string, left = "[", right = "]") {
  left <- check_is_string(left)
  right <- check_is_string(right)
  string <- check_is_string(string)
  return(function(x) paste0(x, " ", left, string, right))
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
          valid_colors_bullets()
        ),
        call = error_call,
        class = error_class
      )
    }
  )
}

valid_colors_bullets <- function() {
  c(
    `*` = "a builtin {.pkg cli} style, e.g. {.val red} or {.val br_blue}",
    `*` = "an R colour name, see {.run grDevices::colors}",
    `*` = "a 6- or 8-digit hexadecimal color string"
  )
}
