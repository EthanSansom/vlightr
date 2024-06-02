# cli --------------------------------------------------------------------------

#' @export
bg <- function(x) {
  switch(
    tolower(check_is_string(x)),
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
    # TODO: Use `cli::make_ansi_color` here!!!
    cli::cli_abort(
      "`x = {.val {x}}` is not a supported background colour.",
      class = "vlightr_error"
    )
  )
}

#' @export
background <- bg

#' @export
color <- function(x) {
  switch(
    tolower(check_is_string(x)),
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
    # TODO: Use `cli::make_ansi_color` here!!!
    cli::cli_abort(
      "`x = {.val {x}}` is not a supported background colour.",
      class = "vlightr_error"
    )
  )
}

#' @export
colour <- color

# misc -------------------------------------------------------------------------

# TODO Implement:
# `x` is a string with instructions on how to make a styled vector.
# `style` is a highlighter generator like checkmate `qassert`.
# "{_/*[col][bg_col]}"
# - initial symbols are emphasis `_` -> underling, `/` -> italics, `*` -> bold, `-` -> strike-through
# - first [] is color (leave blank for no color) and second [] is background
# "prefix{...}suffix"
# - if [] or []
# - test outside of the opening bracket is pasted around the text
# - "<{}>" -> paste0("<", x, ">"), "<{[blue]}> -> cli::cli_col_blue(paste0("<", x, ">"))

style <- function(x) {

}

#' @export
wrap <- function(left = "[", right = "]") {
  left <- check_is_string(left)
  right <- check_is_string(right)
  return(function(x) paste0(left, x, right))
}

# Wrap in "[]", bold, and highlight (provide emphasis even if no color is available)
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
