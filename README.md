
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vlightr

<!-- badges: start -->
<!-- badges: end -->

vlightr is a package for conditionally formatting vectors. You can
`highlight()` a vector to style, colour, or otherwise re-format it’s
elements (if they meet some predicate) when the vector is printed or
formatted. Highlights are persistent, meaning highlighted vectors can be
manipulated (with minimal legwork) while maintaining their custom
formatting.

## Installation

You can install the development version of vlightr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EthanSansom/vlightr")
```

# Features

``` r
library(vlightr)
```

## Conditional Formatting

Apply a custom format to elements of `x` for which a condition returns
`TRUE`. For example, color `NA` values red (as they would appear in a
`tibble::tibble()`).

``` r
x <- c(1L, 0L, NA, 1L, 0L)
x_hl <- vlightr::highlight(x, is.na, vlightr::colour("red"))
print(x_hl)
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/red-nas1-dark.svg">
<img src="man/figures/README-/red-nas1.svg" width="100%" /> </picture>

## Manipulate Highlighted Data

Highlighted vectors play well with other highlighted vectors. The
shorthand `hl()` is useful for quick highlighting.

``` r
# Arithmetic
x_hl + vlightr::hl(10L)
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/operation-dark.svg">
<img src="man/figures/README-/operation.svg" width="100%" /> </picture>

``` r
# Assignment
x_hl[[1]] <- vlightr::hl(NA)
x_hl
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/assign-dark.svg">
<img src="man/figures/README-/assign.svg" width="100%" /> </picture>

``` r
# Coercion
c(x_hl, vlightr::hl(c(-1.5, NA)))
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/coerce-dark.svg">
<img src="man/figures/README-/coerce.svg" width="100%" /> </picture>

## Highlight, Unhighlight, Rehighlight

Highlighted vectors can’t be coerced or cast to other vector types. To
use a function which expect the highlight’s underlying vector type,
first `un_highlight()/ul()` to expose the highlighted data and then
`re_highlight()/rl()` to re-apply the conditional formatting.

``` r
x_hl |>
  ul() |>
  as.logical() |>
  rl(x_hl)
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/rehighlight1-dark.svg">
<img src="man/figures/README-/rehighlight1.svg" width="100%" />
</picture>

The “highlight-pipe” `%hl>%` wraps the `magrittr::%>%` to do this
automatically.

``` r
x_hl %hl>% as.logical()
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/rehighlight2-dark.svg">
<img src="man/figures/README-/rehighlight2.svg" width="100%" />
</picture>

## Multiple Conditional Formats

``` r
dummies <- vlightr::highlight(
  x = x,
  conditions = list(
    is.na, 
    ~ .x == 1, 
    ~ .x == 0
  ),
  formatters = list(
    vlightr::colour("red"), 
    ~ paste(.x, "[Yes]"),
    ~ paste(.x, "[No]")
  )
)
dummies
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/multiple-formats-dark.svg">
<img src="man/figures/README-/multiple-formats.svg" width="100%" />
</picture>

## Re-Use Highlights

``` r
dummy_highlighter <- vlightr::as_highlighter(dummies)
dummy_highlighter(c(0, 1, NA))
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/highlighter-dark.svg">
<img src="man/figures/README-/highlighter.svg" width="100%" />
</picture>

## Use `dplyr::case_when()` Style Syntax

``` r
bad_words <- c("darn", "gosh")
affirmations <- c("good job", "way-to-go")
message <- c("hey", "good job", "but", "darn", "please", "don't", "say", "gosh")

vlightr::highlight_case(
  message,
  .x %in% bad_words ~ \(x) strrep("X", nchar(x)),
  .x %in% affirmations ~ \(x) toupper(x),
  TRUE ~ cli::style_italic
)
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/case-syntax-dark.svg">
<img src="man/figures/README-/case-syntax.svg" width="100%" />
</picture>

## Highlight Arbitrary Vectors

Highlighted vectors are generic, meaning that S3 and S4 vector classes
from other packages are highlight-able.

``` r
library(lubridate, warn.conflicts = FALSE)

today <- ymd("2020-01-01")
meeting_times <- interval(
  today + hours(c(9, 11, 16)), 
  today + hours(c(10, 13, 17))
)
lunch_break <- interval(today + hours(12), today + hours(13))
is_during_lunch <- function(x) int_overlaps(x, lunch_break)

vlightr::highlight(meeting_times, is_during_lunch, cli::col_magenta)
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/generic-dark.svg">
<img src="man/figures/README-/generic.svg" width="100%" /> </picture>
