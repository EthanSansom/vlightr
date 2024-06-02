
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vlightr

<!-- badges: start -->
<!-- badges: end -->

vlightr is a package for

## Installation

You can install the development version of vlightr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EthanSansom/vlightr")
```

## Features

``` r
library(vlightr)
```

Conditionally format vector elements.

``` r
x <- c(1L, 0L, NA, 1L, 0L)
x_hl <- vlightr::highlight(x, is.na, vlightr::colour("red"))
x_hl
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/red-nas-dark.svg">
<img src="man/figures/README-/red-nas.svg" width="100%" /> </picture>

Highlighted vectors play well with other highlighted vectors of the same
type. The shorthand `hl()` is useful for quick highlighting.

``` r
x_hl + vlightr::hl(10L)
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/operation-dark.svg">
<img src="man/figures/README-/operation.svg" width="100%" /> </picture>

Coercion works as well.

``` r
c(x_hl, vlightr::hl(c(-1.5, NA)))
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/coerce-dark.svg">
<img src="man/figures/README-/coerce.svg" width="100%" /> </picture>

Apply multiple conditional formats.

``` r
vlightr::highlight(
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
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/multiple-formats-dark.svg">
<img src="man/figures/README-/multiple-formats.svg" width="100%" />
</picture>

Use `dplyr::case_when()` style syntax for multiple conditional formats.

``` r
vlightr::highlight_case(
  x = c(10, NA, -11, 2, -1, 8),
  is.na(.x) ~ \(x) cli::col_green("Don't Worry"),
  .x < 0 ~ vlightr::bg("bright_blue")
)
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/case-syntax-dark.svg">
<img src="man/figures/README-/case-syntax.svg" width="100%" />
</picture>

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. Hey!
