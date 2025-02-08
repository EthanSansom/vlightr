vlightr
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

{vlightr} is a package for highlighting vectors. It provides a
<vlightr_highlight> superclass which enhances the `format()` and
`print()` method of generic vectors.

## Installation

⚠️ This package is still **under construction**. ⚠️

You can install the development version of vlightr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EthanSansom/vlightr")
```

## Features

``` r
library(vlightr)
library(dplyr, warn.conflicts = FALSE)
```

Want to identify an element of a vector? Highlight it with
`highlight()`.

``` r
x <- c(1, 8, 12, 4, 2)
maximum_hl <- highlight(x, .t = ~ .x == max(.x), .f = ~ bg("br_yellow"))
print(maximum_hl)
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/max-values-dark.svg">
<img src="man/figures/README-/max-values.svg" width="100%" /> </picture>

Highlighted elements change as the highlighted vector changes, so you
won’t lose them.

``` r
# `hl()` is shorthand for `highlight()`
sort(maximum_hl + hl(10))
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/sort-demo-dark.svg">
<img src="man/figures/README-/sort-demo.svg" width="100%" /> </picture>

Highlighted vectors can be used as tibble::tibble() columns too.

``` r
mtcars |>
    as_tibble(rownames = "make") |>
    mutate(
        make = highlight(hair_color, ~ grepl("Mazda", .x), toupper),
        vs = highlight_mult(am, 0 ~ label("automatic"), 1 ~ label("manual"))
    ) |>
    select(make, mpg, disp, vs)
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/tibble-demo-dark.svg">
<img src="man/figures/README-/tibble-demo.svg" width="100%" />
</picture>

Are you (or your boss) having a hard time finding that row you’re
looking for? Use `templight()` to temporarily highlight a vector by
index instead of using a test function.

``` r
mtcars |>
    as_tibble(rownames = "make") |>
    mutate(across(everything(), ~ templight(.x, make == "Datsun 710"))) |>
    select(make, mpg, disp, vs)
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/templight-dark.svg">
<img src="man/figures/README-/templight.svg" width="100%" /> </picture>

You can apply multiple conditional formats to a vector using
`highlight_mult()`. The left-hand-side is you a test function or a
literal value that you want to match and the right-hand-side is your
formatter function.

``` r
indicator <- highlight_mult(
    x = c(1, 0, 1, 0, 0, NA, 5),
    is.na ~ color("red"),
    0 ~ label("No"),
    1 ~ label("Yes"),
    !(.x %in% c(NA, 0, 1)) ~ paste(.x, "[?]")
)
print(indicator)
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/highlight-mult-dark.svg">
<img src="man/figures/README-/highlight-mult.svg" width="100%" />
</picture>

Simplify the code above using `highligh_case()`, which provides a
`dplyr::case_when()` style interface and conditionally formats elements
using at most one formatter.

``` r
indicator <- highlight_case(
    x = c(1, 0, 1, 0, 0, NA, 5),
    is.na ~ color("red"),
    0 ~ label("No"),
    1 ~ label("Yes"),
    true ~ paste(.x, "[?]") # Use `true()` to provide a default formatter
)
print(indicator)
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/highlight-case-dark.svg">
<img src="man/figures/README-/highlight-case.svg" width="100%" />
</picture>

If you want to re-use a highlight, turn it into a `highlighter()`.

``` r
indicator_highlighter <- as_highlighter(indicator)
indicator_highlighter(c(0, 1, NA, -9))
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/highlighter-dark.svg">
<img src="man/figures/README-/highlighter.svg" width="100%" />
</picture>

## Inspiration

This package is heavily inspired by the
[ivs](https://github.com/DavisVaughan/ivs) package, which implements
generic right-open intervals defined by a pair of parallel start and end
vectors.

As a testament to the genericity of the `ivs::iv`, here is an
ill-advised but perfectly legal interval vector.

``` r
library(ivs)
starts <- highlight(-3:2, ~ .x %% 2 == 0, ~ label("Even"))
ends <- highlight(c(-2, -1, 2, 5, 7, 8), ~ .x > 0, ~ paste0("+", .x))

# Make an iv() with highlighted `starts` and `ends`
iv(starts, ends)
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/ivs-inspo1-dark.svg">
<img src="man/figures/README-/ivs-inspo1.svg" width="100%" /> </picture>

``` r
# Manipulate your iv()
iv_groups(iv(starts, ends))
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/ivs-inspo2-dark.svg">
<img src="man/figures/README-/ivs-inspo2.svg" width="100%" /> </picture>

``` r
# Highlight your iv()
highlight(
  iv(starts, ends), 
  ~ (iv_end(.x) - iv_start(.x)) > hl(1),
  color("goldenrod")
)
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/ivs-inspo3-dark.svg">
<img src="man/figures/README-/ivs-inspo3.svg" width="100%" /> </picture>
