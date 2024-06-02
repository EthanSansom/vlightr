
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

``` r
x <- c(1L, 0L, NA, 1L)
x_hl <- highlight(x, is.na, color("red"))
print(x)
print(x_hl)
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README-/red-nas-dark.svg">
<img src="man/figures/README-/red-nas.svg" width="100%" /> </picture>

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.
