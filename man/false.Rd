% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{false}
\alias{false}
\title{Return the value \code{FALSE}}
\usage{
false(x)
}
\arguments{
\item{x}{Any object.}
}
\value{
The value \code{FALSE}.
}
\description{
This function always returns the value \code{FALSE}.
}
\examples{
# `false()` is useful for "turning-off" a conditional format
odd_even <- highlight_mult(
  1:6,
  .x \%\% 2 == 0 ~ wrap("<", ">"),
  .x \%\% 2 == 1 ~ wrap("[", "]")
)

# Even values are wrapped in "<>", odd in "[]"
odd_even

# Turn off the highlighting of odd values
tests(odd_even)[[2]] <- false
odd_even

}
