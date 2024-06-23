% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/condition.R
\name{condition_split}
\alias{condition_split}
\title{Split a highlighted vector by conditions}
\usage{
condition_split(x, unlit = TRUE)
}
\arguments{
\item{x}{\verb{[vlightr_highlight]}

A highlighted vector to split.}

\item{unlit}{\verb{[logical(1)]}

Whether elements of the output list are un-highlighted. Defaults to \code{TRUE}.}
}
\description{
\code{\link[=condition_split]{condition_split()}} divides a highlighted vector \code{x} into the groups defined
by \code{attr(x, "conditions")}. Elements of \code{x} which receive the same conditional
formatting are grouped together.
}
\examples{
x <- highlight(1:5)

}
\concept{condition utilities}