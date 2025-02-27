% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{is_highlightable}
\alias{is_highlightable}
\title{Test if the object can be highlighted}
\usage{
is_highlightable(x)
}
\arguments{
\item{x}{An object to test.}
}
\value{
\code{TRUE} if \code{x} is can be highlighted, \code{FALSE} otherwise.
}
\description{
This function returns \code{TRUE} for objects which can be highlighted (e.g. via
\code{\link[=highlight]{highlight()}}) and \code{FALSE} otherwise. An object \code{x} can be highlighted if:
\itemize{
\item \code{x} is not a \link{data.frame} or subclass thereof
\item \code{x} is not a bare \link{list}
\item \code{x} is otherwise a vector, as defined by \code{\link[vctrs:vector-checks]{vctrs::obj_is_vector()}}
}
}
