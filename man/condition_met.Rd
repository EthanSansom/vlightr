% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/condition.R
\name{condition_met}
\alias{condition_met}
\title{Indicate the elements of a highlighted vector which meet a condition(s)}
\usage{
condition_met(x, which = TRUE, meets = c("all", "any", "none"))
}
\arguments{
\item{x}{\verb{[vlightr_highlight]}

A highlighted vector.}

\item{which}{\verb{[character / logical / numeric]}

Which conditions are used to subset the vector. Conditions can be selected by
index using a numeric or logical vector, or selected using the name of a
corresponding description(s) (\verb{attr(x, 'description)}).}

\item{meets}{\verb{[character(1)]}

One of \code{"all"}, \code{"any"}, or \code{"none"}. The output vector is \code{TRUE} for
elements of \code{x} which:
\itemize{
\item meet all of the specified conditions, if \code{"all"}
\item meet any of the specified conditions, if \code{"any"}
\item meet none of the specified conditions, if \code{"none"}
}}
}
\description{
\code{\link[=condition_met]{condition_met()}} returns a logical vector indicating which elements of \code{x}
meet the specified conditions (i.e. receive the same conditional formatting).
}
\examples{
# Define a highlight
summary_hl <- highlight_case(
  x = 1:5,
  .x == min(.x) ~ paste0("(Min = ", .x, ")"),
  .x == quantile(.x, 0.5) ~ paste0("(Mid = ", .x, ")"),
  .x == max(.x) ~ paste0("(Max = ", .x, ")"),
  .x > quantile(.x, 0.5) ~ paste("[ High", .x, "]"),
  .x < quantile(.x, 0.5) ~ paste("[ Low", .x, "]"),
  .description = c(
    "Min",
    "Middle",
    "Max",
    ">P50",
    "<P50"
  )
)

# Which element is the minimum?
condition_met(summary_hl, which = 1)

# Specify a condition by it's description
condition_met(summary_hl, which = "Min")

# Which elements are the minimum or maximum?
condition_met(summary_hl, which = c("Min", "Max"), meets = "any")
condition_met(summary_hl, which = c("Min", "Max"), meets = "none")
condition_met(summary_hl, which = c("Min", "Max"), meets = "all")
}
\seealso{
Other condition utilities: 
\code{\link{condition_id}()},
\code{\link{condition_keep}()},
\code{\link{condition_split}()}
}
\concept{condition utilities}
