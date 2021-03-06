% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/counters.R
\name{\%out\%}
\alias{\%out\%}
\title{Inverted match}
\usage{
x \%out\% table
}
\arguments{
\item{x}{vector: the values to be matched. Long vectors are supported.}

\item{table}{vector or \code{NULL}: the values to be matched against.}
}
\value{
logical; if \code{x} is not present in \code{table}
}
\description{
\verb{\%out\%} is an inverted version of the infix \code{\%in\%} operator.
}
\details{
\verb{\%out\%} is currently defined as
\code{"\%out\%" <- function(x, table) match(x, table, nomatch = 0) == 0}
}
\examples{
c("A", "B", "3") \%out\% LETTERS
}
