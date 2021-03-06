% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is-binary.R
\name{is_binary}
\alias{is_binary}
\title{Check if Binary}
\usage{
is_binary(x, na.rm = TRUE)
}
\arguments{
\item{x}{A vector.}

\item{na.rm}{logical; Should NA be ignored, \code{TRUE} by default.}
}
\value{
\code{TRUE} if only 2 unique values.
}
\description{
Uses \code{\link[dplyr:n_distinct]{dplyr::n_distinct()}} to check if there are only two unique values.
}
\examples{
if (is_binary(x <- c("Yes", "No"))) x == "Yes"
}
