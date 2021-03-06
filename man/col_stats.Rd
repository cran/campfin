% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/col-stats.R
\name{col_stats}
\alias{col_stats}
\alias{glimpse_fun}
\title{Apply a statistic function to all column vectors}
\usage{
col_stats(data, fun, print = TRUE)

glimpse_fun(data, fun, print = TRUE)
}
\arguments{
\item{data}{A data frame to glimpse.}

\item{fun}{A function to map to each column.}

\item{print}{logical; Should all columns be printed as rows?}
}
\value{
A tibble with a row for every column with the count and proportion.
}
\description{
Apply a counting summary function like \code{\link[dplyr:n_distinct]{dplyr::n_distinct()}} or \code{\link[=count_na]{count_na()}}
to every column of a data frame and return the results along with a
\emph{percentage} of that value.
}
\examples{
col_stats(dplyr::storms, dplyr::n_distinct)
col_stats(dplyr::storms, campfin::count_na)
}
