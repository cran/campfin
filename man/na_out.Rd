% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/counters.R
\name{na_out}
\alias{na_out}
\title{Remove out}
\usage{
na_out(x, y, ignore.case = FALSE)
}
\arguments{
\item{x}{A vector to check.}

\item{y}{A vector to compare against.}

\item{ignore.case}{logical; if \code{FALSE}, the pattern matching is case
sensitive and if \code{TRUE}, case is ignored during matching.}
}
\value{
The vector \code{x} missing any values not in \code{y}.
}
\description{
Set \code{NA} for the values of \code{x} that are \verb{\%out\%} of the vector \code{y}.
}
\examples{
na_out(c("VT", "NH", "ZZ", "ME"), state.abb)
na_out(1:10, seq(1, 10, 2))
}
\seealso{
Other counting wrappers: 
\code{\link{count_diff}()},
\code{\link{count_in}()},
\code{\link{count_na}()},
\code{\link{count_out}()},
\code{\link{na_in}()},
\code{\link{na_rep}()},
\code{\link{prop_distinct}()},
\code{\link{prop_in}()},
\code{\link{prop_na}()},
\code{\link{prop_out}()},
\code{\link{what_in}()},
\code{\link{what_out}()}
}
\concept{counting wrappers}
