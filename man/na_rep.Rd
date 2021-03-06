% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/counters.R
\name{na_rep}
\alias{na_rep}
\title{Remove repeated character elements}
\usage{
na_rep(x, n = 0)
}
\arguments{
\item{x}{A vector to check.}

\item{n}{The minimum number times a character must repeat. If 0, the default,
then any string of one character will be replaced with \code{NA}. If greater
than 0, the string must contain greater than \code{n} number of repetitions.}
}
\value{
The vector \code{x} with \code{NA} replacing repeating character values.
}
\description{
Set \code{NA} for the values of \code{x} that contain a single repeating character and
no other characters.
}
\details{
Uses the regular expression \code{"^(.)\\\\1+$"}.
}
\examples{
na_rep(c("VT", "NH", "ZZ", "ME"))
}
\seealso{
Other counting wrappers: 
\code{\link{count_diff}()},
\code{\link{count_in}()},
\code{\link{count_na}()},
\code{\link{count_out}()},
\code{\link{na_in}()},
\code{\link{na_out}()},
\code{\link{prop_distinct}()},
\code{\link{prop_in}()},
\code{\link{prop_na}()},
\code{\link{prop_out}()},
\code{\link{what_in}()},
\code{\link{what_out}()}
}
\concept{counting wrappers}
