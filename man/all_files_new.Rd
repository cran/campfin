% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/files-new.R
\name{all_files_new}
\alias{all_files_new}
\title{Check if all files in a directory are new}
\usage{
all_files_new(path, glob = NULL, ...)
}
\arguments{
\item{path}{The path to a directory to check.}

\item{glob}{A pattern to search for files (e.g., "*.csv").}

\item{...}{Additional arguments passed to \code{\link[fs:dir_ls]{fs::dir_ls()}}.}
}
\value{
logical; Whether \code{\link[=all]{all()}} files in the directory have a modification
date equal to today.
}
\description{
Tests whether all the files in a given directory have a modification date
equal to the system date. Useful when repeatedly running code with a lengthy
download stage. Many state databases are updated daily, so new data can be
helpful but not always necessary. Set this function in an \code{if} statement.
}
\examples{
tmp <- tempdir()
file.create(tempfile(pattern = as.character(1:5)))
all_files_new(tmp)
}
