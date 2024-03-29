% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grepvec.R
\name{by_hay}
\alias{by_hay}
\alias{by_hay.grepvec}
\title{For each haystack string, get the needle patterns that matched to it.}
\usage{
by_hay(x, value = FALSE, ...)

\method{by_hay}{grepvec}(x, value = FALSE, needles = x$needles, ...)
}
\arguments{
\item{x}{\code{grepvec} object returned from \link{grepvec} when out = "object".}

\item{value}{A logical(0), default FALSE. If FALSE, then at each list index
is a vector containing the integer indices of matches in needles. If TRUE,
the actual matched patterns in needles are used instead.}

\item{...}{Additional arguments to be passed to methods.}

\item{needles}{Character vector containing the needle patterns that were
searched for in haystacks by \link{grepvec}.
By default, the needles used in the \link{grepvec} call are used.}
}
\value{
List of length(haystacks) of integer or character vectors.
Each list index corresponds to the haystack string at that index, and the
list elements are the indices or patterns from needles which matched to the
given haystack string.
The list element will be a vector of length 0 (integer(0) or character(0))
if no needle matches were found for the haystack.
}
\description{
Get the results of \link{grepvec} as a list, length(haystacks), containing the
indices or values of matched needles.
Applying this function to a \link{grepvec} object is equivalent to calling
\link{grepvec} with \code{out = "needles"}.
}
