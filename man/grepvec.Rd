% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grepvec.R
\name{grepvec}
\alias{grepvec}
\title{grepvec: Search a vector of strings for matches in a vector of patterns.}
\usage{
grepvec(
  needles,
  haystacks,
  matchrule = c("all", "first", "last"),
  fixed = FALSE,
  ignore_case = FALSE,
  value = FALSE,
  out = c("needles", "haystacks", "object")
)
}
\arguments{
\item{needles}{A character vector of expressions which will be searched for
in haystacks as fixed strings or regular expressions,
depending on the \code{fixed} argument.}

\item{haystacks}{A character vector of strings which will be searched over
for matches in needles.}

\item{matchrule}{A character(1), default "all". If "first", the first match
found in needles is returned for each string in haystack. This may improve
performance for large needle vectors since the search process for each
string can stop as soon as a match is found.
If "last", the search process will continue until every string in
needles has been tested, and the last found match will be returned.
If "all", a vector is returned containing the indices of all found matches
in \code{needles} (with the same search time as "last").}

\item{fixed}{A logical(1), default FALSE. If TRUE, treat each string in
needles as an exact string to find. If FALSE, needles are treated as
regular expressions.}

\item{ignore_case}{A logical(1), default FALSE. If TRUE, the pattern matching
will be case-insensitive.}

\item{value}{A logical(1), default FALSE. If TRUE, actual strings are
returned instead of indices. Otherwise, integer indices are returned.
The other aspects of the returned object are determined by \code{out}. \code{value}
has no effect when \code{out = "object"}.}

\item{out}{A character(1), default "needles", declaring the output format.
By default, grepvec returns a list of length(haystacks), where
each list element is an integer vector of the indices of matches in the
needles vector (unless \code{value = TRUE}, in which case the actual needle
patterns are returned).  If \code{out = "haystacks"},
the output will be transposed to have length(needles) and each list
element will be a vector of indices corresponding to the haystack strings
that matched to the needle pattern at that index (unless \code{value = TRUE}).
If \code{out = "object"}, an S3 object (class "grepvec")
is returned, which can be used together with the \link{by_ndl} and \link{by_hay}
methods to obtain the results in different formats.}
}
\value{
A list of integer or character vectors, or an S3 class object.
See the \code{out} and \code{value} arguments.
}
\description{
grepvec searches for needles in haystacks. Needles are one or more
regular expression or fixed strings, and haystacks are one or more strings to
search over.
The strings in the needles vector are compiled as regular expressions, unless
\code{fixed = TRUE}.
grepvec can return one or all matches based on the \code{matchrule} argument.
By default, a list of length(haystacks) is returned, containing
at each index an integer vector of the indices in needles which successfully
matched. The value returned by grepvec is flexible and can be adjusted by
the \code{out} and \code{value} arguments.
}
\examples{
grepvec(needles = c("some", "other", "string"),
        haystacks = c("some string 1", "another string"),
        fixed = TRUE)
grepvec(c("^h", ".ell.", "ello$"), c("hello", "jelly"))
grepvec(c("^h", ".ell.", "ello$"), c("hello", "jelly"),
        out = "needles", value = TRUE)
grepvec(c("one", "possib", "many"),
        c("Some text which might possibly contain one of many keywords",
          "Another string without many words",
          "A third impossibly boring string",
          "Done"),
        matchrule = "first",
        out = "hay",
        value = TRUE)
x <- grepvec(letters, c("abc", "123"), out = "object")
head(by_hay(x, value = TRUE))
head(by_ndl(x, value = TRUE))

}
