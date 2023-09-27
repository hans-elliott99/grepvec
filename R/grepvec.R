#' Search a vector of strings for matches in a vector of patterns.
#'
#' `grepvec` searches each string in haystacks for every pattern in needles. By
#' default, the strings in the needles vector are compiled as regular
#' expressions. A list of length(haystacks) is returned, containing at each
#' index an integer vector of the indices in needles which successfully
#' matched. This can be adjusted via the matchrule argument.
#'
#' @section grepvec functions:
#' `grepvec(haystacks, needles, matchrule = "all", fixed = FALSE)`
#' `to.haystacks(results, haystacks)`
#' `to.needles(results, needles)`
#'
#' @docType package
#' @name grepvec
#' @useDynLib grepvec