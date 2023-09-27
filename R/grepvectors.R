# TODO: specify flags to pass to regcomp?

.matchrules <- c("all", "first", "last")

#' Search a vector of strings for matches in a vector of patterns.
#'
#' `grepvec` searches each string in haystacks for every pattern in needles. By
#' default, the strings in the needles vector are compiled as regular
#' expressions. A list of length(haystacks) is returned, containing at each
#' index an integer vector of the indices in needles which successfully
#' matched. This can be adjusted via the matchrule argument.
#'
#' @param haystacks A character vector of strings which will be searched over
#'   for sub-string / regex matches.
#' @param needles A character vector of expressions which will be searched for
#'   in haystacks as sub-strings or regexs, depending on the `fixed` argument.
#' @param matchrule Character, default "all". If "first", the first match found
#'   in needles is returned for each string in haystack. This may improve
#'   performance for large needle vectors since the search process for each
#'   string can stop as soon as a match is found.
#'   If "last", the search process will continue until every sub-string in
#'   needles has been tested, and the last found match will be returned.
#'   If "all", a vector is returned containing the indices of all found matches
#'   in `needles`.
#' @param fixed Logical, default FALSE. If TRUE, treat each string in needles as
#'   an exact sub-string to find. If FALSE, needles are treated as regular
#'   expressions.
#' @return A list, length(haystacks), of integer vectors, each vector containing
#'   the indices of succesfully matched needle strings (or 0 if no match is
#'   found).
#' @examples
#' grepvec(c("some string 1", "another string"),
#'         c("some", "other", "string"), fixed = TRUE)
#' grepvec(c("hello", "jelly"), c("^h", ".ell.", "ello$"))
#' grepvec(c("Some text which might possibly contain one of many keywords"),
#'         c("one", "possib", "many"))
#' @useDynLib grepvec, grepvec_regex, grepvec_fixed
#'
#' @export
grepvec <- function(haystacks,
                    needles,
                    matchrule = c("all", "first", "last"),
                    fixed = FALSE) {
    stopifnot(is.character(haystacks))
    stopifnot(is.character(needles))
    stopifnot(is.logical(fixed))
    stopifnot(is.character(matchrule))
    if (all(matchrule == .matchrules)) {
        matchrule <- "all"
    }
    if (! matchrule %in% .matchrules) {
        stop(paste("Provide one of the following for argument 'matchrule':",
                   paste(.matchrules, collapse = ", ")
        ))
    }
    .matchrule <- switch(matchrule,
                         all   = 0L,
                         first = 1L,
                         last  = 2L)
    # grepvec_* (haystacks, needles, matchrule)
    if (fixed)
        x <- .Call("grepvec_fixed",
                   haystacks, needles, .matchrule)
    else
        x <- .Call("grepvec_regex",
                   haystacks, needles, .matchrule)

    return(x)
}


#' Convert index(index) list into index(char) list
.idx2char <- function(idxlist, charvec) {
    x <- lapply(idxlist, \(vec) if (any(vec != 0)) vec else NULL)
    x <- lapply(x, \(vec) charvec[vec])
    return(x)
}

#' Get the list of needle patterns that matched to each haystack string.
#'
#' Converts grepvec results from the list of indices into a list of the needle
#' patterns, length(haystacks).
#'
#' @param results List returned from grepvec, where each list index corresponds
#'   to a haystack string and each list element is an integer vector with the
#'   indices of the needle patterns that matched the haystack string.
#' @param needles Character vector containing the needle patterns that were
#'   searched for in haystacks by grepvec.
#' @return List of length(haystacks) of character vectors. Each list index
#'   corresponds to the haystack string at that index, and the list elements are
#'   the patterns from needles which matched to the given haystack string.
#'
#' @export
to.needles <- function(results, needles) {
    stopifnot(is.list(results))
    stopifnot(is.character(needles))
    return(.idx2char(results, needles))
}


#' Get the list of haystack strings that matched to each needle pattern.
#'
#' Converts grepvec results from a list of indices, length(haystacks), into a
#' list of haystack strings, length(needles).
#'
#' @param results List returned from grepvec, where each list index corresponds
#'   to a haystack string and each list element is an integer vector with the
#'   indices of the needle patterns that matched the haystack string.
#' @param haystacks Character vector containing the haystack strings that were
#'   searched over by grepvec.
#' @return List of length(needles) of character vectors. Each list index
#'   corresponds to the needle pattern at that index, and the list elements are
#'   the strings from haystack which matched to the given needle pattern.
#'
#' @export
to.haystacks <- function(results, haystacks) {
    stopifnot(is.list(results))
    stopifnot(is.character(haystacks))
    names(results) <- seq_along(results) #stack requires names
    x <- with(stack(results), split(ind, values))
    names(x) <- NULL
    return(.idx2char(x, haystacks))
}
