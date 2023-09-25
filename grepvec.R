dyn.load("src/grepvec.so")

# TODO: specify flags to pass to regcomp?

#' Search a vector of strings for matches in a vector of sub-strings
#' 
#' `grepvec` searches each string in haystacks for every sub-string in needles,
#' returning either the first or last match (if any are found). It does not (yet)
#' utilize regular expressions, so it is looking for exact sub-string matches
#' (like `grep` with argument fixed = TRUE, see `?grep`).
#'
#'  
#' @param haystacks A character vector of strings which will be searched over
#'   for sub-string / regex matches.
#' @param needles A character vector of expressions which will be searched for
#'   in haystacks as sub-strings or regexs, depending on the `fixed` argument.
#' @param fixed Logical, default FALSE. If TRUE, treat each string in needles as
#'   an exact sub-string to find. If FALSE, needles are treated as regular
#'   expressions.
#' @param matchrule Character, default "all". If "first", the first match found in
#'   needles is returned for each string in haystack. This may improve performance
#'   for large needle vectors since the search process can stop as soon as a
#'   match is found. If "last", the search process will continue until every
#'   sub-string in needles has been tested, and the last found match will be
#'   returned. If "all", a vector will be returned containing the indices of
#'   all found matches in `needles`.
#' @return A list of integer vectors with length equal to length(haystacks), each
#'   vector containing the indices of succesfully matched needle strings (or 0 if
#'   no match is found).
#' @examples
#' grepvec(c("some string 1", "another string"),
#'         c("some", "another", "string"))
grepvec <- function(haystacks,
                    needles,
                    fixed = FALSE,
                    matchrule = c("all", "first", "last")) {
    stopifnot(is.character(haystacks))
    stopifnot(is.character(needles))
    stopifnot(is.logical(fixed))
    stopifnot(is.character(matchrule))
    .rules <- c("all", "first", "last")
    if (all(matchrule == .rules)) {
        matchrule <- "all"
    }
    if (! matchrule %in% .rules) {
        stop(paste("Provide one of the following for argument 'matchrule':",
                   paste(.rules, collapse = ", ")
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


# dyn.unload("grepvec.so")
# dyn.load("grepvec.so")
# 
# haystacks = c("hello", "goodbye", "hello", "a", "ba", "aa", "ba", NA_character_)
# needles = c("hello", "[]", "^a", ".o")
# matchrule = "first"
# 
# .Call("grepvec_fixed",
#        haystacks, needles,
#        matchrule = 0L)
