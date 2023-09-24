# dyn.unload("grepvec.R")
dyn.load("grepvec.so")

#' Search a vector of strings for matches in a vector of sub-strings
#' 
#' `grepvec` searches each string in haystacks for every sub-string in needles,
#' returning either the first or last match (if any are found). It does not (yet)
#' utilize regular expressions, so it is looking for exact sub-string matches
#' (like `grep` with argument fixed = TRUE, see `?grep`).
#' 
#' @param haystacks A character vector of strings which will be searched over
#'   for sub-strings.
#' @param needles A character vector of sub-strings which will be searched for
#'   in haystacks.
#' @param usefirst Logical, default TRUE. If TRUE, the first match found in
#'   needles is returned for each string in haystack. This may improve performance
#'   for large needle vectors since the search process can stop as soon as a
#'   match is found. If FALSE, the search process will continue until every
#'   sub-string in needles has been tested, and the last found match will be
#'   returned.
#' @return An integer vector with length equal to length(haystacks) containing
#'   the index of the matched sub-string for each string in haystacks (or 0 if
#'   no match is found).
#' @examples
#' grepvec(c("some string 1", "another string"),
#'         c("some", "another", "string"))
grepvec <- function(haystacks,
                    needles,
                    fixed = FALSE,
                    usefirst = TRUE) {
    stopifnot(is.character(haystacks))
    stopifnot(is.character(needles))
    stopifnot(is.logical(fixed))
    stopifnot(is.logical(usefirst))
    n1 <- length(haystacks)
    n2 <- length(needles)
    x <- .C("grepvec",
            haystacks, n1,
            needles, n2,
            matches = integer(n1),
            fixed = as.integer(fixed),
            usefirst = as.integer(usefirst))
    return(x$matches)
}

# .C("test", "hello")
# .C("test1", c("hello", "[", "goodbye", "[", "hans"), 5L)
# haystacks = c("hello", "goodbye", "hello", "a", "a", "a", "a", NA_character_)
# needles = c("hello", "[]")
# grepvec(haystacks, needles)
