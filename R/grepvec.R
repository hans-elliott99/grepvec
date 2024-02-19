# TODO: specify flags for passing to regcomp?


#' grepvec: Search a vector of strings for matches in a vector of patterns.
#'
#' grepvec searches for needles in haystacks. Needles are one or more
#' regular expression or fixed strings, and haystacks are one or more strings to
#' search over.
#' The strings in the needles vector are compiled as regular expressions, unless
#' `fixed = TRUE`.
#' grepvec can return one or all matches based on the `matchrule` argument.
#' By default, a list of length(haystacks) is returned, containing
#' at each index an integer vector of the indices in needles which successfully
#' matched. The value returned by grepvec is flexible and can be adjusted by
#' the `out` and `value` arguments.
#'
#' @param needles A character vector of expressions which will be searched for
#'   in haystacks as fixed strings or regular expressions,
#'   depending on the `fixed` argument.
#' @param haystacks A character vector of strings which will be searched over
#'   for matches in needles.
#' @param matchrule A character(1), default "all". If "first", the first match
#'   found in needles is returned for each string in haystack. This may improve
#'   performance for large needle vectors since the search process for each
#'   string can stop as soon as a match is found.
#'   If "last", the search process will continue until every string in
#'   needles has been tested, and the last found match will be returned.
#'   If "all", a vector is returned containing the indices of all found matches
#'   in `needles` (with the same search time as "last").
#' @param fixed A logical(1), default FALSE. If TRUE, treat each string in
#'   needles as an exact string to find. If FALSE, needles are treated as
#'   regular expressions.
#' @param ignore_case A logical(1), default FALSE. If TRUE, the pattern matching
#'  will be case-insensitive.
#' @param value A logical(1), default FALSE. If TRUE, actual strings are
#'   returned instead of indices. Otherwise, integer indices are returned.
#'   The other aspects of the returned object are determined by `out`. `value`
#'   has no effect when `out = "object"`.
#' @param out A character(1), default "needles", declaring the output format.
#'   By default, grepvec returns a list of length(haystacks), where
#'   each list element is an integer vector of the indices of matches in the
#'   needles vector (unless `value = TRUE`, in which case the actual needle
#'   patterns are returned).  If `out = "haystacks"`,
#'   the output will be transposed to have length(needles) and each list
#'   element will be a vector of indices corresponding to the haystack strings
#'   that matched to the needle pattern at that index (unless `value = TRUE`).
#'   If `out = "object"`, an S3 object (class "grepvec")
#'   is returned, which can be used together with the [by_ndl] and [by_hay]
#'   methods to obtain the results in different formats.
#' @returns A list of integer or character vectors, or an S3 class object.
#'   See the `out` and `value` arguments.
#' @examples
#' grepvec(needles = c("some", "other", "string"),
#'         haystacks = c("some string 1", "another string"),
#'         fixed = TRUE)
#' grepvec(c("^h", ".ell.", "ello$"), c("hello", "jelly"))
#' grepvec(c("^h", ".ell.", "ello$"), c("hello", "jelly"),
#'         out = "needles", value = TRUE)
#' grepvec(c("one", "possib", "many"),
#'         c("Some text which might possibly contain one of many keywords",
#'           "Another string without many words",
#'           "A third impossibly boring string",
#'           "Done"),
#'         matchrule = "first",
#'         out = "hay",
#'         value = TRUE)
#' x <- grepvec(letters, c("abc", "123"), out = "object")
#' head(by_hay(x, value = TRUE))
#' head(by_ndl(x, value = TRUE))
#'
#' @export
grepvec <- function(needles,
                    haystacks,
                    matchrule = c("all", "first", "last"),
                    fixed = FALSE,
                    ignore_case = FALSE,
                    value = FALSE,
                    out = c("needles", "haystacks", "object")) {
    # prep arguments
    needles <- as.character(needles)
    haystacks <- as.character(haystacks)
    matchrule <- match.arg(matchrule)
    fixed <- as.logical(fixed)
    if (is.na(fixed)) stop("Argument 'fixed' must be logical.")
    ignore_case <- as.logical(ignore_case)
    if (is.na(ignore_case)) stop("Argument 'ignore_case' must be logical.")
    out <- match.arg(out)
    matchrule_ix <- switch(matchrule, all = 0L, first = 1L, last = 2L)
    # grepvec (strings ndl, strings hay, integer m, bool i)
    if (fixed) {
        x <- grepvec_fixed_(needles, haystacks, matchrule_ix, ignore_case)
    } else {
        x <- grepvec_regex_(needles, haystacks, matchrule_ix, ignore_case)
    }
    # determine output format
    if (out == "needles" && value == FALSE)
        return(x)
    obj <- structure(list(result = x,
                          needles = needles,
                          haystacks = haystacks,
                          matchrule = matchrule,
                          fixed = fixed,
                          ignore_case = ignore_case),
                     class = "grepvec")
    if (out == "needles")
        return(by_hay(obj, value = value))
    if (out == "haystacks")
        return(by_ndl(obj, value = value))
    return(obj)
}



#' For each haystack string, get the needle patterns that matched to it.
#'
#' Get the results of [grepvec] as a list, length(haystacks), containing the
#' indices or values of matched needles.
#' Applying this function to a [grepvec] object is equivalent to calling
#' [grepvec] with `out = "needles"`.
#'
#' @param x `grepvec` object returned from [grepvec] when out = "object".
#' @param value A logical(0), default FALSE. If FALSE, then at each list index
#'   is a vector containing the integer indices of matches in needles. If TRUE,
#'   the actual matched patterns in needles are used instead.
#' @param needles Character vector containing the needle patterns that were
#'   searched for in haystacks by [grepvec].
#'   By default, the needles used in the [grepvec] call are used.
#' @param ... Additional arguments to be passed to methods.
#' @return List of length(haystacks) of integer or character vectors.
#'   Each list index corresponds to the haystack string at that index, and the
#'   list elements are the indices or patterns from needles which matched to the
#'   given haystack string.
#'   The list element will be a vector of length 0 (integer(0) or character(0))
#'   if no needle matches were found for the haystack.
#'
#' @export
by_hay <- function(x, value = FALSE, ...) {
    UseMethod("by_hay")
}

#' @rdname by_hay
#' @export
by_hay.grepvec <- function(x, value = FALSE, needles = x$needles, ...) {
    if (value) {
        # length is 0 if no needles matched this haystack
        out <- lapply(x$result, function(ixs) {
            if (length(ixs) == 0) character(0) else needles[ixs]
        })
        return(out)
    }
    return(x$result)
}


#' For each needle pattern, get the haystack strings where a match was found.
#'
#' Get the results of [grepvec] as a list, length(needles), containing the
#' indices or values of matched haystacks.
#' Applying this function to a `grepvec` object is equivalent to calling
#' [grepvec] with `out = "haystacks"`.
#
#' @param x `grepvec` object returned from [grepvec] when out = "object".
#' @param value A logical(0), default FALSE. If FALSE, then at each list index
#'   is a vector containing the integer indices of the haystacks that matched to
#'   the given needle. If TRUE, the actual matched haystack strings are used.
#' @param haystacks Character vector containing the haystack strings that were
#'   searched over by grepvec. By default, the haystacks used in the [grepvec]
#'   call are used.
#' @param n_ndl The length of the character vector containing the needle
#'  patterns that were searched for in the haystacks by grepvec.
#'  By default, the length of the needles used in the [grepvec] call are used.
#' @param ... Additional arguments to be passed to methods.
#' @return List of length(needles) of integer or character vectors. Each list
#'   index corresponds to the needle pattern at that index, and the list
#'   elements are the indices or actual strings from haystacks which matched to
#'   the given needle.
#'   The list element will be a vector of length 0 (integer(0) or character(0))
#'   if no haystack matches were found for the needle.
#' @export
by_ndl <- function(x, value = FALSE, ...) {
    UseMethod("by_ndl")
}

#' @rdname by_ndl
#' @export
by_ndl.grepvec <- function(x,
                           value = FALSE,
                           haystacks = x$haystacks,
                           n_ndl = length(x$needles),
                           ...) {
    res <- x$result
    # transpose list from by-haystacks to by-needle
    names(res) <- seq_along(res) # stack requires names
    byndl <- with(utils::stack(res), split(ind, values))
    # expand list to be length of needles
    out <- lapply(as.character(seq(1, n_ndl)), \(ndl_ix) {
        ixs <- byndl[[ndl_ix]] # get haystack indices for given needle
        # ixs is null if there were no matches for this needle
        if (value) {
            if (is.null(ixs)) character(0) else haystacks[ixs]
        } else {
            as.integer(ixs)
        }
    })
    return(out)
}


# replicate the effect of calling print on a list object, but
# add some extra bits
#' @noRd
#' @export
print.grepvec <- function(x, n = 6, ...) {
    n <- max(1, n[1L])
    nhay <- nshow <- length(x$haystacks)
    nndl <- length(x$needles)
    cat("\n\t\t\t*** grepvec ***\n")
    cat("Searched for",
        format(nndl, big.mark = ",", scientific = FALSE),
        if (x$fixed) "fixed" else "regex",
        if (nndl == 1) "pattern" else "patterns",
        "across",
        format(nhay, big.mark = ",", scientific = FALSE),
        if (nhay == 1) "string." else "strings.",
        "\nReturned",
        if (x$matchrule == "first") "the first match.\n"
        else if (x$matchrule == "last") "the last match.\n"
        else "all matches.\n")

    byhay <- by_hay(x)
    if (nshow > n) {
        cat("First ")
        nshow <- n
    }
    cat(nshow,
        if (nshow == 1) "result:\n" else "results:\n")
    # cut to first n rows
    byhay <- byhay[1:nshow]
    # add haystacks as names
    nms <- paste0('"', x$haystacks[1:nshow], '"')
    longnms <- which(nchar(nms) > 82)
    nms[longnms] <- paste0(substr(nms[longnms], 1, 71), '"...[trunc]')
    for (i in seq_along(byhay)) {
        cat("\n", nms[i], "\n", sep = "")
        ndls <- byhay[[i]]
        print.default(ndls, ...)
        nmtch <- length(ndls)
        if (nmtch > 0)
            cat("(", nmtch, if (nmtch == 1) " match" else " matches", ")\n",
                sep = "")
    }
    if (nhay > nshow)
        cat("\n... out of ",
            format(nhay, big.mark = ",", scientific = FALSE),
            " haystacks.\n", sep = "")
    return(invisible(x))
}
