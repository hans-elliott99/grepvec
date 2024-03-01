#' Search for matches between a vector of patterns and a vector of strings.
#'
#' `grepvec` searches for needles in haystacks. Needles are one or more
#' regular expression or fixed strings, and haystacks are one or more strings to
#' search over. It is like calling `lapply(needles, grep, x = haystacks)`
#' (see `[grep]`).
#' The strings in the needles vector are compiled as regular expressions, unless
#' `fixed = TRUE`.
#' grepvec can return one or all matches based on the `matchrule` argument.
#' By default, a list of length(needles) is returned, containing
#' at each index an integer vector with the indices of haystacks that were
#' successfully matched.
#'
#' @param needles A character vector of expressions which will be searched for
#'   in `haystacks`` as fixed strings or regular expressions,
#'   depending on the `fixed` argument.
#' @param haystacks A character vector of strings which will be searched over
#'   for matches in `needles`.
#' @param fixed A logical(1), default `FALSE`. If `TRUE`, each string in
#'   needles is treated as as an exact string to find. If `FALSE`, needles are
#'   treated as regular expressions.
#' @param ignore_case A logical(1), default `FALSE`. If `TRUE`, the pattern
#'   matching will be case-insensitive. Ignored if `fixed = TRUE`.
#' @param value A logical(1), default `FALSE`. If `TRUE`, actual strings from
#'   `haystacks` are returned instead of indices. Otherwise, integer indices are
#'   returned.
#' @param match A character(1), default "all". If "first", the first match
#'   found in needles is returned for each string in haystack. This may improve
#'   performance for large needle vectors since the search process for each
#'   string can stop as soon as a match is found.
#'   If "all", a vector is returned containing the indices of all found matches
#'   in `needles`.
#' @returns A list of integer or character vectors, with length
#'   `length(needles)`.
#'
#' @examples
#' grepvec(needles = c("some", "other", "string"),
#'         haystacks = c("some string 1", "another string"),
#'         fixed = TRUE)
#' grepvec(c("^h", ".ell.", "ello$"), c("hello", "jelly"))
#' grepvec(c("^h", ".ell.", "ello$"), c("hello", "jelly"),
#'         value = TRUE)
#' grepvec(c("one", "possib", "many"),
#'         c("Some text which might possibly contain one of many keywords",
#'           "Another string without many words",
#'           "A third impossibly boring string",
#'           "Done"),
#'         match = "first",
#'         value = TRUE)
#' grepvec("a[bc]", c("app", "ABBA", "accolade"), ignore_case = TRUE)
#'
#' @useDynLib grepvec, grepvec_
#' @export
grepvec <- function(needles,
                    haystacks,
                    fixed = FALSE,
                    ignore_case = FALSE,
                    value = FALSE,
                    names = FALSE,
                    keepdim = FALSE,
                    match = c("all", "first")) {
    # prep arguments
    needles <- as.character(needles)
    haystacks <- as.character(haystacks)
    fixed <- as.logical(fixed)
    if (is.na(fixed)) stop("argument 'fixed' must be logical.")
    ignore_case <- as.logical(ignore_case)
    if (is.na(ignore_case)) stop("argument 'ignore_case' must be logical.")
    if (fixed && ignore_case) {
        ignore_case <- FALSE
        warning("argument 'ignore_case' is ignored when 'fixed' is TRUE.")
    }
    names <- as.logical(names)
    if (is.na(names)) stop("argument 'names' must be logical.")
    keepdim <- as.logical(keepdim)
    if (is.na(keepdim)) stop("argument 'keepdim' must be logical.")
    match <- match.arg(match)
    match_ix <- switch(match, all = 0L, first = 1L)
    # grepvec (ndl, hay, f, m, i, k)
    on.exit(.Call("on_exit_grepvec_"))
    x <- .Call("grepvec_",
               needles, haystacks, fixed, match_ix, ignore_case, keepdim)
    if (value)
        x <- lapply(x, function(ixs) haystacks[ixs])
    if (names)
        names(x) <- needles
    return(x)
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
by_hay.grepvec <- function(x,
                           value = FALSE,
                           needles = x$needles,
                           n_hay = length(x$haystacks),
                           ...) {
    res <- x$result
    # transpose list from by-needle to by-haystack
    names(res) <- seq_along(res) # stack requires names
    byhay <- with(utils::stack(res), split(ind, values))
    # expand list to be length of haystacks
    out <- lapply(as.character(seq(1, n_hay)), \(hay_ix) {
        ixs <- byhay[[hay_ix]] # get needle indices for given haystack
        # ixs is null if there were no matches for this needle
        if (value) {
            if (is.null(ixs)) character(0) else needles[ixs]
        } else {
            as.integer(ixs)
        }
    })
    return(out)
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
                           ...) {
    if (value) {
        # length is 0 if no hays matched with this ndl
        out <- lapply(x$result, function(ixs) {
            if (length(ixs) == 0) character(0) else haystacks[ixs]
        })
        return(out)
    }
    return(x$result)
}


# replicate the effect of calling print on a list object, but
# add some extra bits
#' @noRd
#' @export
print.grepvec <- function(x, n = 6, ...) {
    n <- max(1, n[1L])
    nhay <- nshow <- length(x$haystacks)
    nndl <- length(x$needles)
    cat("\n\t*** grepvec ***\n")
    cat("Searched for",
        format(nndl, big.mark = ",", scientific = FALSE),
        if (x$fixed) "fixed" else "regex",
        if (nndl == 1) "pattern" else "patterns",
        "across",
        format(nhay, big.mark = ",", scientific = FALSE),
        if (nhay == 1) "string" else "strings",
        "(case", if (x$ignore_case) "insensitive)." else "sensitive).",
        "\nReturned",
        if (x$matchrule == "first") "the first match.\n"
        else if (x$matchrule == "last") "the last match.\n"
        else "all matches. ")

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
            " haystacks.", sep = "")
    cat("\n")
    return(invisible(x))
}
