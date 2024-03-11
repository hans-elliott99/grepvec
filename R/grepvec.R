
check_bool <- function(opt, name) {
    if (!isTRUE(opt) && !identical(opt, FALSE))
        stop(paste0("argument '", name, "' must be TRUE or FALSE."))
    return(opt)
}



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
#' @useDynLib grepvec, C_grepvec, C_vecgrep, C_on_exit_grepvec
#' @export
grepvec <- function(needles,
                    haystacks,
                    ignore_case = FALSE,
                    value = FALSE,
                    fixed = FALSE,
                    use_bytes = FALSE,
                    invert = FALSE,
                    keepdim = FALSE,
                    use_names = FALSE,
                    match = c("all", "first")) {
    needles <- as.character(needles)
    haystacks <- as.character(haystacks)
    value <- check_bool(value, "value")
    use_names <- check_bool(use_names, "use_names")
    match <- match.arg(match)

    on.exit(.Call("C_on_exit_grepvec"))
    x <- .Call("C_grepvec",
               needles, haystacks,
               ignore_case, fixed, use_bytes, invert, match, keepdim,
               return_logical = FALSE)
    if (value)
        x <- lapply(x, function(ixs) haystacks[ixs])
    if (use_names)
        names(x) <- needles
    return(x)
}



#' @rdname grepvec
#' @export
greplvec <- function(needles,
                     haystacks,
                     ignore_case = FALSE,
                     fixed = FALSE,
                     use_bytes = FALSE,
                     use_names = FALSE,
                     match = c("all", "first")) {
    # prep arguments
    needles <- as.character(needles)
    haystacks <- as.character(haystacks)
    use_names <- check_bool(use_names, "use_names")
    match <- match.arg(match)

    on.exit(.Call("C_on_exit_grepvec"))
    x <- .Call("C_grepvec", needles, haystacks,
               ignore_case, fixed, use_bytes,
               invert = FALSE, # don't use invert when grepl
               match = match,
               keepdim = FALSE, # TRUE has no effect when return_logical
               return_logical = TRUE)
    if (use_names)
        names(x) <- needles
    return(x)
}


#' @rdname grepvec
#' @export
vecgrep <- function(haystacks,
                    needles,
                    ignore_case = FALSE,
                    value = FALSE,
                    fixed = FALSE,
                    use_bytes = FALSE,
                    invert = FALSE,
                    keepdim = FALSE,
                    use_names = FALSE,
                    match = c("all", "first")) {
    # prep arguments
    needles <- as.character(needles)
    haystacks <- as.character(haystacks)
    value <- check_bool(value, "value")
    use_names <- check_bool(use_names, "use_names")
    match <- match.arg(match)

    on.exit(.Call("C_on_exit_grepvec"))
    x <- .Call("C_vecgrep", needles, haystacks,
               ignore_case, fixed, use_bytes, invert, match, keepdim,
               return_logical = FALSE)
    if (value)
        x <- lapply(x, function(ixs) needles[ixs])
    if (use_names)
        names(x) <- haystacks
    return(x)
}



#' @rdname grepvec
#' @export
vecgrepl <- function(haystacks,
                     needles,
                     ignore_case = FALSE,
                     fixed = FALSE,
                     use_bytes = FALSE,
                     use_names = FALSE,
                     match = c("all", "first")) {
    # prep arguments
    needles <- as.character(needles)
    haystacks <- as.character(haystacks)
    use_names <- check_bool(use_names, "use_names")
    match <- match.arg(match)

    on.exit(.Call("C_on_exit_grepvec"))
    x <- .Call("C_vecgrep", needles, haystacks,
               ignore_case, fixed, use_bytes,
               invert = FALSE, # don't use invert when grepl
               match = match,
               keepdim = FALSE, # TRUE has no effect when return_logical
               return_logical = TRUE)
    if (use_names)
        names(x) <- haystacks
    return(x)
}


#' Check if a string contains any of a vector of patterns.
#'
#' @useDynLib grepvec, C_vecmatch
#' @export
grep_any <- function(haystacks,
                     needles,
                     ignore_case = FALSE,
                     fixed = FALSE,
                     use_bytes = FALSE,
                     invert = FALSE) {
    fct <- FALSE
    if (is.factor(haystacks)) {
        fct <- TRUE
        hay <- levels(haystacks)
    } else {
        hay <- as.character(haystacks)
    }
    needles <- as.character(needles)
    x <- .Call("C_vecmatch", needles, hay,
               ignore_case = ignore_case, value = FALSE, fixed = fixed,
               use_bytes = use_bytes, invert = invert,
               return_logical = TRUE, return_counts = FALSE)
    # index by factor == index by their encoded integer lvls
    if (fct)
        x <- x[haystacks]
    return(x)
}


#' Get the first match from a vector of patterns for each string.
#'
#' @useDynLib grepvec, C_vecmatch
#' @export
grep_first <- function(haystacks,
                       needles,
                       ignore_case = FALSE,
                       value = FALSE,
                       fixed = FALSE,
                       use_bytes = FALSE,
                       invert = FALSE) {
    fct <- FALSE
    if (is.factor(haystacks)) {
        fct <- TRUE
        hay <- levels(haystacks)
    } else {
        hay <- as.character(haystacks)
    }
    needles <- as.character(needles)
    value <- check_bool(value, "value")
    x <- .Call("C_vecmatch", needles, hay,
               ignore_case = ignore_case, value = value, fixed = fixed,
               use_bytes = use_bytes, invert = invert,
               return_logical = FALSE, return_counts = FALSE)
    # index by factor == index by their encoded integer lvls
    if (fct)
        x <- x[haystacks]
    return(x)
}



#' Count the number of matches in a vector of patterns for each string.
#'
#' @useDynLib grepvec, C_vecmatch
#' @export
grep_count <- function(haystacks,
                       needles,
                       ignore_case = FALSE,
                       fixed = FALSE,
                       use_bytes = FALSE,
                       invert = FALSE) {
    haystacks <- as.character(haystacks)
    needles <- as.character(needles)
    fct <- FALSE
    if (is.factor(haystacks)) {
        fct <- TRUE
        hay <- levels(haystacks)
    } else {
        hay <- haystacks
    }
    x <- .Call("C_vecmatch", needles, hay,
               ignore_case = ignore_case, value = FALSE, fixed = fixed,
               use_bytes = use_bytes, invert = invert,
               return_logical = FALSE, return_counts = TRUE)
    # index by factor == index by their encoded integer lvls
    if (fct)
        x <- x[haystacks]
    return(x)
}


#' @rdname grep_any
#' @export
"%grepin%" <- function(x, patterns) {
    grep_any(x, patterns,
             ignore_case = FALSE, fixed = FALSE,
             use_bytes = FALSE, invert = FALSE)
}


#' @rdname grep_any
#' @export
"%igrepin%" <- function(x, patterns) {
    grep_any(x, patterns,
             ignore_case = TRUE, fixed = FALSE,
             use_bytes = FALSE, invert = FALSE)
}

# TODO:
# - general documentation
# - approx matching
# - object which allows for compilation of a set of patterns and then you can
#   use them however you want