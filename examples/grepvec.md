# grepvec demo


- [Examples](#examples)
- [Speed](#speed)
- [Compare grepvec with native R
  solutions](#compare-grepvec-with-native-r-solutions)

``` r
# make sure you installed via remotes::install_github or devtools
library(grepvec)
library(microbenchmark)
set.seed(1614)

#
# create some data
#
shakespeare_url <- "https://ocw.mit.edu/ans7870/6/6.006/s08/lecturenotes/files/t8.shakespeare.txt"
txt <- trimws(readLines(shakespeare_url))

#
# get some random words from the txt to use as patterns
#
gen_word_list <- function(lines, n = 10) {
    li <- as.integer(runif(n, 1, length(lines)))
    words <- paste(lines[li], collapse = " ")
    words <- unique(strsplit(words, " ")[[1]])
    words <- words[words != ""]
    words <- words[as.integer(runif(n, 1, length(words)))]
    words <- gsub("\\[|\\]|\\(|\\)", "", words)
    return(words)
}

words <- gen_word_list(txt, n = 2000)

t0 = Sys.time()
m <- grepvec(words, txt[1:500], matchrule = "all", out = "object", fixed = FALSE)
difftime(Sys.time(), t0)
```

    Time difference of 0.1783898 secs

``` r
# try1: 8.95 secs
# try2: 8.43 secs (very few changes)
# try3: 8.12 secs (use C API for vectors)

## back to C code
# try1: 0.35 secs
# try2: 7.5 secs (use std::vector to store regexes instead of C dynamic array)
# okay, seems like the really slow bit is the std::regex lib

## C code but using boost::regex
# try1: 0.63 seconds (hallelujiah)

## & when fixed = TRUE:
# 0.23 secs
```

## Examples

``` r
grepvec(c("one", "possib", "many"),
        c("Some text which might possibly contain one of many keywords",
          "Another string without many words",
          "A third impossibly boring string",
          "Done"))
```

    [[1]]
    [1] 1 2 3

    [[2]]
    [1] 3

    [[3]]
    [1] 2

    [[4]]
    [1] 1

``` r
grepvec(letters[1:6], c("AbC", 123), ignore_case = TRUE, value = TRUE)
```

    [[1]]
    [1] "a" "b" "c"

    [[2]]
    character(0)

``` r
grepvec("^regex$", "regex")
```

    [[1]]
    [1] 1

``` r
grepvec("^fixed$", "fixed", fixed = TRUE)
```

    [[1]]
    integer(0)

By default `grepvec` returns a list of the needle indexes matching to
each haystack, but there are other options. For example, we can return a
grepvec object which stores the results along with the data needed to
transform the results into other formats.

``` r
words <- gen_word_list(txt, n = 2000)
t0 = Sys.time()
m <- grepvec(words, txt[1:500], matchrule = "all", out = "object")
difftime(Sys.time(), t0)
```

    Time difference of 0.1933751 secs

``` r
# show indices of needle matches (default behavior)
head(by_hay(m))
```

    [[1]]
     [1]  391  774  813 1131 1346 1524 1554 1681 1780 1812

    [[2]]
     [1]  391  574  675  813 1131 1346 1388 1554 1662 1681 1739 1777 1812 1987

    [[3]]
     [1]  302  391  774  813 1131 1346 1524 1536 1554 1681 1812

    [[4]]
     [1]   74  120  302  369  391  471  813 1043 1131 1346 1439 1536 1554 1585 1662
    [16] 1681 1791 1812 1885 1987

    [[5]]
    integer(0)

    [[6]]
    [1]  302 1131 1346 1681

``` r
# convert indices to the corresponding strings
head(setNames(by_hay(m, value = TRUE), m$haystacks))
```

    $`This is the 100th Etext file presented by Project Gutenberg, and`
     [1] "then?" "be."   "of?"   "so?"   "he?"   "and"   "it?"   "ho?"   "This" 
    [10] "in?"  

    $`is presented in cooperation with World Library, Inc., from their`
     [1] "then?" "on."   "on"    "of?"   "so?"   "he?"   "with"  "it?"   "at."  
    [10] "ho?"   "it"    "heir"  "in?"   "at"   

    $`Library of the Future and Shakespeare CDROMS.  Project Gutenberg`
     [1] "are"   "then?" "be."   "of?"   "so?"   "he?"   "and"   "of"    "it?"  
    [10] "ho?"   "in?"  

    $`often releases Etexts that are NOT placed in the Public Domain!!`
     [1] "in!"   "place" "are"   "ease"  "then?" "that?" "of?"   "oft"   "so?"  
    [10] "he?"   "as"    "of"    "it?"   "that." "at."   "ho?"   "that." "in?"  
    [19] "oft"   "at"   

    [[5]]
    character(0)

    $Shakespeare
    [1] "are" "so?" "he?" "ho?"

``` r
# transpose so we can see the haystacks that matched to each needle
head(setNames(by_ndl(m, value = TRUE), m$needles))
```

    $Joan
    character(0)

    $`bear-baitings.`
    character(0)

    $Fal.
    character(0)

    $opening
    character(0)

    $PINCH.
    character(0)

    $`morrow,`
    character(0)

``` r
# print object
print(m)
```


                *** grepvec ***
    Searched for 2,000 regex patterns across 500 strings. 
    Returned all matches.
    First 6 results:

    "This is the 100th Etext file presented by Project Gutenberg, and"
     [1]  391  774  813 1131 1346 1524 1554 1681 1780 1812
    (10 matches)

    "is presented in cooperation with World Library, Inc., from their"
     [1]  391  574  675  813 1131 1346 1388 1554 1662 1681 1739 1777 1812 1987
    (14 matches)

    "Library of the Future and Shakespeare CDROMS.  Project Gutenberg"
     [1]  302  391  774  813 1131 1346 1524 1536 1554 1681 1812
    (11 matches)

    "often releases Etexts that are NOT placed in the Public Domain!!"
     [1]   74  120  302  369  391  471  813 1043 1131 1346 1439 1536 1554 1585 1662
    [16] 1681 1791 1812 1885 1987
    (20 matches)

    ""
    integer(0)

    "Shakespeare"
    [1]  302 1131 1346 1681
    (4 matches)

    ... out of 500 haystacks.

``` r
# lengths == 0 (no matches) or 1 (some match) if matchrule = "first" or "last"
# note, matchrule = "first" will always be faster than "last" or "all"
all(lengths(grepvec(words, txt[1:1000], matchrule = "first")) ==
        lengths(grepvec(words, txt[1:1000], matchrule = "last")))
```

    [1] TRUE

It is easy to calculate the number of matching patterns across a vector
of strings. For example, as a column in a data.frame:

``` r
# get number of matches in pattern vector, e.g., for a data.frame column
patterns <- gen_word_list(txt, n = 2000)
ex <- data.frame(txt_col = txt[300:500])
ex <- transform(ex,
                n_match_rgx = lengths(grepvec(patterns, txt_col)),
                n_match_fxd = lengths(grepvec(patterns, txt_col, fixed = TRUE)))
head(ex)
```

                                         txt_col n_match_rgx n_match_fxd
    1     But if thou live remembered not to be,           4           1
    2 Die single and thine image dies with thee.          13           4
    3                                                      0           0
    4                                                      0           0
    5                                          4           0           0
    6  Unthrifty loveliness why dost thou spend,           6           3

Returning the grepvec object may be useful if you have a few operations
to perform:

``` r
txt_var <- txt[300:500]
(res <- grepvec(patterns, txt_var, out = "object"))
```


                *** grepvec ***
    Searched for 2,000 regex patterns across 201 strings. 
    Returned all matches.
    First 6 results:

    "But if thou live remembered not to be,"
    [1]  232  583 1329 1477
    (4 matches)

    "Die single and thine image dies with thee."
     [1]  523  704  806  833 1233 1329 1497 1513 1515 1587 1626 1742 1809
    (13 matches)

    ""
    integer(0)

    ""
    integer(0)

    "4"
    integer(0)

    "Unthrifty loveliness why dost thou spend,"
    [1]  439  474  760 1329 1652 1742
    (6 matches)

    ... out of 201 haystacks.

``` r
match_ixs <- by_hay(res)
match_vals <- lapply(by_hay(res, value = TRUE),
                     \(vec) if (length(vec) == 0) NA else vec)


ex <- data.frame(txt_col = txt_var,
                 n_matches = lengths(match_ixs),
                 first_match = unlist(lapply(match_vals, `[[`, 1)),
                 all_matches = vapply(match_vals,
                                  paste,
                                  character(1L),
                                  collapse = ", "))
head(ex)
```

                                         txt_col n_matches first_match
    1     But if thou live remembered not to be,         4         not
    2 Die single and thine image dies with thee.        13        him?
    3                                                    0        <NA>
    4                                                    0        <NA>
    5                                          4         0        <NA>
    6  Unthrifty loveliness why dost thou spend,         6         why
                                                              all_matches
    1                                                not, not., he?, not.
    2 him?, an., then?, a, it, he?, die., them?, mad?, it., age, in, mad?
    3                                                                  NA
    4                                                                  NA
    5                                                                  NA
    6                                    why, spend?, love., he?, why, in

Be careful when unlisting the results, because if no match was found a 0
length vector is returned.

``` r
res <- grepvec(patterns, txt_var, matchrule = "first")
head(res)
```

    [[1]]
    [1] 232

    [[2]]
    [1] 523

    [[3]]
    integer(0)

    [[4]]
    integer(0)

    [[5]]
    integer(0)

    [[6]]
    [1] 439

``` r
length(res)
```

    [1] 201

``` r
length(unlist(res))
```

    [1] 166

``` r
# helper function for flattening a list of vectors with length 0 or 1
flatten <- function(x, keepdim = TRUE) {
    if (keepdim) {
        x <- lapply(x, \(vec) if (length(vec) == 0) NA else vec[1L])
    }
    return(unlist(x))
}

length(flatten(res))
```

    [1] 201

Invalid regex patterns:

``` r
# what if we use an invalid regex pattern?
grepvec(c("[bad", "(regex"), "those are bad regex patterns")
```

    Warning: error compiling regex '[bad': Unmatched [ or [^ in character class
    declaration.  The error occurred while parsing the regular expression:
    '[bad>>>HERE>>>'.

    Warning: error compiling regex '(regex': Unmatched marking parenthesis ( or \(.
    The error occurred while parsing the regular expression: '(regex>>>HERE>>>'.

    [[1]]
    integer(0)

``` r
# grep is similair but also errors out
tryCatch(
    grep("[bad", "those are bad regex patterns"),
    error = \(e) { cat("grep error message:\n"); conditionMessage(e) }
)
```

    Warning in grep("[bad", "those are bad regex patterns"): TRE pattern
    compilation error 'Missing ']''

    grep error message:

    [1] "invalid regular expression '[bad', reason 'Missing ']''"

## Speed

``` r
# test grepvec on some bigger vectors
hay <- c(txt, txt, txt, txt)
ndl <- words
cat("N Hay =", format(length(hay), big.mark = ","),
    "| N Needle =", format(length(ndl), big.mark = ","), "\n")
```

    N Hay = 497,824 | N Needle = 2,000 

``` r
t0 <- Sys.time()
suppressWarnings({
    x <- grepvec(ndl, hay, fixed = FALSE, matchrule = "all")
})
difftime(Sys.time(), t0)
```

    Time difference of 4.242218 mins

``` r
# returning only the first match is faster
t0 <- Sys.time()
suppressWarnings({
    x <- grepvec(ndl, hay, fixed = FALSE, matchrule = "first")
})
difftime(Sys.time(), t0)
```

    Time difference of 49.80179 secs

``` r
# large Ns - causes stack overflow on my sys w/out dynammic alloc, and cause
# PROTECTion stack overflow if list elemements are not unprotected as soon as
# added to list (in grepvec.c)
longndls <- gen_word_list(txt, length(txt))
# "banana" is not in shakespeare,
#  so put it at some indices so we can verify that the results are correct
all(lengths(grepvec("banana", txt)) == 0)
```

    [1] TRUE

``` r
longndls[1] <- "banana" # so now it's a pattern to search for
banan_idx <- c((1:100)^2, length(txt))
somehay <- txt
somehay[banan_idx] <- "banana"

# basically a >15 billion element matrix
cat("N Hay =", format(length(somehay), big.mark = ","),
    "| N Needle =", format(length(longndls), big.mark = ","), "\n")
```

    N Hay = 124,456 | N Needle = 124,456 

``` r
t0 <- Sys.time()
suppressWarnings({
    x <- grepvec(longndls, somehay, matchrule = "first", out = "obj", fixed = TRUE)
})
difftime(Sys.time(), t0)
```

    Time difference of 4.134299 secs

``` r
# the only strings that matched to needle 1 should be those at 'banan_idx'
xix <- by_hay(x)
all(which(unlist(xix) == 1) == banan_idx)
```

    [1] TRUE

``` r
unique(by_hay(x, value = TRUE)[which(flatten(xix) == 1)])
```

    [[1]]
    [1] "banana"

## Compare grepvec with native R solutions

Suggestions wanted.

``` r
#
# implement as if matchrule = last, and use try because grep errors if bad regex
#
loop_grep <- function(needles, haystacks) {
    x <- vector(mode = "list", length = length(haystacks))
    for (i in seq_along(needles)) {
        try({
            matchinds <- grep(needles[i], haystacks)
            x[matchinds] <- i #lapply(x[matchinds], \(vec) c(vec, i))
        }, silent = TRUE)
    }
    return(x)
}

lapply_grep <- function(needles, haystacks) {
    x <- vector(mode = "list", length = length(haystacks))
    .fn <- \(ndl) {
        try({
            matchinds <- grep(ndl, haystacks)
            return(matchinds)
        }, silent = TRUE)
        return(NULL)
    }
    ndlmtches <- lapply(needles, .fn)
    for (i in seq_along(needles)) {
        matchinds <- ndlmtches[[i]]
        x[matchinds] <- i #lapply(x[matchinds], \(vec) c(vec, i))
    }
    return(x)
}

# verify same results
shortndls <- words[1:100]
x_loop <- loop_grep(shortndls, txt)
x_lapply <- lapply_grep(shortndls, txt)
x_grepvec <- grepvec(shortndls, txt, matchrule = "last")
all(unlist(x_loop) == unlist(x_lapply))
```

    [1] TRUE

``` r
# grepvec returns empty vec if no results, others return NULL. make equal
x_grepvec <- lapply(x_grepvec, \(vec) if (length(vec) == 0) NULL else vec)
all(unlist(x_lapply) == unlist(x_grepvec))
```

    [1] TRUE

``` r
microbenchmark(loop_grep(shortndls, txt),
               lapply_grep(shortndls, txt),
               grepvec(shortndls, txt, matchrule = "last"),
               times = 10)
```

    Unit: seconds
                                            expr      min       lq     mean
                       loop_grep(shortndls, txt) 6.223104 6.280492 6.650967
                     lapply_grep(shortndls, txt) 6.160114 6.239869 6.637259
     grepvec(shortndls, txt, matchrule = "last") 2.150144 2.169785 2.261649
       median       uq      max neval
     6.523296 6.929130 7.251586    10
     6.586978 6.924567 7.411134    10
     2.193619 2.246433 2.678154    10
