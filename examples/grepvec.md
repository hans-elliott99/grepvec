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
m <- grepvec(words, txt[1:1000], matchrule = "all", out = "object")
# show indices of needle matches (default behavior)
head(by_hay(m))
```

    [[1]]
     [1]   71  214  321  454  593 1237 1547 1742 1809 1840 1938

    [[2]]
     [1]   71  124  185  214  215  250  481  593  690  791  968 1046 1217 1237 1249
    [16] 1276 1629 1742 1840 1938 1980

    [[3]]
    [1]   71  214 1237 1547 1572 1742 1809 1840 1938

    [[4]]
     [1]   53   71  124  170  214  250  297  481  690  968 1152 1200 1221 1237 1243
    [16] 1351 1438 1742 1757 1776 1840 1938

    [[5]]
    integer(0)

    [[6]]
    [1]  214 1572

``` r
# convert indices to the corresponding strings
head(setNames(by_hay(m, value = TRUE), m$haystacks))
```

    $`This is the 100th Etext file presented by Project Gutenberg, and`
     [1] "he."      "so?"      "by"       "by."      "present." "the"     
     [7] "and"      "on?"      "go?"      "the"      "the"     

    $`is presented in cooperation with World Library, Inc., from their`
     [1] "he."      "at"       "on."      "so?"      "from"     "at"      
     [7] "at."      "present." "in"       "ope"      "in"       "ope"     
    [13] "with"     "the"      "on."      "on."      "or"       "on?"     
    [19] "the"      "the"      "with"    

    $`Library of the Future and Shakespeare CDROMS.  Project Gutenberg`
    [1] "he." "so?" "the" "and" "ear" "on?" "go?" "the" "the"

    $`often releases Etexts that are NOT placed in the Public Domain!!`
     [1] "that?"    "he."      "at"       "Do"       "so?"      "at"      
     [7] "place."   "at."      "in"       "in"       "main"     "Domain!!"
    [13] "place."   "the"      "place"    "often"    "often"    "on?"     
    [19] "releases" "Do"       "the"      "the"     

    [[5]]
    character(0)

    $Shakespeare
    [1] "so?" "ear"

``` r
# transpose so we can see the haystacks that matched to each needle
head(setNames(by_ndl(m, value = TRUE), m$needles))
```

    $dotards.
    character(0)

    $makes
    character(0)

    $`Got,`
    character(0)

    $`earth,`
    character(0)

    $serve
    [1] "(Internet, Bitnet, Compuserve, ATTMAIL or MCImail)."      
    [2] "Internet (72600.2026@compuserve.com); TEL: (212-254-5093)"
    [3] "How much more praise deserved thy beauty's use,"          
    [4] "Reserve them for my love, not for their rhyme,"           

    $ox
    [1] "P. O. Box  2782" "P.O. Box 2782"  

``` r
# print object
print(m)
```


                *** grepvec ***
    Searched for 2,000 regex patterns across 1,000 strings. 
    Returned all matches.
    First 6 results:

    "This is the 100th Etext file presented by Project Gutenberg, and"
     [1]   71  214  321  454  593 1237 1547 1742 1809 1840 1938
    (11 matches)

    "is presented in cooperation with World Library, Inc., from their"
     [1]   71  124  185  214  215  250  481  593  690  791  968 1046 1217 1237 1249
    [16] 1276 1629 1742 1840 1938 1980
    (21 matches)

    "Library of the Future and Shakespeare CDROMS.  Project Gutenberg"
    [1]   71  214 1237 1547 1572 1742 1809 1840 1938
    (9 matches)

    "often releases Etexts that are NOT placed in the Public Domain!!"
     [1]   53   71  124  170  214  250  297  481  690  968 1152 1200 1221 1237 1243
    [16] 1351 1438 1742 1757 1776 1840 1938
    (22 matches)

    ""
    integer(0)

    "Shakespeare"
    [1]  214 1572
    (2 matches)

    ... out of 1,000 haystacks.

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
    1     But if thou live remembered not to be,          18           8
    2 Die single and thine image dies with thee.          18          11
    3                                                      0           0
    4                                                      0           0
    5                                          4           0           0
    6  Unthrifty loveliness why dost thou spend,          18           6

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
     [1]  127  512  535  566  583  729  774  810  813 1074 1099 1305 1346 1554 1681
    [16] 1693 1792 1812
    (18 matches)

    "Die single and thine image dies with thee."
     [1]  139  194  391  644  720  944 1131 1198 1235 1346 1388 1524 1554 1663 1681
    [16] 1739 1812 1923
    (18 matches)

    ""
    integer(0)

    ""
    integer(0)

    "4"
    integer(0)

    "Unthrifty loveliness why dost thou spend,"
     [1]   19  127  225  287  441  535  577  726  729  813  919 1074 1131 1159 1346
    [16] 1554 1681 1812
    (18 matches)

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
    1     But if thou live remembered not to be,        18       thou?
    2 Die single and thine image dies with thee.        18        thin
    3                                                    0        <NA>
    4                                                    0        <NA>
    5                                          4         0        <NA>
    6  Unthrifty loveliness why dost thou spend,        18          do
                                                                                                all_matches
    1       thou?, be,, if, now?, no, thou?, be., live, of?, thou?, be,, red, he?, it?, ho?, red, live, in?
    2 thin, thee., then?, image, image, thin, so?, die, age, he?, with, and, it?, Die, ho?, it, in?, thine.
    3                                                                                                    NA
    4                                                                                                    NA
    5                                                                                                    NA
    6  do, thou?, love?, end, do, if, love., spend, thou?, of?, love?, thou?, so?, dost, he?, it?, ho?, in?

Be careful when unlisting the results, because if no match was found a 0
length vector is returned.

``` r
res <- grepvec(patterns, txt_var, matchrule = "first")
head(res)
```

    [[1]]
    [1] 127

    [[2]]
    [1] 139

    [[3]]
    integer(0)

    [[4]]
    integer(0)

    [[5]]
    integer(0)

    [[6]]
    [1] 19

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

    Warning in grepvec(c("[bad", "(regex"), "those are bad regex patterns"): could
    not compile regex for pattern: [bad

    Warning in grepvec(c("[bad", "(regex"), "those are bad regex patterns"): could
    not compile regex for pattern: (regex

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

    Time difference of 2.483955 mins

``` r
# returning only the first match is faster
t0 <- Sys.time()
suppressWarnings({
    x <- grepvec(ndl, hay, fixed = FALSE, matchrule = "first")
})
difftime(Sys.time(), t0)
```

    Time difference of 12.03624 secs

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
    x <- grepvec(longndls, somehay, matchrule = "first", out = "obj")
})
difftime(Sys.time(), t0)
```

    Time difference of 27.13047 secs

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
                       loop_grep(shortndls, txt) 5.015354 5.106655 5.193175
                     lapply_grep(shortndls, txt) 5.116686 5.143838 5.200952
     grepvec(shortndls, txt, matchrule = "last") 1.218099 1.223905 1.251596
       median       uq      max neval
     5.209785 5.267124 5.369964    10
     5.210358 5.235419 5.302881    10
     1.244400 1.263760 1.311056    10
