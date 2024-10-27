# vecgrep demo


- [Examples](#examples)
- [Speed](#speed)
- [Compare vecgrep with native R
  solutions](#compare-vecgrep-with-native-r-solutions)
- [Encodings](#encodings)

2024-10-26 20:58:26.586284

``` r
# compiled on:
Sys.info()[c("sysname", "release", "version", "machine")]
```

                                 sysname                              release 
                                 "Linux"  "5.10.16.3-microsoft-standard-WSL2" 
                                 version                              machine 
    "#1 SMP Fri Apr 2 22:23:49 UTC 2021"                             "x86_64" 

``` r
devtools::load_all()
```

    ℹ Loading grepvec

``` r
# make sure you installed in some way
library(grepvec)
library(microbenchmark)
set.seed(1614)

load_ext_data <- function(filename, ...) {
    tryCatch(
        readLines(file.path("../inst/extdata/", filename), ...),
        warning = \(e) readLines(file.path("./inst/extdata", filename), ...)
    )
}

#
# create some data
#
txt <- load_ext_data("shakespeare.txt")

# rm document info
txt <- txt[min(which(txt == "1609")) : max(which(txt == "THE END"))]
# so we have pure Shakespeare
writeLines(c(head(txt, 15), "....", tail(txt, 5)))
```

    1609

    THE SONNETS

    by William Shakespeare



    1
    From fairest creatures we desire increase,
    That thereby beauty's rose might never die,
    But as the riper should by time decease,
    His tender heir might bear his memory:
    But thou contracted to thine own bright eyes,
    Feed'st thy light's flame with self-substantial fuel,
    ....
    O, all that borrowed motion, seeming owed,
    Would yet again betray the fore-betrayed,
    And new pervert a reconciled maid.'

    THE END

``` r
#
# get some random words from the txt to use as patterns
#
gen_word_list <- function(lines, n = 10) {
    li <- as.integer(runif(n, 1, length(lines)))
    words <- paste(lines[li], collapse = " ")
    words <- unique(strsplit(words, " ")[[1]])
    words <- gsub("[^[:alnum:] ]", "", words)
    words <- words[words != ""]
    words <- words[as.integer(runif(n, 1, length(words)))]
    return(words)
}
```

## Examples

``` r
# grepvec:
# returns a list of length needles (argument 1)
grepvec(c("needle", "mice", "dirt"), "A needle in a dirty haystack")
```

    [[1]]
    [1] 1

    [[2]]
    integer(0)

    [[3]]
    [1] 1

``` r
# vecgrep:
# returns a list of length haystacks (argument 1) - like transposed grepvec
vecgrep("A needle in a dirty haystack", c("needle", "mice", "dirt"))
```

    [[1]]
    [1] 1 3

``` r
# other utils:
strings <- c("the quick brown fox", "jumps over", "the lazy dog")
# check if strings contain any of the patterns in the pattern vector 
grepl_any(c("fox", "dog"), strings)
```

    [1]  TRUE FALSE  TRUE

``` r
# or, equivalent:
c("fox", "dog") %grepin% strings
```

    [1]  TRUE FALSE  TRUE

``` r
# get the first match in the pattern vector for each string in x
grep_first(c("quick", "fox", "lazy", "dog"), strings, value = TRUE)
```

    [1] "quick" NA      "lazy" 

``` r
# count the number of patterns that occur in each string
grep_count(c("o", "u"), strings)
```

    [1] 2 2 1

``` r
words <- gen_word_list(txt, n = 2000)

t0 = Sys.time()
m1 <- vecgrep(txt[1:500], words, match = "all")
difftime(Sys.time(), t0)
```

    Time difference of 0.6469839 secs

``` r
t0 = Sys.time()
m <- grepl_any(words, txt[1:500])
difftime(Sys.time(), t0)
```

    Time difference of 0.07104111 secs

``` r
t0 = Sys.time()
m <- grep_first(words, txt[1:500])
difftime(Sys.time(), t0)
```

    Time difference of 0.07102871 secs

``` r
t0 = Sys.time()
m <- grep_first(words, txt[1:500], value = TRUE)
difftime(Sys.time(), t0)
```

    Time difference of 0.06845689 secs

``` r
t0 = Sys.time()
m <- grep_count(words, txt[1:500])
difftime(Sys.time(), t0)
```

    Time difference of 0.6283624 secs

``` r
all(lengths(m1) == m)
```

    [1] TRUE

Note: no matter what, it is regex compilation/execution that slows
things down. So it makes sense to avoid compiling over and over again.
Really, should be concatenating regexes.  
- The only time it makes sense to leave regexes separate is when we want
to return the *patterns* that matched, as opposed to the place in the
text where they matched…  
- whether or not we ever really want that is a fair question…  
- for example, if our patterns are actually a list of place names, and
we want to determine which match to each row, then we don’t care if it’s
technically the pattern being returned or the substr of the txt being
returned

``` r
words <- unique(gen_word_list(txt, 100))
pattern <- paste(words, collapse = "|")
substr(pattern, 1, 100)
```

    [1] "in|leave|Our|dearer|To|Beg|FLUELLEN|truth|her|so|brother|must|should|glove|us|takes|thy|King|Know|le"

``` r
g <- gregexec(pattern, txt[1:10])
as.numeric(g[10][[1]])
```

    [1]  7 17 34 39

``` r
regmatches(txt[1:10], g)[10]
```

    [[1]]
         [,1] [,2] [,3] [,4]
    [1,] "a"  "a"  "in" "as"

The innovation of grepvec could be to implement this for arbitrarily
long strings because currently this happens:

``` r
pattern <- paste(gen_word_list(txt, 2000), collapse = "|")
try(gregexec(pattern, txt[1:10]))
```

    Error in gregexpr(pattern = pattern, text = text, ignore.case = ignore.case,  : 
      assertion 'tree->num_tags == num_tags' failed in executing regexp: file 'tre-compile.c', line 634

- So figure out what the character limit is in TRE (tre-compile.c) and
  then implement something that collapses regexes until they reach a
  certain length:

``` r
words <- gen_word_list(txt, 2000)
text  <- txt[1:500]


plen <- length(words)
jumpsize <- 500
njumps <- ceiling(plen / jumpsize) 

t0 <- Sys.time()
out <- NULL
for (it in seq(0, njumps - 1)) {
    st <- 1 + (it * jumpsize)
    end <- jumpsize + (it * jumpsize)
    if (end > plen) end = plen
    pat <- paste(words[st:end], collapse = "|")
    g <- gregexec(pat, text)
    # number of matches
    num <- lengths(g)
    # num <- lapply(g, \(x) if (x[1] == -1) 0 else length(x))
    matches <- regmatches(text, g)
    # then append to previous...
}
difftime(Sys.time(), t0)
```

    Time difference of 13.19791 secs

``` r
# this is about the most words I could concatenate without error
# (ofc, depends on length of indiv words):
t0 <- Sys.time()
. <- gregexpr(paste(words[1:900], collapse = "|"), text)
difftime(Sys.time(), t0)
```

    Time difference of 0.2198954 secs

``` r
t0 <- Sys.time()
. <- vecgrep(text, words[1:925])
difftime(Sys.time(), t0)
```

    Time difference of 0.3069727 secs

- would also need to optimize jump size because, e.g., 200 is faster
  than 500 and faster than 10

``` r
pat <- gen_word_list(txt, 500) # your vector of patterns
d <- data.frame(txt_col = txt[1001:2000], match = FALSE)

# example - get rows that match to any pattern
match_mask <- unlist(vecgrepl(d$txt_col, pat, match = "first"))
d[match_mask, "match"] <- TRUE

head(d[grepl_any(pat, d$txt_col), ])
```

                                             txt_col match
    1       Even of five hundred courses of the sun,  TRUE
    2       Show me your image in some antique book,  TRUE
    3     Since mind at first in character was done.  TRUE
    4 That I might see what the old world could say,  TRUE
    5         To this composed wonder of your frame,  TRUE
    6 Whether we are mended, or whether better they,  TRUE

``` r
head(subset(d, pat %grepin% d$txt_col)) # equivalent
```

                                             txt_col match
    1       Even of five hundred courses of the sun,  TRUE
    2       Show me your image in some antique book,  TRUE
    3     Since mind at first in character was done.  TRUE
    4 That I might see what the old world could say,  TRUE
    5         To this composed wonder of your frame,  TRUE
    6 Whether we are mended, or whether better they,  TRUE

``` r
# example - get first/last pattern that matches to each string
d <- transform(d,
               first = unlist(vecgrep(txt_col, pat,
                                      keepdim = TRUE, value = TRUE, match = "first")),
               last = unlist(vecgrep(txt_col, rev(pat),
                                     keepdim = TRUE, value = TRUE, match = "first"))
               )
head(subset(d, match == TRUE))
```

                                             txt_col match first  last
    1       Even of five hundred courses of the sun,  TRUE    he    he
    2       Show me your image in some antique book,  TRUE    so    so
    3     Since mind at first in character was done.  TRUE   was first
    4 That I might see what the old world could say,  TRUE    he   say
    5         To this composed wonder of your frame,  TRUE   you    me
    6 Whether we are mended, or whether better they,  TRUE    we   men

``` r
# example - get number of matches in a vector
d <- transform(d,
               nmatch = grep_count(pat, txt_col))
head(subset(d, match == TRUE))
```

                                             txt_col match first  last nmatch
    1       Even of five hundred courses of the sun,  TRUE    he    he      1
    2       Show me your image in some antique book,  TRUE    so    so     10
    3     Since mind at first in character was done.  TRUE   was first      2
    4 That I might see what the old world could say,  TRUE    he   say      4
    5         To this composed wonder of your frame,  TRUE   you    me      5
    6 Whether we are mended, or whether better they,  TRUE    we   men     11

``` r
# example - keep dimensions to convert to a data.frame
x <- do.call(rbind,
             vecgrepl(txt[31:35], letters[1:5]))
x <- as.data.frame(cbind(txt[31:35], x))
names(x) <- c("line", letters[1:5])
head(x)
```

                                                  line    a     b     c    d    e
    1     Then being asked, where all thy beauty lies, TRUE  TRUE FALSE TRUE TRUE
    2        Where all the treasure of thy lusty days; TRUE FALSE FALSE TRUE TRUE
    3        To say within thine own deep sunken eyes, TRUE FALSE FALSE TRUE TRUE
    4 Were an all-eating shame, and thriftless praise. TRUE FALSE FALSE TRUE TRUE
    5  How much more praise deserved thy beauty's use, TRUE  TRUE  TRUE TRUE TRUE

``` r
#
# other examples
#

# example - factor speedup when duplicate haystacks
x <- rep(letters, 100)
pat <- letters
microbenchmark(
    grepl_any(pat, x),
    grepl_any(pat, as.factor(x))
)
```

    Unit: microseconds
                             expr    min      lq     mean median      uq    max
                grepl_any(pat, x) 2503.0 2846.55 3128.832 3004.1 3270.85 5078.4
     grepl_any(pat, as.factor(x))  141.6  168.10  243.539  199.4  286.20  668.3
     neval
       100
       100

``` r
# note - if duplicate patterns, user can simply call unique() on pattern vector
```

## Speed

``` r
# test vecgrep on some bigger vectors
hay <- txt
ndl <- words
cat("N Hay =", format(length(hay), big.mark = ","),
    "| N Needle =", format(length(ndl), big.mark = ","), "\n")
```

    N Hay = 124,194 | N Needle = 2,000 

``` r
t0 <- Sys.time()
suppressWarnings({
    x <- vecgrep(hay, ndl, fixed = FALSE, match = "all")
})
difftime(Sys.time(), t0)
```

    Time difference of 3.232874 mins

``` r
#
# returning only the first match is faster
#
t0 <- Sys.time()
suppressWarnings({
    x <- vecgrep(hay, ndl, fixed = FALSE, match = "first")
})
difftime(Sys.time(), t0)
```

    Time difference of 19.45347 secs

``` r
#
# fixed searches are much much faster
# since regex compilation/execution adds quite a bit of time
#
t0 <- Sys.time()
suppressWarnings({
    x <- vecgrep(hay, ndl, fixed = TRUE, match = "all")
})
difftime(Sys.time(), t0)
```

    Time difference of 5.127629 secs

``` r
t0 <- Sys.time()
suppressWarnings({
    x <- vecgrep(hay, ndl, fixed = TRUE, match = "first")
})
difftime(Sys.time(), t0)
```

    Time difference of 0.5578504 secs

## Compare vecgrep with native R solutions

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

#
# lapply is quick, then use stack and split to transpose the list
# (pretty slow)
t_lapply_grep <- function(needles, haystacks) {
    byndl <- lapply(needles, grep, x = haystacks)
    names(byndl) <- seq_along(byndl) # stack requires names
    byhay <- with(utils::stack(byndl), split(ind, values))
    # expand list to be of length haystacks
    out <- lapply(as.character(seq(1, length(haystacks))), \(hay_ix) {
        ixs <- byhay[[hay_ix]] # get needle indices for given haystack
        # ixs is null if there were no matches for this needle
        return(ixs[length(ixs)]) # "last match"
    })
}

# verify methods return same results
shortndls <- words[1:100]
shorttxt <- txt[1:500]
x_loop <- loop_grep(shortndls, shorttxt)
x_lapply <- lapply_grep(shortndls, shorttxt)
x_grepvec <- vecgrep(shorttxt, shortndls, match = "all")
all(unlist(x_loop) == unlist(x_lapply))
```

    [1] TRUE

``` r
# grepvec returns empty vec if no results, others return NULL. make equal
x_grepvec <- lapply(x_grepvec, \(vec) if (length(vec) == 0) NULL else vec[length(vec)])
all(unlist(x_lapply) == unlist(x_grepvec))
```

    [1] TRUE

``` r
microbenchmark(loop_grep(shortndls, shorttxt),
               lapply_grep(shortndls, shorttxt),
               vecgrep(shorttxt, shortndls),
               times = 10)
```

    Unit: milliseconds
                                 expr     min      lq     mean  median      uq
       loop_grep(shortndls, shorttxt) 21.7421 22.5561 28.30216 26.7590 34.0367
     lapply_grep(shortndls, shorttxt) 22.1933 22.5605 27.30439 29.0238 30.9864
         vecgrep(shorttxt, shortndls) 33.6157 33.7571 36.99934 34.7196 34.7771
         max neval
     37.0058    10
     31.8017    10
     49.3853    10

``` r
microbenchmark(
    grep("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"),
    vecgrep("some-email$grep.com", "^[[:alnum:]._-]+@[[:alnum:].-]+$")
)
```

    Unit: microseconds
                                                                   expr  min   lq
        grep("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com") 16.3 17.2
     vecgrep("some-email$grep.com", "^[[:alnum:]._-]+@[[:alnum:].-]+$") 48.5 50.2
       mean median    uq   max neval
     19.328   19.0 19.60  40.6   100
     55.481   51.3 52.25 327.8   100

``` r
microbenchmark(
    grep("([^ @]+)@([^ @]+)", "name@server.com"),
    vecgrep("name@server.com", "([^ @]+)@([^ @]+)")
)
```

    Unit: microseconds
                                                expr  min    lq   mean median    uq
        grep("([^ @]+)@([^ @]+)", "name@server.com")  5.0  5.35  6.895   6.70  7.10
     vecgrep("name@server.com", "([^ @]+)@([^ @]+)") 26.2 27.05 28.899  27.75 28.35
       max neval
      55.0   100
     102.3   100

``` r
p <- gen_word_list(txt, n = 1)
cat("regex:", p, "\n")
```

    regex: has 

``` r
microbenchmark(
    grep(p, txt),
    vecgrep(txt, p)
)
```

    Unit: milliseconds
                expr      min        lq     mean    median        uq      max neval
        grep(p, txt)  62.8447  65.73635  73.8310  71.26345  78.36615 102.9796   100
     vecgrep(txt, p) 102.2551 110.96270 125.6342 121.16730 137.16210 201.1386   100

## Encodings

``` r
Sys.getlocale("LC_CTYPE")
```

    [1] "C.UTF-8"

``` r
# load some text with a latin-1 encoding
(lat1 <- load_ext_data("latin1.txt", n = 1))
```

    [1] "premi\xe8re is first"

``` r
pat <- "première"
vecgrep(lat1, pat)
```

    Warning in vecgrep(lat1, pat): invalid multibyte sequence in haystack string 1.
    Check the encodings of the input vectors.

    [[1]]
    integer(0)

``` r
try(grep(pat, lat1))
```

    Warning in grep(pat, lat1): unable to translate 'premi<e8>re is first' to a
    wide string

    Warning in grep(pat, lat1): input string 1 is invalid

    integer(0)

``` r
# cat(paste("Text:", lat1, "\nEncoding:", Encoding(lat1), "\n"))
Encoding(lat1) <- "latin1"
cat(paste("Text:", lat1, "\nSet Encoding:", Encoding(lat1), "\n"))
```

    Text: première is first 
    Set Encoding: latin1 

``` r
pat <- "première"
cat("Pattern:", pat, "\nEncoding:", Encoding(pat), "\n")
```

    Pattern: première 
    Encoding: UTF-8 

``` r
vecgrep(lat1, pat)
```

    [[1]]
    [1] 1

``` r
grep(pat, lat1)
```

    [1] 1

``` r
grep(pat, lat1, useBytes = TRUE)
```

    integer(0)

``` r
vecgrep(lat1, pat, use_bytes = TRUE)
```

    [[1]]
    integer(0)
