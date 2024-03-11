# vecgrep demo


- [Examples](#examples)
- [Speed](#speed)
- [Compare vecgrep with native R
  solutions](#compare-vecgrep-with-native-r-solutions)
- [Encodings](#encodings)

2024-03-10 17:49:57.607492

``` r
# compiled on:
Sys.info()[c("sysname", "release", "version", "machine")]
```

                                 sysname                              release 
                                 "Linux"  "5.10.16.3-microsoft-standard-WSL2" 
                                 version                              machine 
    "#1 SMP Fri Apr 2 22:23:49 UTC 2021"                             "x86_64" 

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
words <- gen_word_list(txt, n = 2000)

t0 = Sys.time()
m <- vecgrep(txt[1:500], words, match = "all")
difftime(Sys.time(), t0)
```

    Time difference of 0.3741214 secs

``` r
pat <- gen_word_list(txt, 500) # your vector of patterns
d <- data.frame(txt_col = txt[1001:2000], match = FALSE)

# example - get rows that match to any pattern
match_mask <- unlist(vecgrepl(d$txt_col, pat, match = "first"))
d[match_mask, "match"] <- TRUE


# example - get first/last pattern that matches to each string
first = unlist(vecgrep(d$txt_col, pat, keepdim = TRUE, value = TRUE, match = "first"))
d <- transform(d,
               first = unlist(vecgrep(txt_col, pat,
                                      keepdim = TRUE, value = TRUE, match = "first")),
               last = unlist(vecgrep(txt_col, rev(pat),
                                     keepdim = TRUE, value = TRUE, match = "first"))
               )
head(subset(d, match == TRUE))
```

                                             txt_col match first last
    1       Even of five hundred courses of the sun,  TRUE    th   th
    2       Show me your image in some antique book,  TRUE    me  you
    3     Since mind at first in character was done.  TRUE    at    a
    4 That I might see what the old world could say,  TRUE     I That
    5         To this composed wonder of your frame,  TRUE    is   To
    6 Whether we are mended, or whether better they,  TRUE    me   or

``` r
# example - keep dimensions to convert to a data.frame
x <- do.call(rbind,
             vecgrep(txt[31:35], letters[1:5], keepdim = TRUE))
x <- as.data.frame(cbind(txt[31:35], x))
names(x) <- c("line", letters[1:5])
head(x)
```

                                                  line a    b    c d e
    1     Then being asked, where all thy beauty lies, 1    2 <NA> 4 5
    2        Where all the treasure of thy lusty days; 1 <NA> <NA> 4 5
    3        To say within thine own deep sunken eyes, 1 <NA> <NA> 4 5
    4 Were an all-eating shame, and thriftless praise. 1 <NA> <NA> 4 5
    5  How much more praise deserved thy beauty's use, 1    2    3 4 5

## Speed

``` r
# test vecgrep on some bigger vectors
hay <- c(txt, txt)
ndl <- words
cat("N Hay =", format(length(hay), big.mark = ","),
    "| N Needle =", format(length(ndl), big.mark = ","), "\n")
```

    N Hay = 248,388 | N Needle = 2,000 

``` r
t0 <- Sys.time()
suppressWarnings({
    x <- vecgrep(hay, ndl, fixed = FALSE, match = "all")
})
difftime(Sys.time(), t0)
```

    Time difference of 3.313463 mins

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

    Time difference of 17.47421 secs

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

    Time difference of 7.027064 secs

``` r
t0 <- Sys.time()
suppressWarnings({
    x <- vecgrep(hay, ndl, fixed = TRUE, match = "first")
})
difftime(Sys.time(), t0)
```

    Time difference of 0.7360201 secs

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
       loop_grep(shortndls, shorttxt) 20.2980 21.4206 22.27238 22.1877 22.8881
     lapply_grep(shortndls, shorttxt) 21.0915 21.4443 22.70309 22.7270 23.8800
         vecgrep(shorttxt, shortndls) 18.9395 19.1817 20.36243 20.3597 21.2187
         max neval
     24.8425    10
     24.7850    10
     22.4110    10

``` r
microbenchmark(
    grep("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"),
    vecgrep("some-email$grep.com", "^[[:alnum:]._-]+@[[:alnum:].-]+$")
)
```

    Unit: microseconds
                                                                   expr  min   lq
        grep("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com") 13.9 14.3
     vecgrep("some-email$grep.com", "^[[:alnum:]._-]+@[[:alnum:].-]+$") 27.0 27.7
       mean median    uq   max neval
     15.239  15.05 15.40  30.1   100
     30.654  28.60 29.85 141.1   100

``` r
microbenchmark(
    grep("([^ @]+)@([^ @]+)", "name@server.com"),
    vecgrep("name@server.com", "([^ @]+)@([^ @]+)")
)
```

    Unit: microseconds
                                                expr  min   lq   mean median    uq
        grep("([^ @]+)@([^ @]+)", "name@server.com")  4.2  4.5  5.403    5.4  5.55
     vecgrep("name@server.com", "([^ @]+)@([^ @]+)") 16.2 16.9 18.907   17.3 17.70
      max neval
     18.5   100
     72.7   100

``` r
p <- gen_word_list(txt, n = 1)
microbenchmark(
    grep(p, txt),
    vecgrep(txt, p)
)
```

    Unit: milliseconds
                expr     min      lq     mean  median       uq      max neval
        grep(p, txt) 52.8362 54.6549 56.19072 55.9402 57.25255  67.0220   100
     vecgrep(txt, p) 55.1091 59.1243 63.98801 61.1709 66.63960 134.4896   100

``` r
cat("regex:", p, "\n")
```

    regex: Haply 

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
