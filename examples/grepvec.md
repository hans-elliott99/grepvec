# grepvec demo


- [Examples](#examples)
- [Speed](#speed)
- [Compare grepvec with native R
  solutions](#compare-grepvec-with-native-r-solutions)
- [Encodings](#encodings)

2024-03-02 10:54:04.726051

Note: use xlength instead of length

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

#
# create some data
#
txt <- tryCatch(
    readLines("../inst/extdata/shakespeare.txt"),
    warning = \(e) readLines("./inst/extdata/shakespeare.txt")
)
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
    words <- words[words != ""]
    words <- words[as.integer(runif(n, 1, length(words)))]
    words <- gsub("\\[|\\]|\\(|\\)", "", words)
    return(words)
}
```

## Examples

``` r
grep("gr(a|e)y", c("grey", "gray"))
```

    [1] 1 2

``` r
grepvec("gr(a|e)y", c("grey", "gray"))
```

    [[1]]
    [1] 1 2

``` r
grepvec(c("^a", "\\d"), c("1", "apple"))
```

    [[1]]
    [1] 2

    [[2]]
    [1] 1

``` r
lapply(c("^a", "\\d"), \(p) grep(p, c("1", "apple")))
```

    [[1]]
    [1] 2

    [[2]]
    [1] 1

``` r
grepvec(c("one", "possib", "many"),
        c("Some text which might possibly contain one of many keywords",
          "Another string without many words",
          "A third impossibly boring string",
          "Done"))
```

    [[1]]
    [1] 1 4

    [[2]]
    [1] 1 3

    [[3]]
    [1] 1 2

``` r
grepvec(letters[1:4], c("AbC", 123), ignore_case = TRUE, value = TRUE)
```

    [[1]]
    [1] "AbC"

    [[2]]
    [1] "AbC"

    [[3]]
    [1] "AbC"

    [[4]]
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

``` r
words <- gen_word_list(txt, n = 2000)

t0 = Sys.time()
m <- grepvec(words, txt[1:500], match = "all")
difftime(Sys.time(), t0)
```

    Time difference of 0.2034526 secs

``` r
# base R version
t0 = Sys.time()
m2 <- lapply(words, grep, x = txt[1:500])
difftime(Sys.time(), t0)
```

    Time difference of 0.4147382 secs

Be careful when unlisting the results, because if no match was found a 0
length vector is returned.

``` r
res <- grepvec(words[1:100], tail(txt, 500), match = "first", value = TRUE)
head(res)
```

    [[1]]
    character(0)

    [[2]]
    [1] "'\"How mighty then you are, O hear me tell!"

    [[3]]
    character(0)

    [[4]]
    character(0)

    [[5]]
    character(0)

    [[6]]
    character(0)

``` r
length(res)
```

    [1] 100

``` r
length(unlist(res))
```

    [1] 27

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

    [1] 100

``` r
pat <- gen_word_list(txt, 2000) # your vector of patterns
d <- data.frame(txt_col = txt[1001:2000], match = FALSE)

# example - get rows that match any pattern
d[unique(unlist(grepvec(pat, d$txt_col, match = "first"))), "match"] <- TRUE
head(subset(d, match == TRUE))
```

                                             txt_col match
    1       Even of five hundred courses of the sun,  TRUE
    2       Show me your image in some antique book,  TRUE
    3     Since mind at first in character was done.  TRUE
    4 That I might see what the old world could say,  TRUE
    5         To this composed wonder of your frame,  TRUE
    6 Whether we are mended, or whether better they,  TRUE

``` r
head(subset(d, match == FALSE))
```

                                       txt_col match
    10                                         FALSE
    11                                         FALSE
    12                                      60 FALSE
    27                                         FALSE
    28                                         FALSE
    41 To play the watchman ever for thy sake. FALSE

``` r
# example - create columns based on matches
keywords <- gen_word_list(d$txt_col, 3)
cat("searching for patterns:", paste0(keywords, collapse = ", "), "\n")
```

    searching for patterns: my, muse, what 

``` r
d[, keywords] <- 0
(ixs <- unlist(grepvec(keywords, d$txt_col, match = "first")))
```

    [1]  25 319   4

``` r
for (i in seq_along(keywords))
    d[ixs[i], keywords[i]] <- 1
## see some results
d[c(1:2, unlist(ixs)), ]
```

                                               txt_col match my muse what
    1         Even of five hundred courses of the sun,  TRUE  0    0    0
    2         Show me your image in some antique book,  TRUE  0    0    0
    25  And yet to times in hope, my verse shall stand  TRUE  1    0    0
    319        So oft have I invoked thee for my muse, FALSE  0    1    0
    4   That I might see what the old world could say,  TRUE  0    0    1

``` r
# example - getting the last match is as quick and easy as getting the first
grepvec(pat[1:2], txt, match = "first")
```

    [[1]]
    [1] 226

    [[2]]
    [1] 7565

``` r
grepvec(pat[1:2], rev(txt), match = "first")
```

    [[1]]
    [1] 417

    [[2]]
    [1] 2841

``` r
# example - keep dimensions to convert to a data.frame
x <- do.call(cbind.data.frame,
             grepvec(letters, txt, keepdim = TRUE, names = TRUE))
## number of occurrences in all of shakespeare
nocc <- apply(x, MARGIN = 2, FUN = \(y) sum(!is.na(y)))
sort(nocc, decreasing = TRUE)
```

         e      t      o      a      n      r      h      s      i      d      l 
    106410 100988 100003  98070  95266  94498  92847  92196  90806  76956  73853 
         u      m      y      w      f      c      g      b      p      v      k 
     71277  63577  59114  52125  49438  48912  44108  37598  36110  29234  25544 
         x      j      q      z 
      4542   2618   2334   1015 

``` r
# capital letters?
x <- do.call(cbind.data.frame,
             grepvec(LETTERS, txt, keepdim = TRUE, names = TRUE))
## number of occurrences in all of shakespeare
nocc <- apply(x, MARGIN = 2, FUN = \(y) sum(!is.na(y)))
sort(nocc, decreasing = TRUE)
```

        I     A     T     S     E     O     N     R     L     C     H     W     B 
    38331 33075 31892 23619 23436 21914 19914 18834 17777 16525 15738 15236 13175 
        M     D     U     P     G     F     Y     K     V     J     Q     X     Z 
    13034 11859 11259 10283 10099  9505  7941  5914  3481  2009  1168   595   532 

``` r
# example - if you are a data.table user, functions as.data.table and setDT make
# it easy to convert a list of equal length vectors into a table 
if (require("data.table"))
    print(data.table::setDT(grepvec(c("a", "z"), txt,
                                    keepdim = TRUE, names = TRUE, value = TRUE)))
```

    Loading required package: data.table

                                                      a
                                                 <char>
         1:                      by William Shakespeare
         2:  From fairest creatures we desire increase,
         3: That thereby beauty's rose might never die,
         4:    But as the riper should by time decease,
         5:      His tender heir might bear his memory:
        ---                                            
    124190:                                        <NA>
    124191:                                        <NA>
    124192:                                        <NA>
    124193:                                        <NA>
    124194:                                        <NA>
                                                             z
                                                        <char>
         1:          Thy youth's proud livery so gazed on now,
         2:         The lovely gaze where every eye doth dwell
         3:            Gilding the object whereupon it gazeth,
         4: Which steals men's eyes and women's souls amazeth.
         5:      That hath his windows glazed with thine eyes:
        ---                                                   
    124190:                                               <NA>
    124191:                                               <NA>
    124192:                                               <NA>
    124193:                                               <NA>
    124194:                                               <NA>

Invalid regex patterns:

``` r
# what if we use an invalid regex pattern?
grepvec(c("[bad", "(regex"), "those are bad regex patterns")
```

    Warning in grepvec(c("[bad", "(regex"), "those are bad regex patterns"):
    invalid regular expression '[bad': Missing ']'

    Warning in grepvec(c("[bad", "(regex"), "those are bad regex patterns"):
    invalid regular expression '(regex': Missing ')'

    [[1]]
    integer(0)

    [[2]]
    integer(0)

``` r
# grep is similair but throws a warning and errors out
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
hay <- c(txt, txt)
ndl <- words
cat("N Hay =", format(length(hay), big.mark = ","),
    "| N Needle =", format(length(ndl), big.mark = ","), "\n")
```

    N Hay = 248,388 | N Needle = 2,000 

``` r
t0 <- Sys.time()
suppressWarnings({
    x <- grepvec(ndl, hay, fixed = FALSE, match = "all")
})
difftime(Sys.time(), t0)
```

    Time difference of 1.792635 mins

``` r
#
# returning only the first match is much faster
#
t0 <- Sys.time()
suppressWarnings({
    x <- grepvec(ndl, hay, fixed = FALSE, match = "first")
})
difftime(Sys.time(), t0)
```

    Time difference of 6.291326 secs

``` r
#
# fixed searches are much much faster
# since regexes don't need be compiled or executed
#
t0 <- Sys.time()
suppressWarnings({
    x <- grepvec(ndl, hay, fixed = TRUE, match = "all")
})
difftime(Sys.time(), t0)
```

    Time difference of 10.91032 secs

``` r
t0 <- Sys.time()
suppressWarnings({
    x <- grepvec(ndl, hay, fixed = TRUE, match = "first")
})
difftime(Sys.time(), t0)
```

    Time difference of 0.6527908 secs

## Compare grepvec with native R solutions

Suggestions wanted.

``` r
loop_grep <- function(needles, haystacks) {
    x <- vector(mode = "list", length = length(haystacks))
    for (i in seq_along(needles)) {
        x[[i]] <- grep(needles[i], haystacks)
    }
    x
}

lapply_grep <- function(needles, haystacks) {
    lapply(needles, grep, x = haystacks)
}

lapply_grep_lambda <- function(needles, haystacks) {
    lapply(needles, \(p) grep(p, haystacks))
}

# verify same results
shortndls <- words[1:100] 
shorttxt <- txt[1:10000]
x_loop <- loop_grep(shortndls, shorttxt)
x_lapply <- lapply_grep(shortndls, shorttxt)
x_grepvec <- grepvec(shortndls, shorttxt)
all(unlist(x_loop) == unlist(x_lapply))
```

    [1] TRUE

``` r
# grepvec returns empty vec if no results, others return NULL. make equal
# x_grepvec <- lapply(x_grepvec, \(vec) if (length(vec) == 0) NULL else vec)
all(unlist(x_lapply) == unlist(x_grepvec))
```

    [1] TRUE

``` r
microbenchmark(loop_grep(shortndls, shorttxt),
               lapply_grep(shortndls, shorttxt),
               lapply_grep_lambda(shortndls, shorttxt),
               grepvec(shortndls, shorttxt, match = "all"),
               times = 10)
```

    Unit: milliseconds
                                            expr      min       lq     mean
                  loop_grep(shortndls, shorttxt) 454.2725 457.8696 473.8369
                lapply_grep(shortndls, shorttxt) 445.0287 445.9500 477.6572
         lapply_grep_lambda(shortndls, shorttxt) 452.1758 461.0080 490.6828
     grepvec(shortndls, shorttxt, match = "all") 213.5278 214.7112 229.0303
       median       uq      max neval
     463.9407 469.8303 550.2628    10
     460.7358 483.2868 563.8530    10
     475.5170 535.7120 553.8740    10
     221.9986 229.7894 267.5283    10

Some comparisons with `base::grep`:

`grep` is typically faster when searching for a single pattern in a
single string, which makes sense considering `grepvec` has some extra
overhead. The difference in performance is variable and depends on the
regular expression.

`grepvec` may become slightly faster when comparing a pattern with a
bunch of strings (see the last example), but this can vary quite a bit
based on the regular expression.

`grepvec` will be most useful when searching for a vector of patterns in
a vector of strings. As demonstrated above, native R solutions aren’t
quite as fast.

``` r
microbenchmark(
    grep("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"),
    grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com")
)
```

    Unit: microseconds
                                                                   expr  min   lq
        grep("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com") 13.4 13.8
     grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com") 19.7 20.9
       mean median    uq   max neval
     16.546   14.7 15.80  41.0   100
     26.412   22.2 25.85 102.7   100

``` r
microbenchmark(
    grep("([^ @]+)@([^ @]+)", "name@server.com"),
    grepvec("([^ @]+)@([^ @]+)", "name@server.com")
)
```

    Unit: microseconds
                                                expr  min   lq   mean median   uq
        grep("([^ @]+)@([^ @]+)", "name@server.com")  4.1  4.5  5.564    5.2  5.4
     grepvec("([^ @]+)@([^ @]+)", "name@server.com") 15.6 16.1 17.727   16.6 17.1
      max neval
     43.0   100
     71.9   100

``` r
microbenchmark(
    grep("([0-9][0-9]?)/([0-9][0-9]?)/([0-9][0-9]([0-9][0-9])?)", c("01/01/1996", "2001-01-1")),
    grepvec("([0-9][0-9]?)/([0-9][0-9]?)/([0-9][0-9]([0-9][0-9])?)", c("01/01/1996", "2001-01-1"))
)
```

    Unit: microseconds
                                                                                                    expr
        grep("([0-9][0-9]?)/([0-9][0-9]?)/([0-9][0-9]([0-9][0-9])?)",      c("01/01/1996", "2001-01-1"))
     grepvec("([0-9][0-9]?)/([0-9][0-9]?)/([0-9][0-9]([0-9][0-9])?)",      c("01/01/1996", "2001-01-1"))
      min    lq   mean median    uq  max neval
      6.4 10.85 12.191  12.00 12.85 44.6   100
     16.7 30.90 31.897  32.25 33.55 72.0   100

``` r
p <- gen_word_list(txt, n = 1)
microbenchmark(
    grep(p, txt),
    grepvec(p, txt)
)
```

    Unit: milliseconds
                expr     min       lq     mean   median      uq     max neval
        grep(p, txt) 53.6124 59.92450 63.77697 62.45925 67.1125 78.1553   100
     grepvec(p, txt) 18.0961 20.64005 22.50911 21.71730 23.6274 32.9003   100

``` r
cat("regex:", p, "\n")
```

    regex: The 

## Encodings

``` r
latin1 <- readLines("https://raw.githubusercontent.com/stain/encoding-test-files/master/latin1.txt",
                    encoding = "latin-1")

print(latin1)
```

    [1] "premi\xe8re is first"            "premie?re is slightly different"
    [3] "????????? is Cyrillic"           "? am Deseret"                   

``` r
print(iconv(latin1, "latin1", "UTF8"))
```

    [1] "première is first"               "premie?re is slightly different"
    [3] "????????? is Cyrillic"           "? am Deseret"                   

``` r
grep("prem", latin1)
```

    Warning in grep("prem", latin1): unable to translate 'premi<e8>re is first' to
    a wide string

    Warning in grep("prem", latin1): input string 1 is invalid

    [1] 2

``` r
## grepvec will translate to UTF8 internally
grepvec("prem", latin1)
```

    [[1]]
    [1] 1 2

``` r
grepvec("xe8re", latin1[1])
```

    [[1]]
    integer(0)

``` r
grep("xe8re", latin1[1])
```

    Warning in grep("xe8re", latin1[1]): unable to translate 'premi<e8>re is first'
    to a wide string

    Warning in grep("xe8re", latin1[1]): input string 1 is invalid

    integer(0)

``` r
grepvec("\xe8re", latin1[1])
```

    [[1]]
    [1] 1

``` r
try(grep("\xe8re", latin1[1])) # error
```

    Warning in grep("\xe8re", latin1[1]): unable to translate '<e8>re' to a wide
    string

    Error in grep("\xe8re", latin1[1]) : regular expression is invalid

``` r
grep("première", iconv(latin1[1], from = "latin1", to = "UTF8"))
```

    [1] 1

``` r
grepvec("première", iconv(latin1[1], from = "latin1", to = "UTF8"))
```

    [[1]]
    [1] 1

``` r
grep("première", latin1[1])
```

    Warning in grep("première", latin1[1]): unable to translate 'premi<e8>re is
    first' to a wide string

    Warning in grep("première", latin1[1]): input string 1 is invalid

    integer(0)

``` r
grepvec("première", latin1[1])
```

    [[1]]
    integer(0)

``` r
try(grep(latin1[1], latin1[1], fixed = TRUE))
```

    Error in grep(latin1[1], latin1[1], fixed = TRUE) : 
      regular expression is invalid in this locale

``` r
grepvec(latin1[1], latin1[1], fixed = TRUE)
```

    [[1]]
    [1] 1
