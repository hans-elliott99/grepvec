# grepvec demo


- [Examples](#examples)
- [Speed](#speed)
- [Compare grepvec with native R
  solutions](#compare-grepvec-with-native-r-solutions)
- [Encodings](#encodings)

2024-03-06 01:39:39.724077

Notes: - test and translate diff encodings  
- refactored a bit  
- encoding message left on

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

    using char

    [[1]]
    [1] 1 2

``` r
grepvec(c("^a", "\\d"), c("1", "apple"))
```

    using char

    Warning in grepvec(c("^a", "\\d"), c("1", "apple")): invalid regular expression
    '\d': Unknown character class name

    [[1]]
    [1] 2

    [[2]]
    integer(0)

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

    using char

    [[1]]
    [1] 1 4

    [[2]]
    [1] 1 3

    [[3]]
    [1] 1 2

``` r
grepvec(letters[1:4], c("AbC", 123), ignore_case = TRUE, value = TRUE)
```

    using char

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

    using char

    [[1]]
    [1] 1

``` r
grepvec("^fixed$", "fixed", fixed = TRUE)
```

    using char

    [[1]]
    integer(0)

``` r
words <- gen_word_list(txt, n = 2000)

t0 = Sys.time()
m <- grepvec(words, txt[1:500], match = "all")
```

    using char

``` r
difftime(Sys.time(), t0)
```

    Time difference of 0.5034442 secs

``` r
# base R version
t0 = Sys.time()
m2 <- lapply(words, grep, x = txt[1:500])
difftime(Sys.time(), t0)
```

    Time difference of 0.4545038 secs

Be careful when unlisting the results, because if no match was found a 0
length vector is returned.

``` r
res <- grepvec(words[1:100], tail(txt, 500), match = "first", value = TRUE)
```

    using char

``` r
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
```

    using char

``` r
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

    using char

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

    using char

    [[1]]
    [1] 226

    [[2]]
    [1] 7565

``` r
grepvec(pat[1:2], rev(txt), match = "first")
```

    using char

    [[1]]
    [1] 417

    [[2]]
    [1] 2841

``` r
# example - keep dimensions to convert to a data.frame
x <- do.call(cbind.data.frame,
             grepvec(letters, txt, keepdim = TRUE, names = TRUE))
```

    using char

``` r
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
```

    using char

``` r
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
if (require("data.table", quietly = TRUE))
    print(data.table::setDT(grepvec(c("a", "z"), txt,
                                    keepdim = TRUE, names = TRUE, value = TRUE)))
```

    using char
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

    using char

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
```

    using char

``` r
difftime(Sys.time(), t0)
```

    Time difference of 4.434608 mins

``` r
#
# returning only the first match is much faster
#
t0 <- Sys.time()
suppressWarnings({
    x <- grepvec(ndl, hay, fixed = FALSE, match = "first")
})
```

    using char

``` r
difftime(Sys.time(), t0)
```

    Time difference of 17.88123 secs

``` r
#
# fixed searches are much much faster
# since regexes don't need be compiled or executed
#
t0 <- Sys.time()
suppressWarnings({
    x <- grepvec(ndl, hay, fixed = TRUE, match = "all")
})
```

    using char

``` r
difftime(Sys.time(), t0)
```

    Time difference of 13.73832 secs

``` r
t0 <- Sys.time()
suppressWarnings({
    x <- grepvec(ndl, hay, fixed = TRUE, match = "first")
})
```

    using char

``` r
difftime(Sys.time(), t0)
```

    Time difference of 0.9125111 secs

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
```

    using char

``` r
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

    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char

    Unit: milliseconds
                                            expr      min       lq     mean
                  loop_grep(shortndls, shorttxt) 471.5450 505.3988 545.7771
                lapply_grep(shortndls, shorttxt) 455.5682 478.7463 531.8067
         lapply_grep_lambda(shortndls, shorttxt) 467.8352 494.6814 590.9643
     grepvec(shortndls, shorttxt, match = "all") 452.5465 497.6271 523.0776
       median       uq      max neval
     537.4674 598.8033 634.1333    10
     496.6791 539.6561 724.0494    10
     506.2174 633.7066 992.6495    10
     504.5117 566.5447 585.8278    10

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

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    using char

    Warning in grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"):
    invalid regular expression '^[[:alnum:]._-]+@[[:alnum:].-]+$': Unknown
    character class name

    Unit: microseconds
                                                                   expr  min     lq
        grep("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com") 15.8  20.00
     grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com") 86.2 102.55
        mean median     uq   max neval
      33.663  26.35  39.50 130.5   100
     157.829 149.65 195.35 354.8   100

``` r
microbenchmark(
    grep("([^ @]+)@([^ @]+)", "name@server.com"),
    grepvec("([^ @]+)@([^ @]+)", "name@server.com")
)
```

    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char

    Unit: microseconds
                                                expr  min   lq   mean median    uq
        grep("([^ @]+)@([^ @]+)", "name@server.com")  4.7  5.2  7.071   6.10  6.95
     grepvec("([^ @]+)@([^ @]+)", "name@server.com") 18.2 19.6 24.550  20.65 24.95
      max neval
     43.1   100
     84.2   100

``` r
microbenchmark(
    grep("([0-9][0-9]?)/([0-9][0-9]?)/([0-9][0-9]([0-9][0-9])?)", c("01/01/1996", "2001-01-1")),
    grepvec("([0-9][0-9]?)/([0-9][0-9]?)/([0-9][0-9]([0-9][0-9])?)", c("01/01/1996", "2001-01-1"))
)
```

    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char

    Unit: microseconds
                                                                                                    expr
        grep("([0-9][0-9]?)/([0-9][0-9]?)/([0-9][0-9]([0-9][0-9])?)",      c("01/01/1996", "2001-01-1"))
     grepvec("([0-9][0-9]?)/([0-9][0-9]?)/([0-9][0-9]([0-9][0-9])?)",      c("01/01/1996", "2001-01-1"))
      min    lq  mean median   uq  max neval
      6.2  6.75  8.05    7.6  8.0 46.0   100
     20.2 21.00 22.76   21.7 22.1 84.1   100

``` r
p <- gen_word_list(txt, n = 1)
microbenchmark(
    grep(p, txt),
    grepvec(p, txt)
)
```

    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char
    using char

    Unit: milliseconds
                expr     min      lq     mean   median       uq      max neval
        grep(p, txt) 54.0313 63.7548 75.12590 73.48435 83.49620 148.7196   100
     grepvec(p, txt) 59.0619 66.0396 77.35004 76.05475 86.35665 111.6660   100

``` r
cat("regex:", p, "\n")
```

    regex: The 

## Encodings

R supports many differenct character encodings. When you load text data
into R, R has no way to know for certain what encoding scheme the bytes
in its memory correspond to.  
Grepvec takes care to behave similairly to grep in dealing with
character encodings. If the encoding of a string has been set (e.g., via
`iconv`, `Encoding`, or something else), grepvec can treat the string
appropriately.

In the example below, `pat = "première"` has encoding UTF8 (because
that’s the default in my locale) but the string stored in `lat1` has a
Latin-1 encoding. If the encoding of `lat1` is not identified before it
is passed to grepvec or grep, these functions will error because they
cannot determine how the characters should be treated.  
Fundamentally, although the UTF8-encode “première” and the Latin-1
version appear identical on our screens, different sequences of bytes
are used to represent the strings.

``` r
Sys.getlocale("LC_CTYPE")
```

    [1] "C.UTF-8"

``` r
# load some text with a latin-1 encoding
lat1 <- readLines("https://raw.githubusercontent.com/stain/encoding-test-files/master/latin1.txt",
                n = 1)
pat <- "première"
try(grepvec(pat, lat1))
```

    using wchar

    Warning in grepvec(pat, lat1): invalid multibyte sequence in haystack string 1.

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
grepvec(pat, lat1)
```

    using wchar

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
grepvec(pat, lat1, use_bytes = TRUE)
```

    using char

    [[1]]
    integer(0)

``` r
Encoding(pat)
```

    [1] "UTF-8"

``` r
grepvec(pat, iconv(lat1, to = "UTF8"))
```

    using wchar

    [[1]]
    [1] 1

``` r
Encoding(lat1)
```

    [1] "latin1"

``` r
grepvec(iconv(pat, to = "latin1"), lat1)
```

    using wchar

    [[1]]
    [1] 1

``` r
grepvec(iconv(pat, to = "UTF8"), iconv(lat1, to = "UTF8"), fixed = TRUE)
```

    using utf8

    [[1]]
    [1] 1

``` r
grep("premi.re", lat1)
```

    [1] 1

``` r
grepvec("premi.re", lat1)
```

    using char

    [[1]]
    [1] 1

``` r
(p <- rawToChar(as.raw(200)))
```

    [1] "\xc8"

``` r
Encoding(p) <- "latin1"
p
```

    [1] "È"

``` r
grep(p, lat1, ignore.case = TRUE)
```

    [1] 1

``` r
grepvec(p, lat1, ignore_case = TRUE)
```

    using wchar

    [[1]]
    [1] 1

``` r
grepvec(p, toupper(lat1), fixed = TRUE)
```

    using native

    [[1]]
    [1] 1

``` r
(ex <- readLines("https://raw.githubusercontent.com/stain/encoding-test-files/master/koi8_r.txt")[3])
```

    [1] "\xeb\xc9\xd2\xc9\xcc\xcc\xc9\xc3\xc1 is Cyrillic"

``` r
grep("Cyr", ex)
```

    Warning in grep("Cyr", ex): unable to translate
    '<eb><c9><d2><c9><cc><cc><c9><c3><c1> is Cyrillic' to a wide string

    Warning in grep("Cyr", ex): input string 1 is invalid

    integer(0)

``` r
grepvec("Cyr", ex)
```

    using char

    [[1]]
    [1] 1

``` r
(ex <- readLines("https://raw.githubusercontent.com/stain/encoding-test-files/master/utf8.txt"))
```

    [1] "première is first"              "première is slightly different"
    [3] "Кириллица is Cyrillic"          "𐐀 am Deseret"                  

``` r
Encoding(ex) <- "UTF-8"

grep("K", ex)
```

    integer(0)

``` r
grepvec("K", ex)
```

    using char

    [[1]]
    integer(0)

``` r
grep("Кириллица", ex)
```

    [1] 3

``` r
grepvec("Кириллица", ex)
```

    using wchar

    [[1]]
    [1] 3

TODO: what if text encoding is very strange, will it get handled? try to
break things…
