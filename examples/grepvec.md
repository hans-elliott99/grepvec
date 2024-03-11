# grepvec demo


- [Examples](#examples)
- [Speed](#speed)
- [Compare grepvec with native R
  solutions](#compare-grepvec-with-native-r-solutions)
- [Encodings](#encodings)

2024-03-10 17:43:22.003366

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
    words <- words[words != ""]
    words <- words[as.integer(runif(n, 1, length(words)))]
    words <- gsub("[^[:alnum:] ]", "", words)
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
grepl("gr(a|e)y", c("grey", "gray"))
```

    [1] TRUE TRUE

``` r
greplvec("gr(a|e)y", c("grey", "gray"))
```

    [[1]]
    [1] TRUE TRUE

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

    Time difference of 0.4134016 secs

``` r
# base R version
t0 = Sys.time()
m2 <- lapply(words, grep, x = txt[1:500])
difftime(Sys.time(), t0)
```

    Time difference of 0.4290156 secs

``` r
microbenchmark(
    grepvec(words, txt[1:500]),
    lapply(words, grepl, x = txt[1:500]),
    times = 10
)
```

    Unit: milliseconds
                                     expr      min       lq     mean   median
               grepvec(words, txt[1:500]) 428.6035 440.2078 462.8100 458.0745
     lapply(words, grepl, x = txt[1:500]) 428.8009 436.4146 472.2485 454.4562
           uq      max neval
     477.0245 514.8989    10
     480.7258 600.7642    10

By default, if no match is found a 0 length vector is returned.  
This can be convenient if you want to compare the number of string
matched to each pattern, via `lengths`.  
However, if you are want the output to have a consistent shape, you can
use option ‘keepdim = TRUE’.

``` r
# keepdim = TRUE:
grepvec(letters[1:3], letters, keepdim = TRUE)
```

    [[1]]
     [1]  1 NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
    [26] NA

    [[2]]
     [1] NA  2 NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
    [26] NA

    [[3]]
     [1] NA NA  3 NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
    [26] NA

``` r
# be careful when unlisting results
res <- grepvec(words[1:100], tail(txt, 500), match = "first", value = TRUE)
head(res)
```

    [[1]]
    character(0)

    [[2]]
    [1] "'\"How mighty then you are, O hear me tell!"

    [[3]]
    [1] "If best were as it was, or best without."

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

    [1] 36

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
# alternatively, you can use keepdim = TRUE
res <- grepvec(words[1:100], tail(txt, 500),
               match = "first", value = TRUE, keepdim = TRUE)
head(res)
```

    [[1]]
    [1] NA

    [[2]]
    [1] "'\"How mighty then you are, O hear me tell!"

    [[3]]
    [1] "If best were as it was, or best without."

    [[4]]
    [1] NA

    [[5]]
    [1] NA

    [[6]]
    [1] NA

``` r
length(res) == unlist(length(res))
```

    [1] TRUE

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
    7          Or whether revolution be the same. FALSE
    10                                            FALSE
    11                                            FALSE
    12                                         60 FALSE
    22 And delves the parallels in beauty's brow, FALSE
    27                                            FALSE

``` r
# or just filter data.frame based on matches to a vector
head(d[unique(unlist(grepvec(pat, d$txt_col, match = "first"))), ])
```

                                                  txt_col match
    127 Tired with all these, from these would I be gone,  TRUE
    169     Uttering bare truth, even so as foes commend.  TRUE
    468        Then lacked I matter, that enfeebled mine.  TRUE
    32     Dost thou desire my slumbers should be broken,  TRUE
    394    Some fresher stamp of the time-bettering days.  TRUE
    421     Who is it that says most, which can say more,  TRUE

``` r
# example - create columns based on matches
keywords <- gen_word_list(d$txt_col, 3)
cat("searching for patterns:", paste0(keywords, collapse = ", "), "\n")
```

    searching for patterns: before, long, thou 

``` r
d[, keywords] <- 0
(ixs <- unlist(grepvec(keywords, d$txt_col, match = "first")))
```

    [1]  15 145  32

``` r
for (i in seq_along(keywords))
    d[ixs[i], keywords[i]] <- 1
## see some results
d[c(1:2, unlist(ixs)), ]
```

                                                 txt_col match before long thou
    1           Even of five hundred courses of the sun,  TRUE      0    0    0
    2           Show me your image in some antique book,  TRUE      0    0    0
    15  Each changing place with that which goes before,  TRUE      1    0    0
    145    In days long since, before these last so bad.  TRUE      0    1    0
    32    Dost thou desire my slumbers should be broken,  TRUE      0    0    1

``` r
# example - getting the last match is as quick and easy as getting the first
grepvec(pat[1:2], txt, match = "first")
```

    [[1]]
    [1] 71

    [[2]]
    [1] 253

``` r
grepvec(pat[1:2], rev(txt), match = "first")
```

    [[1]]
    [1] 668

    [[2]]
    [1] 694

``` r
# example - keep dimensions to convert to a data.frame
x <- do.call(cbind.data.frame,
             grepvec(letters, txt, keepdim = TRUE, use_names = TRUE))
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
             grepvec(LETTERS, txt, keepdim = TRUE, use_names = TRUE))
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
    head(data.table::setDT(grepvec(c("a", "z"), txt,
                                    keepdim = TRUE, use_names = TRUE, value = TRUE)))
```

                            a      z
                       <char> <char>
    1:                   <NA>   <NA>
    2:                   <NA>   <NA>
    3:                   <NA>   <NA>
    4:                   <NA>   <NA>
    5: by William Shakespeare   <NA>
    6:                   <NA>   <NA>

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

    Time difference of 3.730986 mins

``` r
#
# returning only the first match is faster
#
t0 <- Sys.time()
suppressWarnings({
    x <- grepvec(ndl, hay, fixed = FALSE, match = "first")
})
difftime(Sys.time(), t0)
```

    Time difference of 21.6817 secs

``` r
#
# fixed searches are much much faster
# since regex compilation/execution adds quite a bit of time
#
t0 <- Sys.time()
suppressWarnings({
    x <- grepvec(ndl, hay, fixed = TRUE, match = "all")
})
difftime(Sys.time(), t0)
```

    Time difference of 10.78141 secs

``` r
t0 <- Sys.time()
suppressWarnings({
    x <- grepvec(ndl, hay, fixed = TRUE, match = "first")
})
difftime(Sys.time(), t0)
```

    Time difference of 0.8776286 secs

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
all(unlist(x_lapply) == unlist(x_grepvec))
```

    [1] TRUE

``` r
microbenchmark(loop_grep(shortndls, shorttxt),
               lapply_grep_lambda(shortndls, shorttxt),
               lapply_grep(shortndls, shorttxt),
               grepvec(shortndls, shorttxt),
               times = 10)
```

    Unit: milliseconds
                                        expr      min       lq     mean   median
              loop_grep(shortndls, shorttxt) 410.6130 417.0674 429.6828 427.7767
     lapply_grep_lambda(shortndls, shorttxt) 425.5619 427.5859 436.6493 437.2591
            lapply_grep(shortndls, shorttxt) 413.6365 422.7303 434.0889 434.0046
                grepvec(shortndls, shorttxt) 401.5897 405.8104 420.5835 423.2233
           uq      max neval
     441.8245 449.5912    10
     442.0465 448.3361    10
     450.0992 455.4186    10
     430.5148 439.3942    10

Some comparisons with `base::grep`:

`grep` is typically faster when searching for a single pattern in a
single string, which makes sense considering `grepvec` has some extra
overhead. The difference in performance is variable and depends on the
regular expression.

`grepvec` may become slightly faster when comparing a pattern with a
bunch of strings (see the last example), but this can vary quite a bit
based on the regular expression.

`grepvec` will be most useful when searching for a vector of patterns in
a vector of strings.

``` r
microbenchmark(
    grep("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com"),
    grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com")
)
```

    Unit: microseconds
                                                                   expr  min   lq
        grep("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com") 13.3 13.9
     grepvec("^[[:alnum:]._-]+@[[:alnum:].-]+$", "some-email@grep.com") 26.9 28.0
       mean median   uq   max neval
     14.922   14.6 15.0  33.0   100
     30.862   28.8 29.3 119.5   100

``` r
microbenchmark(
    grep("([^ @]+)@([^ @]+)", "name@server.com"),
    grepvec("([^ @]+)@([^ @]+)", "name@server.com")
)
```

    Unit: microseconds
                                                expr  min   lq   mean median   uq
        grep("([^ @]+)@([^ @]+)", "name@server.com")  4.1  4.6  5.278   5.35  5.6
     grepvec("([^ @]+)@([^ @]+)", "name@server.com") 16.4 17.4 19.607  17.75 18.5
      max neval
     15.8   100
     78.7   100

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
      5.4  5.80  6.811   6.45  6.80 27.5   100
     17.9 18.55 20.664  19.05 19.55 66.6   100

``` r
p <- gen_word_list(txt, n = 1)
microbenchmark(
    grep(p, txt),
    grepvec(p, txt)
)
```

    Unit: milliseconds
                expr     min       lq     mean   median       uq     max neval
        grep(p, txt) 60.4518 64.23655 65.03119 64.94585 65.67475 73.5548   100
     grepvec(p, txt) 59.1060 62.73815 63.82789 63.80185 64.89565 71.7922   100

``` r
cat("regex:", p, "\n")
```

    regex: hand 

## Encodings

R supports many differenct character encodings. When you load text data
into R, R has no way to know for certain what encoding scheme the bytes
in its memory correspond to.  
Grepvec takes care to behave similairly to grep in dealing with
character encodings. If the encoding of a string has been set (e.g., via
`iconv` or `Encoding`), grepvec can treat the string appropriately.

In the example below, `pat = "première"` has encoding UTF8 (because
that’s the default in my locale) but the string stored in `lat1` has a
Latin-1 encoding. If the encoding of `lat1` is not identified before it
is passed to grepvec or grep, these functions will error because they
cannot determine how the characters should be treated.  
Fundamentally, although the UTF8-encoded “première” and the Latin-1
version appear identical on our screens, different sequences of bytes
are used to represent the strings.

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
try(grepvec(pat, lat1))
```

    Warning in grepvec(pat, lat1): invalid multibyte sequence in haystack string 1.
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
grepvec(pat, lat1)
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
grepvec(pat, lat1, use_bytes = TRUE)
```

    [[1]]
    integer(0)

``` r
Encoding(pat)
```

    [1] "UTF-8"

``` r
Encoding(lat1)
```

    [1] "latin1"

``` r
grepvec(pat, iconv(lat1, to = "UTF-8"))
```

    [[1]]
    [1] 1

``` r
grepvec(iconv(pat, to = "latin1"), lat1)
```

    [[1]]
    [1] 1

``` r
grepvec(iconv(pat, to = "UTF-8"), iconv(lat1, to = "UTF-8"), fixed = TRUE)
```

    [[1]]
    [1] 1

``` r
grepvec(iconv(pat, to = "UTF-8"), iconv(lat1, to = "UTF-8"), fixed = FALSE)
```

    [[1]]
    [1] 1

``` r
grep("premi.re", lat1)
```

    [1] 1

``` r
grepvec("premi.re", lat1)
```

    [[1]]
    [1] 1

``` r
# if the character encodings don't match but the characters are equal the regex
# should still work...

# these strings are encoded differently, and thus their bytes do not match
pat <- iconv("prem.ère", to = "latin1")
x <- "première"
# accented e (ie, "e with grave") is a single byte in latin1, 2 bytes in utf8
charToRaw(pat)
```

    [1] 70 72 65 6d 2e e8 72 65

``` r
charToRaw(x) 
```

    [1] 70 72 65 6d 69 c3 a8 72 65

``` r
grep(pat, x)
```

    [1] 1

``` r
grepvec(pat, x)
```

    [[1]]
    [1] 1

``` r
grep(pat, iconv(x, to = "latin1"))
```

    [1] 1

``` r
grepvec(pat, iconv(x, to = "latin1"))
```

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

    [[1]]
    [1] 1

``` r
grepvec(p, toupper(lat1), fixed = TRUE)
```

    [[1]]
    [1] 1

``` r
(ex <- load_ext_data("utf8.txt"))
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

    [[1]]
    integer(0)

``` r
charToRaw("K") # ascii K
```

    [1] 4b

``` r
charToRaw("К")
```

    [1] d0 9a

``` r
grep("Кириллица", ex)
```

    [1] 3

``` r
grepvec("Кириллица", ex)
```

    [[1]]
    [1] 3
