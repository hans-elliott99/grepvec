# devtools::load_all() # or devtools::install() may be needed
library(grepvec)
library(microbenchmark)

shakespeare_url <- "https://ocw.mit.edu/ans7870/6/6.006/s08/lecturenotes/files/t8.shakespeare.txt"

# get some random words from the txt to use as patterns
gen_word_list <- function(lines, n = 10) {
    li <- as.integer(runif(n, 1, length(lines)))
    words <- paste(lines[li], collapse = " ")
    words <- unique(strsplit(words, " ")[[1]])
    words <- words[words != ""]
    wi <- as.integer(runif(n, 1, length(words)))
    return(words[wi])
}

txt <- trimws(readLines(shakespeare_url))
words <- gen_word_list(txt, n = 2000)

grepvec(txt[1:1000], words)

# test grepvec
hay <- c(txt, txt, txt, txt)
ndl <- words
cat("N Hay =", format(length(hay), big.mark=","),
    "| N Needle =", format(length(ndl), big.mark=","), fill=TRUE)
#> N Hay = 497,824 | N Needle = 2,000

t0 <- Sys.time()
x <- grepvec(hay, ndl, fixed = FALSE, matchrule = "all")
difftime(Sys.time(), t0)
#> ~2.8 mins

# large Ns - causes stack overflow on my sys w/out dynammic alloc, and cause
# PROTECTion stack overflow if list elemements are not unprotected as soon as
# added to list
longndls <- gen_word_list(txt, length(txt))
longndls[1] <- "HANS" #this word is not in shakespeare
hans_idx <- c((1:100)^2, length(txt))
somehay <- txt
somehay[hans_idx] <- "HANS"

# basically a 15 billion element matrix
cat("N Hay =", format(length(somehay), big.mark=","),
    "| N Needle =", format(length(longndls), big.mark=","), fill=TRUE)
t0 <- Sys.time()
x <- grepvec(somehay, longndls, fixed = FALSE, matchrule = "first")
difftime(Sys.time(), t0)
#> ~4 mins

# strings that exactly matched to needle 1 should be those at 'hans_idx'
all(which(unlist(x) == 1) == hans_idx)
#> TRUE


# convert the idx list to character lists, of haystacks or needles
res <- grepvec(txt[1:10], strsplit(txt[1], " ")[[1]])
to_ndl(res, words)
to_hay(res, txt)


# compare with existing R solutions -------------------------------------------
loop_grep <- function(haystacks, needles) {
    x <- vector(mode = "list", length = length(haystacks))
    for (i in seq_along(needles)) {
        try({
            matchinds <- grep(needles[i], haystacks)
            x[matchinds] <- lapply(x[matchinds], \(vec) c(vec, i))
        }, silent = TRUE)
    }
    return(x)
}

lapply_grep <- function(haystacks, needles) {
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
        x[matchinds] <- lapply(x[matchinds], \(vec) c(vec, i))
    }
    return(x)
}

# verify same results
shortndls <- words[1:100]
x_loop <- loop_grep(txt, shortndls)
x_lapply <- lapply_grep(txt, shortndls)
x_grepvec <- grepvec(txt, shortndls)
all(unlist(x_loop) == unlist(x_lapply))
#> TRUE
# grepvec returns 0 if no results, others return NULL. make equal
x_grepvec <- lapply(x_grepvec, \(vec) if (all(vec == 0)) NULL else vec)
all(unlist(x_lapply) == unlist(x_grepvec))
#> TRUE

microbenchmark(loop_grep(txt, shortndls),
               lapply_grep(txt, shortndls),
               grepvec(txt, shortndls),
               times = 10)
#> Unit: seconds
#>                    expr      min       lq     mean   median       uq      max
#>    loop_grep(txt, ndls) 5.942300 6.143407 6.309293 6.238158 6.531905 6.716219
#>  lapply_grep(txt, ndls) 5.946089 6.183130 6.682903 6.651146 6.872943 8.212148
#>      grepvec(txt, ndls) 1.398782 1.454352 1.532908 1.466876 1.561440 1.894956
