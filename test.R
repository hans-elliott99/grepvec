source("grepvec.R")
shakespeare_url <- "https://ocw.mit.edu/ans7870/6/6.006/s08/lecturenotes/files/t8.shakespeare.txt"

# TODO: compare w old C vector method

gen_word_list <- function(lines, n = 10) {
    li <- as.integer(runif(n, 1, length(lines)))
    words <- paste(lines[li], collapse = " ")
    words <- unique(strsplit(words, " ")[[1]])
    words <- words[words != ""]
    wi <- as.integer(runif(n, 1, length(words)))
    return(words[wi])
}
txt <- readLines(shakespeare_url)
txt <- trimws(txt)
words <- gen_word_list(txt, n = 2000)

hay <- c(txt, txt, txt, txt)
ndl <- words

cat("N Hay =", format(length(hay), big.mark=","),
    "| N Needle =", format(length(ndl), big.mark=","), fill=TRUE)
# N Hay = 497,824 | N Needle = 2,000

t0 <- Sys.time()
x <- grepvec(hay, ndl, fixed = FALSE, matchrule = "all")
t1 <- Sys.time()
difftime(t1, t0)
# ~2.8 mins

# large Ns - causes stack overflow on my sys w/out dynammic alloc, and cause
# PROTECTion stack overflow if list elemements are not unprotected as soon as
# added to list
w2 <- gen_word_list(txt, length(txt))
w2[1] <- "HANS"
hans_idx <- c((1:100)^2, length(txt))
t2 <- txt
t2[hans_idx] <- "HANS"

# basically a 15 billion element matrix
cat("N Hay =", format(length(t2), big.mark=","),
    "| N Needle =", format(length(w2), big.mark=","), fill=TRUE)
t0 <- Sys.time()
x <- grepvec(t2, w2, fixed = FALSE, matchrule = "first")
t1 <- Sys.time()
difftime(t1, t0)
# ~4 mins


all(which(unlist(x) == 1) == hans_idx)
# TRUE