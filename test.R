source("grepvec.R")
shakespeare_url <- "https://ocw.mit.edu/ans7870/6/6.006/s08/lecturenotes/files/t8.shakespeare.txt"

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

hay <- c(txt) #, txt, txt, txt)
ndl <- words

cat("N Hay =", format(length(hay), big.mark=","),
    "| N Needle =", format(length(ndl), big.mark=","), fill=TRUE)
t0 <- Sys.time()
x <- grepvec(hay, ndl, usefirst=FALSE)
t1 <- Sys.time()
difftime(t1, t0)


# large Ns - causes stack overflow on my sys w/out dynammic alloc
w2 <- gen_word_list(txt, length(txt))
w2[1] <- "HANS"
hans_idx <- c((1:100)^2, length(txt))
t2 <- txt
t2[hans_idx] <- "HANS"

t0 <- Sys.time()
x <- grepvec(t2, w2, usefirst=TRUE)
t1 <- Sys.time()
difftime(t1, t0)

all(which(x == 1) == hans_idx)
