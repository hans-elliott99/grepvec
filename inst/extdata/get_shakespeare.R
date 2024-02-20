#!/usr/bin/env Rscript

shakespeare_url <- "https://ocw.mit.edu/ans7870/6/6.006/s08/lecturenotes/files/t8.shakespeare.txt"
txt <- trimws(readLines(shakespeare_url))

saveRDS(txt, "./inst/extdata/shakespeare.rds")
