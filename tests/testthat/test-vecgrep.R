library(testthat)
set.seed(1614)

gen_word_list <- function(lines, n = 10) {
    li <- as.integer(runif(n, 1, length(lines)))
    words <- paste(lines[li], collapse = " ")
    words <- unique(strsplit(words, " ")[[1]])
    words <- words[words != ""]
    words <- words[as.integer(runif(n, 1, length(words)))]
    words <- gsub("[^[:alnum:] ]", "", words)
    return(words)
}

test_that("vecgrep returns correct indices", {
    expect_equal(vecgrep(c("apple", "banana", "big cherry"), c("a", "b")),
                 list(1, 1:2, 2))
    expect_equal(vecgrep(c("apple", "zzz", "big cherry"), c("a", "b", "c")),
                 list(1, integer(0), 2:3))
    expect_equal(vecgrep(c("apple", "banana", "cherry"), c("a", "b", "d"), 
                         fixed = TRUE, match = "first"),
                 list(1, 1, integer(0)))
    expect_equal(vecgrep(c("apple", "^apple"), "^apple", fixed = TRUE),
                 list(integer(0), 1))
})

test_that("vecgrep handles empty vectors", {
    expect_equal(vecgrep(c("a", "b"), c()), list(integer(0), integer(0)))
    expect_equal(vecgrep(c(), c("apple", "banana", "cherry")),
                 list())
})

test_that("vecgrep handles empty strings", {
    expect_equal(vecgrep(c("a", "b"), c("")), list(1, 1))
    expect_equal(vecgrep(c(""), c("apple", "banana", "cherry")),
                 list(integer(0)))
})

test_that("vecgrep handles non-matching patterns", {
    expect_equal(vecgrep(c("x", "z"), c("apple", "banana", "cherry")),
                 list(integer(0), integer(0)))
})


test_that("vecgrep returns vectors of the correct length", {
    x <- c("apple", "banana", "cherry")
    patterns <- c("a", "n", "e")
    g1 <- vecgrep(x, patterns, match = "first")
    expect_equal(lengths(g1), rep_len(1, length(patterns)))
    g2 <- vecgrep(x, patterns, match = "all")
    expect_equal(lengths(g2), c(2, 2, 1))
})

test_that("vecgrep respects case sensitivity", {
    expect_equal(vecgrep(c("APPLE"), c("a")), list(integer(0)))
    expect_equal(vecgrep(c("apple"), c("A")), list(integer(0)))
    expect_true(all(unlist(vecgrep(letters, LETTERS, ignore_case = TRUE))))
})

test_that("vecgrep warns when fixed=TRUE & ignore_case=TRUE", {
    expect_warning(vecgrep(c("app"), c("a"), fixed = TRUE, ignore_case = TRUE))
})

test_that("vecgrep handles non-character inputs", {
    expect_equal(vecgrep(1, "apple"), list(integer(0)))
    expect_equal(vecgrep(c("a", "b"), 1), list(integer(0), integer(0)))
})


test_that("vecgrep memory management behaves for longer vectors", {
    shakespeare <- readLines(test_path("data", "shakespeare.txt"))[1:200]
    words <- gen_word_list(shakespeare, n = 2000)
    inds <- c(1, 100, 200)
    words[inds] <- "vecgrep"
    expect_no_error(g <- vecgrep(shakespeare, words))
    expect_true(max(unique(unlist(g))) <= length(words))
    expect_true(min(unique(unlist(g))) >= 1)

    expect_equal(sort(unique(unlist(vecgrep("vecgrep", words)))), inds)
})


test_that("vecgrep returns a named list when names=TRUE", {
    x <- c("apple", "banana", "cherry", "plum")
    patterns <- c("a", "n", "e")
    g1 <- vecgrep(x, patterns, use_names = TRUE)
    expect_equal(names(g1), x)

    expect_equal(names(vecgrep(c(), c("a"), use_names = TRUE)), character(0))
    expect_equal(names(vecgrep("", "a", use_names = TRUE)), "")
    expect_equal(names(vecgrep(NA, "a", use_names = TRUE)), NA_character_)
})


test_that("in vecgrep invert = TRUE works", {
    g1 <- vecgrep(letters, LETTERS, invert = TRUE)
    expect_equal(unique(lengths(g1)), 26)
    g2 <- vecgrep(letters, LETTERS, invert = TRUE, ignore_case = TRUE)
    expect_equal(unique(lengths(g2)), 25)

    expect_equal(vecgrep("a", "b", invert = TRUE), list(1))
    expect_equal(vecgrep("a", "b", invert = FALSE), list(integer(0)))
})


test_that("in vecgrep keepdims = TRUE works", {
    x <- c("apple", "banana", "cherry", "plum")
    patterns <- c("a", "n", "e", "z")
    g1 <- vecgrep(x, patterns, keepdim = TRUE)
    expect_equal(length(g1), length(patterns))
    expect_true(all(lengths(g1) == length(x)))
    expect_true(all(unique(unlist(g1)) %in% c(1:3, NA)))
    g2 <- vecgrep(x, patterns, keepdim = TRUE, match = "first")
    expect_equal(lengths(g2), rep_len(1, length(patterns)))
    expect_equal(unlist(g2), c(1, 1, 3, NA))
})


test_that("vecgrep works with non-ASCII characters", {
    x <- c("apple", "banana", "cherry", "ápple")
    patterns <- c("a", "n", "e", "á")
    g1 <- vecgrep(x, patterns)
    expect_equal(g1, list(c(1, 3), 1:2, 3, 3:4))

    lat1 <- readLines(test_path("data", "latin1.txt"),
                      encoding = "latin1", n = 1)
    expect_equal(vecgrep(lat1, "première"), list(1))
    expect_equal(vecgrep(lat1, "première", fixed = TRUE), list(1))
    expect_equal(vecgrep(lat1, "première", use_bytes = TRUE), list(integer(0)))
    expect_equal(
        vecgrep(lat1, iconv("première", to = "latin1"), use_bytes = TRUE),
        list(1)
    )
    expect_equal(
        vecgrep(lat1, iconv("première", to = "latin1"), use_bytes = FALSE),
        list(1)
    )

    # don't specify encoding
    lat1 <- readLines(test_path("data", "latin1.txt"), n = 1)
    expect_warning(g1 <- vecgrep("prem", lat1))
    expect_equal(g1, list(integer(0)))
})



test_that("vecgrepl returns as expected", {
    x <- c("apple", "banana", "cherry", "plum")
    patterns <- c("a", "n", "e", "z")
    g1 <- vecgrepl(x, patterns)
    expect_true(all(lengths(g1) == length(x)))
    expect_true(all(unlist(g1) %in% c(TRUE, FALSE)))

    g2 <- vecgrepl(x, patterns, match = "first")
    expect_true(all(lengths(g2) == 1))
    expect_true(all(unlist(g1) %in% c(TRUE, FALSE)))
})
