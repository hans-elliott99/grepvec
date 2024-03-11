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

test_that("grepvec returns correct indices", {
    expect_equal(grepvec(c("a", "b"), c("apple", "banana", "big cherry")),
                 list(1:2, 2:3))
    expect_equal(grepvec(c("a", "b"), c("apple", "banana", "cherry"),
                         fixed = TRUE),
                 list(1:2, 2))
    expect_equal(grepvec(c("a", "b", "d"), c("apple", "banana", "cherry"),
                         fixed = TRUE, match = "first"),
                 list(1, 2, integer(0)))
})

test_that("grepvec handles empty vectors", {
    expect_equal(grepvec(c("a", "b"), c()), list(integer(0), integer(0)))
    expect_equal(grepvec(c(), c("apple", "banana", "cherry")),
                 list())
})

test_that("grepvec handles empty strings", {
    expect_equal(grepvec(c("a", "b"), c("")), list(integer(0), integer(0)))
    expect_equal(grepvec(c(""), c("apple", "banana", "cherry")),
                 list(1:3))
})

test_that("grepvec handles non-matching patterns", {
    expect_equal(grepvec(c("x", "z"), c("apple", "banana", "cherry")),
                 list(integer(0), integer(0)))
})


test_that("grepvec returns vectors of the correct length", {
    x <- c("apple", "banana", "cherry", "plum")
    patterns <- c("a", "n", "e")
    g1 <- grepvec(patterns, x, match = "first")
    expect_equal(lengths(g1), rep_len(1, length(patterns)))
})

test_that("grepvec respects case sensitivity", {
    expect_equal(grepvec(c("A"), c("apple")), list(integer(0)))
    expect_equal(grepvec(c("a"), c("APPLE")), list(integer(0)))
    expect_true(all(unlist(grepvec(letters, LETTERS, ignore_case = TRUE))))
})

test_that("grepvec warns when fixed=TRUE & ignore_case=TRUE", {
    expect_warning(grepvec(c("a"), c("app"), fixed = TRUE, ignore_case = TRUE))
})

test_that("grepvec handles non-character inputs", {
    expect_equal(grepvec(1, "apple"), list(integer(0)))
    expect_equal(grepvec(c("a", "b"), 1), list(integer(0), integer(0)))
})


test_that("grepvec memory management behaves for longer vectors", {
    shakespeare <- readLines(test_path("data", "shakespeare.txt"))
    words <- gen_word_list(shakespeare, n = 2000)
    expect_no_error(grepvec(words, shakespeare[1:200]))
})


test_that("grepvec returns a named list when names=TRUE", {
    x <- c("apple", "banana", "cherry", "plum")
    patterns <- c("a", "n", "e")
    g1 <- grepvec(patterns, x, use_names = TRUE)
    expect_equal(names(g1), patterns)

    expect_equal(names(grepvec(c(), c("a"), use_names = TRUE)), character(0))
    expect_equal(names(grepvec("", "a", use_names = TRUE)), "")
    expect_equal(names(grepvec(NA, "a", use_names = TRUE)), NA_character_)
})


test_that("in grepvec invert = TRUE works", {
    g1 <- grepvec(letters, LETTERS, invert = TRUE)
    expect_equal(unique(lengths(g1)), 26)
    g2 <- grepvec(letters, LETTERS, invert = TRUE, ignore_case = TRUE)
    expect_equal(unique(lengths(g2)), 25)
})


test_that("in grepvec keepdims = TRUE works", {
    x <- c("apple", "banana", "cherry", "plum")
    patterns <- c("a", "n", "e", "z")
    g1 <- grepvec(patterns, x, keepdim = TRUE)
    expect_equal(length(g1), length(patterns))
    expect_true(all(lengths(g1) == length(x)))
    expect_true(all(unique(unlist(g1)) %in% c(1:3, NA)))
    g2 <- grepvec(patterns, x, keepdim = TRUE, match = "first")
    expect_equal(lengths(g2), rep_len(1, length(patterns)))
    expect_equal(unlist(g2), c(1, 2, 1, NA))
})


test_that("grepvec works with non-ASCII characters", {
    x <- c("apple", "banana", "cherry", "ápple")
    patterns <- c("a", "n", "e", "á")
    g1 <- grepvec(patterns, x)
    expect_equal(g1, list(1:2, 2, c(1, 3:4), 4))

    lat1 <- readLines(test_path("data", "latin1.txt"),
                      encoding = "latin1", n = 1)
    expect_equal(grepvec("première", lat1), list(1))
    expect_equal(grepvec("première", lat1, fixed = TRUE), list(1))
    expect_equal(grepvec("première", lat1, use_bytes = TRUE), list(integer(0)))
    expect_equal(
        grepvec(iconv("première", to = "latin1"), lat1, use_bytes = TRUE),
        list(1)
    )
    expect_equal(
        grepvec(iconv("première", to = "latin1"), lat1, use_bytes = FALSE),
        list(1)
    )

    lat1 <- readLines(test_path("data", "latin1.txt"), n = 1)
    expect_warning(g1 <- grepvec("prem", lat1))
    expect_equal(g1, list(integer(0)))
})

test_that("greplvec returns as expected", {
    x <- c("apple", "banana", "cherry", "plum")
    patterns <- c("a", "n", "e", "z")
    g1 <- greplvec(patterns, x)
    expect_true(all(lengths(g1) == length(x)))
    expect_true(all(unlist(g1) %in% c(TRUE, FALSE)))

    g2 <- greplvec(patterns, x, match = "first")
    expect_true(all(lengths(g2) == 1))
    expect_true(all(unlist(g1) %in% c(TRUE, FALSE)))
})