library(testthat)

test_that("grepvec returns correct indices", {
    expect_equal(grepvec(c("a", "b"), c("apple", "banana", "big cherry")),
                 list(1, 1:2, 2))
    expect_equal(grepvec(c("a", "b"), c("apple", "banana", "cherry"),
                         fixed = TRUE),
                 list(1, 1:2, integer(0)))
    expect_equal(grepvec(c("a", "b"), c("apple", "banana", "cherry"),
                         fixed = TRUE, matchrule = "first"),
                 list(1, 1, integer(0)))
    expect_equal(grepvec(c("a", "b"), c("apple", "banana", "cherry"),
                         fixed = TRUE, matchrule = "last"),
                 list(1, 2, integer(0)))
})

test_that("grepvec handles empty vectors", {
    expect_equal(grepvec(c("a", "b"), c()), list())
    expect_equal(grepvec(c(), c("apple", "banana", "cherry")),
                 list(integer(0), integer(0), integer(0)))
})

test_that("grepvec handles empty strings", {
    expect_equal(grepvec(c("a", "b"), c("")), list(integer(0)))
    expect_equal(grepvec(c(""), c("apple", "banana", "cherry")),
                 list(1, 1, 1))
})

test_that("grepvec handles non-matching patterns", {
    expect_equal(grepvec(c("x", "z"), c("apple", "banana", "cherry")),
                 list(integer(0), integer(0), integer(0)))
})


test_that("grepvec returns vectors of the correct length", {
    x <- c("apple", "banana", "cherry", "plum")
    patterns <- c("a", "n", "e")
    g1 <- grepvec(patterns, x, matchrule = "first")
    g2 <- grepvec(patterns, x, matchrule = "last")
    expect_equal(lengths(g1), lengths(g2))
    expect_equal(sort(unique(c(lengths(g1), lengths(g2)))), c(0, 1))
})

test_that("grepvec respects case sensitivity", {
    expect_equal(grepvec(c("A"), c("apple")), list(integer(0)))
    expect_equal(grepvec(c("a"), c("APPLE")), list(integer(0)))
    expect_equal(grepvec(c("A"), c("apple"), ignore_case = TRUE), list(1))
    expect_equal(grepvec(c("A"), c("apple"), ignore_case = TRUE, fixed = TRUE),
                 list(1))
})

test_that("grepvec handles non-character inputs", {
    expect_equal(grepvec(1, "apple"), list(integer(0)))
    expect_equal(grepvec(c("a", "b"), 1), list(integer(0)))
})


test_that("by_hay and by_ndl are working", {
    x <- grepvec(c("a", "b"), c("apple", "banana", "big cherry"), out = "obj")
    expect_equal(lengths(by_hay(x)), c(1, 2, 1))
    expect_equal(lengths(by_ndl(x)), c(2, 2))
})
