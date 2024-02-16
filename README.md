# grepvec


- [Finding needles in haystacks](#finding-needles-in-haystacks)
- [Install](#install)
  - [Dependencies](#dependencies)
- [Examples](#examples)
- [Development](#development)
- [Use Cases](#use-cases)

## Finding needles in haystacks

Use `grepvec` to find needles in haystacks.

That is, search for each item in a vector of patterns (either regular
expressions or fixed strings) across each item in a vector of strings.  
R’s native ‘grep’ functions search for a single pattern in a vector of
strings. To search for many possible patterns across a string or vector
of strings, some form of looping is required. `grepvec` implements this
in C so that it is more efficient than native R solutions.

## Install

Since this package is not currently on CRAN, you can install it in R
with `remotes::install_github("hans-elliott99/grepvec")`.

For development you can, clone the repo and use `devtools::load_all` or
`devtools::install`.

### Dependencies

You have nothing else to install…

I hope to keep `grepvec` lightweight, and there are no package
dependencies other than the “base” R packages (`utils` and `base`
specifically), so you don’t need to install anything else.

For development, `testthat` is needed for unit testing, and I use
`devtools` (and its dependencies) as well.

## Examples

Please see `examples/grepvec.md` for more examples.

``` r
library(grepvec)

grepvec(needles = c("some", "other", "string"),
        haystacks = c("some string 1", "another string"))
```

    [[1]]
    [1] 1 3

    [[2]]
    [1] 2 3

``` r
grepvec(c("^h", ".ell.", "ello$"), c("hello", "jelly"))
```

    [[1]]
    [1] 1 2 3

    [[2]]
    [1] 2

``` r
grepvec(c("^h", ".ell.", "ello$"), c("hello", "jelly"),
        value = TRUE)
```

    [[1]]
    [1] "^h"    ".ell." "ello$"

    [[2]]
    [1] ".ell."

``` r
grepvec(c("one", "possib", "many"),
        c("Some text which might possibly contain one of many keywords",
          "Another string, without many words",
          "A third impossibly boring string",
          "I'm done thinking of strings."),
        out = "haystacks",
        value = TRUE)
```

    [[1]]
    [1] "Some text which might possibly contain one of many keywords"
    [2] "I'm done thinking of strings."                              

    [[2]]
    [1] "Some text which might possibly contain one of many keywords"
    [2] "A third impossibly boring string"                           

    [[3]]
    [1] "Some text which might possibly contain one of many keywords"
    [2] "Another string, without many words"                         

``` r
grepvec(letters, paste(letters, collapse = ""), value = TRUE)
```

    [[1]]
     [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
    [20] "t" "u" "v" "w" "x" "y" "z"

``` r
grepvec(letters, paste(letters, collapse = ""), matchrule = "first", value = TRUE)
```

    [[1]]
    [1] "a"

``` r
grepvec(letters, paste(letters, collapse = ""), matchrule = "last", value = TRUE)
```

    [[1]]
    [1] "z"

``` r
x <- grepvec(letters[1:6], c("abc", "123"), out = "object")
setNames(grepvec::by_hay(x, value = TRUE), x$haystacks)
```

    $abc
    [1] "a" "b" "c"

    $`123`
    character(0)

``` r
setNames(grepvec::by_ndl(x, value = TRUE), x$needles)
```

    $a
    [1] "abc"

    $b
    [1] "abc"

    $c
    [1] "abc"

    $d
    character(0)

    $e
    character(0)

    $f
    character(0)

## Development

I have used `grepvec` in my own work but it lacks testing by other
users. If you find it useful please let me know, or let me know how I
might improve it to better suit your needs.

Feel free to contribute by creating issues or submitting pull requests.

Thanks!

## Use Cases

The `grepvec` function provides a convenient way to search a vector of
strings (haystacks) for multiple patterns (needles) at once, which is
not directly supported by base R functions like grep, or by similair
`stringr` functions.

While it’s possible to combine multiple patterns into a single regular
expression using the “\|” operator, this can be difficult to manage for
a large number of patterns. Further, it doesn’t easily allow you to
identify which specific patterns were matched, or how many.

Some potential use cases:

- Text Analysis: If you’re analyzing a corpus of text (like a collection
  of documents, tweets, or web pages), you might want to find
  occurrences of a list of specific key words or phrases. `grepvec` can
  be used to efficiently perform this task.

- Data Cleaning: When cleaning data, you often need to find and replace
  certain patterns. If you have a list of known errors or
  inconsistencies (like misspellings or alternative representations of
  the same value), you can use `grepvec` to identify these in your data.

- Others?
