# grepvec


- [Finding needles in haystacks](#finding-needles-in-haystacks)
- [Install](#install)
  - [Dependencies](#dependencies)
- [Examples](#examples)
- [Similarities and differences from
  `grep`](#similarities-and-differences-from-grep)
  - [PERL](#perl)
  - [Missing values](#missing-values)
  - [UTF-8 everywhere](#utf-8-everywhere)
- [Development](#development)
- [Use Cases](#use-cases)

## Finding needles in haystacks

Use `grepvec` to find needles in haystacks.

That is, search for each pattern in a vector of regular expressions or
fixed strings across each string in another vector.  
R’s native ‘grep’ functions search for a single pattern in a vector of
strings. To search for many possible patterns across a string or vector
of strings, some form of looping is required. `grepvec` implements this
in C so that it is more efficient.

## Install

Since this package is not currently on CRAN, you can install it in R
with `remotes::install_github("hans-elliott99/grepvec")`.

For development you can clone the repo and use `devtools`.

### Dependencies

There are no package dependencies other than the “base” R packages
(`utils` and `base` specifically), so you don’t need to install anything
else.

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

## Similarities and differences from `grep`

The idea is to make the behavior of `grepvec` similair to that of
`base::grep` (with the most obvious difference being that `grepvec`
returns a list).

For example, `grepvec` uses the same regex library
([tre](https://github.com/laurikari/tre)) used by R when you call
`grep(..., perl = FALSE)`, the default case.

### PERL

I may add perl-compatible regular expressions through the PCRE library,
but currently there is no `perl` option in `grepvec`.

### Missing values

Another difference is in the propagation of missing values.  
With `grep`, if the pattern is `NA` the result is a vector, length(x),
of `NA`.  
However, if the hasytack (“x” in `grep`) is `NA`, the NAs are ignored:

``` r
grep(NA, letters)
```

     [1] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
    [26] NA

``` r
grep("a", NA)
```

    integer(0)

``` r
grep("a", c(NA, "apple"))
```

    [1] 2

`grepvec` never returns `NA`:

``` r
grepvec(NA, letters[1:2])
```

    [[1]]
    integer(0)

    [[2]]
    integer(0)

``` r
grepvec(c("a", "b"), c(NA, "apple"))
```

    [[1]]
    integer(0)

    [[2]]
    [1] 1

Since `grepvec` is meant to be used to check for *many* patterns in
*many* strings, if a needle is `NA`, it is treated as a pattern that
could never match any string. Likewise, if a haystack is `NA`, it is
treated as a string where no needles can be found. Instead of returning
`NA`, a vector of length 0 is returned.

Ideally, this makes the results easier to work with. For example, it is
easier to compare the number of matches across haystacks, since `NA`
values would be considered in the length of the vector:

``` r
lengths(grepvec(NA, letters[1:2]))
```

    [1] 0 0

``` r
lengths(grep(NA, letters[1:2]))
```

    [1] 1 1

``` r
# NAs contribute to length 
length(c(NA, NA, 3))
```

    [1] 3

### UTF-8 everywhere

For now (/out of laziness/out of desire for speed), when strings are
compared by `grepvec`, they are first converted to UTF-8 (if needed).  
R has support for different encodings, but this complicates things. See
this statement from the [cpp11 R
package](https://cran.r-project.org/web/packages/cpp11/vignettes/motivations.html#utf-8-everywhere).

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

  - Related: it would be interesting to add an “approximate grepvec”
    (see `?agrep`). This could help with some common headaches in data
    cleaning like merging a standard set of location names onto messy
    data.

- Others?
