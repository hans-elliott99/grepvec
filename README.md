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

## Finding needles in haystacks

Use `grepvec` to find needles in haystacks.

That is, search for each pattern in a vector of regular expressions or
fixed strings across each string in another vector.  
R’s native ‘grep’ functions search for a single pattern in a vector of
strings. To search for many possible patterns across a string or vector
of strings, some form of looping is required. `grepvec` implements this
in C.

> Note: `grepvec` was a fun attempt to improve on the speed of existing
> solutions to this problem. It was faster than native R solutions when
> it was a bare-bone implementation (see the “simple_grepvec” branch).
> Adding further developments - like using TRE instead of regex.h and
> supporting different character encodings - were arguably necessary,
> but slowed down the program quite a bit. I’m sure it could be improved
> in many, many ways.

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

Please see `examples/` for more examples.

``` r
devtools::load_all()
```

    ℹ Loading grepvec

``` r
library(grepvec)

# grepvec returns a list with length equivalent to length(needles).
# each element in the list is a vector, which can be length 0 up to length(haystacks).
# The elements of each vector are the indices to the strings in haystacks that
# contained the pattern for the given needle.
grepvec(needles = c("some", "other", "string"),
        haystacks = c("some string 1", "another string"),
        fixed = TRUE)
```

    [[1]]
    [1] 1

    [[2]]
    [1] 2

    [[3]]
    [1] 1 2

``` r
grepvec(c("^h", ".ell.", "ello$"), c("hello", "jelly"))
```

    [[1]]
    [1] 1

    [[2]]
    [1] 1 2

    [[3]]
    [1] 1

``` r
grepvec(c("^h", ".ell.", "ello$"), c("hello", "JELLY"),
        value = TRUE, ignore_case = TRUE)
```

    [[1]]
    [1] "hello"

    [[2]]
    [1] "hello" "JELLY"

    [[3]]
    [1] "hello"

``` r
# vecgrep returns a list length(haystacks), returning the patterns (needles)
# that matched to each haystack string. It's like a transposed version of grepvec
vecgrep(c("hello", "jelly"), c("^h", ".ell.", "ello$"),
        value = TRUE)
```

    [[1]]
    [1] "^h"    ".ell." "ello$"

    [[2]]
    [1] ".ell."

``` r
vecgrep(paste(letters, collapse = ""), letters[1:3], value = TRUE)
```

    [[1]]
    [1] "a" "b" "c"

``` r
## return only the first match, instead of all
vecgrep(paste(letters, collapse = ""), letters[1:3], value = TRUE, match = "first")
```

    [[1]]
    [1] "a"

``` r
# Some other utilities

strings <- c("the quick brown fox", "jumps over", "the lazy dog")

# check if strings contain any of the patterns in the pattern vector 
grepl_any(c("fox", "dog"), strings)
```

    [1]  TRUE FALSE  TRUE

``` r
# or, equivalent:
c("fox", "dog") %grepin% strings
```

    [1]  TRUE FALSE  TRUE

``` r
# get the first match in the pattern vector for each string in x
grep_first(c("quick", "fox", "lazy", "dog"), strings, value = TRUE)
```

    [1] "quick" NA      "lazy" 

``` r
# count the number of patterns that occur in each string
grep_count(c("o", "u"), strings)
```

    [1] 2 2 1

## Similarities and differences from `grep`

The idea is to make the behavior of `grepvec` similair to that of
`base::grep` (with the most obvious difference being that `grepvec`
returns a list).

For example, `grepvec` uses the same regex library
([tre](https://github.com/laurikari/tre)) used by R when you call
`grep(..., perl = FALSE)`, the default case.

### PERL

We could add perl-compatible regular expressions through the [PCRE
library](https://www.pcre.org/original/doc/html/index.html), but
currently there is no `perl` option in `grepvec`.

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
grepvec(NA, letters)
```

    [[1]]
    integer(0)

``` r
grepvec("a", NA)
```

    [[1]]
    integer(0)

``` r
grepvec("a", c(NA, "apple"))
```

    [[1]]
    [1] 2

``` r
vecgrep(c(NA, "apple"), "a")
```

    [[1]]
    integer(0)

    [[2]]
    [1] 1

Since `grepvec` is meant to be used to check for multiple patterns in
multiple strings, if a needle is `NA`, it is treated as a pattern that
could never match any string. Likewise, if a haystack is `NA`, it is
treated as a string where no needles can be found. Instead of returning
`NA`, a vector of length 0 is returned.

Ideally, this makes the results easier to work with. For example, it is
easier to compare the number of matches across haystacks, since `NA`
values would be considered in the length of the vector:

``` r
length(grepvec(NA, letters)[[1]])
```

    [1] 0

``` r
length(grep(NA, letters))
```

    [1] 26

``` r
# NAs contribute to length 
length(c(NA, NA, 3))
```

    [1] 3

### UTF-8 everywhere

~~For now (/out of laziness/out of desire for speed), when strings are
compared by `grepvec`, they are first converted to UTF-8 (if needed).  
R has support for different encodings, but this complicates things. See
the statement from the [cpp11 R
package](https://cran.r-project.org/web/packages/cpp11/vignettes/motivations.html).~~

I’m not sure it was worth the effort, but I went ahead and implemented
support for multiple encodings. After studying the `base::grep` code, I
more or less copied its strategy for character encodings exactly.

## Development

I have used `grepvec` in my own work but it lacks testing by other
users. If you find it useful please let me know, or let me know how I
might improve it to better suit your needs.

Feel free to contribute if you wish.
