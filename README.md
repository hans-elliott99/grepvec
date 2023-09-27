
# grepvec

## Search for a Vector of Patterns in a Vector of Strings
### (Or, finding needles in haystacks)

Use 'grepvec' to search a vector of strings (the haystacks) for matches in a vector
  of regular expressions or sub-strings (your needles). R's native 'grep' functions search for
  a single pattern in a vector of strings. To search for many possible patterns
  across a string or vector of strings, some form of looping is required. 'grepvec' implements this
  in C so that it is much faster than native R solutions.

## Development
This project is in development. I have used it effectively in my own work for keyword-search related tasks, but it lacks testing by other users. If you find it useful please let me know, and let me know how I might improve it to better suit your needs. Thanks!

## Install
Since this package is not on CRAN, you can install in R with `devtools::install_github("hans-elliott99/grepvec)`

## Examples
For now, please see `examples.R`.
