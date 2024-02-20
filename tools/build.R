
devtools::document()

.Call("grepvec_regex_", needles = c("A", "b", "c"), haystck = "abc",
      matchrule = 0L, ignorecase = TRUE)

.Call("grepvec_fixed_", needles = c("A", "b", "c"), haystck = "abc",
      matchrule = 0L, ignorecase = FALSE)
