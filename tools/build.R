cpp11::cpp_register()
devtools::load_all()

grepvec_fixed_(haystck = "abc", needles = c("a", "b", "c"),
               matchrule = 0L, ignorecase = FALSE)
grepvec_regex_(needles = c("A", "b", "c"), haystck = "abc",
               matchrule = 0L, ignorecase = FALSE)

grepvec_regex_(haystck = "abc", needles = c("^A", "b$", "c$"),
               matchrule = 0L, ignorecase = FALSE)

if (exists("TEST") && TEST) devtools::test()

# note to self:
# if you get errors about a symbol not existing, make sure to check NAMESPACE
# since cpp_register, load_all don't update it and even devtools::document fails
# to do so, sometimes.