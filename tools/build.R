


cpp11::cpp_register()
devtools::load_all()

grepvec_fixed_(haystck = "abc", needles = c("a", "b", "c"),
               matchrule = 0, ignorecase = FALSE)
grepvec_regex_(needles = c("A", "b", "c"), haystck = "abc",
               matchrule = 0, ignorecase = FALSE)

grepvec_regex_(haystck = "abc", needles = c("^A", "b$", "c$"),
               matchrule = 0, ignorecase = FALSE)

if (exists("TEST") && TEST) devtools::test()
