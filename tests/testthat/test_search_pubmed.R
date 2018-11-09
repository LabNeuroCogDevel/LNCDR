library(LNCDR)
context("pubmed_search")


test_that("same matching date, nanmax ", {
 d <- pubmed_search("Tervo-Clemmens[Author]", "temp/authsearch")
 # have more than one result
 expect_true(nrow(d) > 4)
 # have expected columns
 expect_true(all(names(d) %in% c("journal", "title", "year",
                                 "abstract", "doi", "authors" )))
 # have one of the dois
 expect_true("10.1016/j.bpsc.2018.05.004" %in% d$doi)
})
