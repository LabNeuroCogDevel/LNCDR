library(LNCDR)
context("pubmed_search")

# run this test twice -- once to download, another to reuse
btc <- function(){
    d <- pubmed_search("Tervo-Clemmens[Author]", "temp/authsearch")
    # have more than one result
    expect_true(nrow(d) > 4)
    # have expected columns
    expect_true(all(names(d) %in% c("journal", "title", "year",
                                    "abstract", "doi", "authors" )))
    # have one of the dois
    expect_true("10.1016/j.bpsc.2018.05.004" %in% d$doi)
}

test_that("download results", {
    skip("PUBMED BROKE: FIX ME") # TODO FIX
    expect_true(length(Sys.glob("temp/authsearch*xml")) == 0)
    btc()
})
test_that("reuse results", {
    skip("PUBMED BROKE: FIX ME") # TODO FIX
    expect_true(length(Sys.glob("temp/authsearch*xml")) > 0)
    btc()
})

# remove temporary files when we finish
teardown({
    lapply(Sys.glob("temp/authsearch*xml"),unlink)
    unlink("temp")
})
