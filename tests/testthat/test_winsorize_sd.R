library(LNCDR)
context("winsorize")

test_that("winsorize sd", {
 vec <- 1:10
 w <- winsorize_sd(vec, 1, "sd")
 # tails replaced by sd+mean bounds
 expect_true(length(vec) > length(unique(w)))
 # but mean stays the same
 expect_equal(mean(vec), mean(w))
})

