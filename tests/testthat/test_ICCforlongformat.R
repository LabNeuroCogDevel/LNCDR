library(LNCDR)
context("ICCforlongfmt")
# test with
# cd ../; Rscript testthat.R

test_that("no error", {
  d <- data.frame(id=rep(1:6, 3), visit=rep(1:3, each=6),
                  a=runif(18, 0, 10),
                  b=runif(18, 0, 10))
  d_ICCs <- ICCforlongformat(d, c("a", "b"), "id", "visit")
 expect_equal(nrow(d_ICCs), 2)
})
