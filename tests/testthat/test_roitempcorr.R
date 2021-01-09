library(LNCDR)
context("roicormat_wide")
# test with
# cd ../; Rscript testthat.R

test_that("roicormat_wide", {
  m <- matrix(1:4, 2, 2) # 2x2 matrix 
  x <- roicormat_wide(m) # expect 

  # only upper tri value in 2x2 is '3'
  expect_equal(x[1,1], c(3))
  expect_equal(names(x), c("1_2")) # 1st row, 2nd column
})
