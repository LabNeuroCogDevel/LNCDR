library(LNCDR)
context("hera")

test_that("hera concat", {
 if(!dir.exists("/Volumes/Hera")) skip("no hera")

 scratch <- "/Volumes/Hera/scratch/"
 expect_equal(hera("/Volumes/Hera/scratch/"), scratch)
 expect_equal(hera("H:/scratch/"), scratch)
 expect_equal(hera("scratch/"), scratch)
})
