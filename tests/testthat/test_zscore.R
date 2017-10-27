library(LNCDR)
context("zscore")
# test with
# cd ../; Rscript testthat.R

# example data
d<-data.frame(a=1:10,b=seq(10,100,by=10),c=rep('a',10))

test_that("zscorecols", {
 z <- zscorecols(d,'c')

 expect_true(all(  z$c == 'a' ) )
 expect_equal(z$a, z$b,tolerance=1e-15 )
 expect_true(z$a[1]    != z$a[2])
 expect_true(mean(z$a) == 0     )
})

