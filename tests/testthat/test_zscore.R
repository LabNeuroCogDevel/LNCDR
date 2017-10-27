library(LNCDR)
context("zscore")
# test with
# cd ../; Rscript testthat.R

# example data
df<-data.frame(
     a=1:10,
     b=1:10*10,
     c=c(rep('x',5),rep('y',5)),
     d=1:10)

test_that("zscorecols", {
 # zscore without column c and d
 z <- zscorecols(df,c('c','d') )

 # we did soemthing
 expect_true(z$a[1]  != z$a[2])

 # we did the right thing
 expect_equal(mean(z$a), 0)
 expect_equal(z$a, z$b,tolerance=1e-15 )
 
 # did nothing to cols c and d 
 expect_true(z$c[1]  == 'x' )
 expect_true(z$c[10] == 'y' )
 expect_equal(z$d[1], 1 )

})


test_that("zscorewithinfactor", {
 
   x <- 1:10
   f <- sample(c(rep('x',5),rep('y',5)))
   z <- zscorewithinfactor(f, x)

   expect_true( length(z) == 10 )

   # does what we expect
   expect_equal( z[f=='x'], zscore(x[f=='x']), tolerance=1e-5 )
   expect_equal( z[f=='y'], zscore(x[f=='y']), tolerance=1e-5 )
 
})
