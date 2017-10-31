library(LNCDR)
context("col_ungroup")
# test with
# cd ../; Rscript testthat.R

test_that("ungroup prefix", {
  d    <- data.frame(x=1:3,Contact1Phone=4:6,Contact2Phone=7:9,Contact1Address=2:4,Contact2Address=5:7)
  d.ug <- col_ungroup(d,'Contact[12]','grpvar')
  expect_true(all(names(d.ug)==c('x','Phone','Address','grpvar'))) 
  expect_true(all(unique(sort(d.ug$grpvar))==c('Contact1','Contact2'))) 
  
})
test_that("ungroup suffix", {
  d    <- data.frame(x=1:3,a.measure=4:6,b.measure=7:9,a.m2=2:4,b.m2=5:7)
  d.ug <- col_ungroup(d,'^(a|b)\\.','grpvar')
  expect_true(all(names(d.ug)==c('x','measure','m2','grpvar')) )
  expect_true(all(unique(sort(d.ug$grpvar))==c('a.','b.')) )
  
})
test_that("ungroup uneven", {
  d    <- data.frame(x=1:3,a.measure=4:6,b.measure=7:9,a.m2=2:4)
  d.ug <- col_ungroup(d,'^(a|b)\\.','grpvar')
  expect_true(all(names(d.ug)==c('x','measure','m2','grpvar')) )
  expect_true(all(unique(sort(d.ug$grpvar))==c('a.','b.')) )
  expect_true( all(is.na(d.ug$m2[d.ug$grpvar=='b'])))
  expect_true( !any(is.na(d.ug$m2[d.ug$grpvar=='a'])))
  
})
