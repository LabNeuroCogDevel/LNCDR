library(LNCDR)
context("save1D")
# test with
# cd ../; Rscript testthat.R

# example data
d<-data.frame(onsets=1:12,dur=1:2,block=1:3)
d<-d[sample(1:12),]

test_that("no block column stops", {
 blockcolidx <- grep('block',names(d))
 expect_error(save1D(d[,-blockcolidx],'onsets'),'save1D needs input dataframe to have a column named "block"')
})

test_that("bad onset column", {
 expect_error(save1D(d,'notonsets'),"cannot find notonsets in dataframe")
})

test_that("data is sorted", {
 res <- capture.output(save1D(d,'onsets'))
 nums <- strsplit(res,' ')
 lineissorted <- sapply(nums, function(x){ !is.unsorted(as.numeric(x))}) 
 expect_true(all(lineissorted))
})

# TODO: test middle '*' if remove block 2
# TODO: test end '*' if nblocks=4
# TODO: test amp, dur, and :dur*amp
