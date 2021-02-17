library(LNCDR)
library(oro.nifti)
context("to_nii")
# test with
# cd ../; Rscript testthat.R

test_that("read write save", {
   d<-data.frame(i=20:30,j=20:30,k=20:30,v=100)
   to_nii('../HarOx-sub-2mm.striatumplusthalamus.nii.gz',d,'test')
   expect_true( file.exists('test.nii.gz') )

   nii <- readNIfTI('test.nii.gz')
   nd <- nii@.Data[nii@.Data>0]

   expect_length(nd, 11 )
   expect_true( all(nd==100) )

   file.remove('test.nii.gz')
  
})

test_that("read write save with charater indexies", {
   d<-data.frame(i=20:30,j=20:30,k=20:30,v=100)
   d$i <- as.character(d$i); d$k <- as.character(d$k); d$j <- as.character(d$j) 
   to_nii('../HarOx-sub-2mm.striatumplusthalamus.nii.gz',d,'test')
   expect_true( file.exists('test.nii.gz') )

   nii <- readNIfTI('test.nii.gz')
   nd <- nii@.Data[nii@.Data>0]

   expect_length(nd, 11 )
   expect_true( all(nd==100) )

   file.remove('test.nii.gz')
  
})

test_that("reset afni min/max", {
   skip_if(system("which 3dNotes 2>/dev/null",intern=T)<=0L)
   d <- data.frame(i=20:30, j=20:30, k=20:30, v=100)
   to_nii("../HarOx-sub-2mm.striatumplusthalamus.nii.gz", d, "test")
   expect_equal(system("3dBrickStat test.nii.gz", intern=T), max(nd))
   file.remove("test.nii.gz")
})
