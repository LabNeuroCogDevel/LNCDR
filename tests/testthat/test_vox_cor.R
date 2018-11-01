library(LNCDR)
library(oro.nifti)
context("vox_cor")
# test with
# cd ../; Rscript testthat.R

test_that("mask_nii interface", {
   fn <- "../HarOx-sub-2mm.striatumplusthalamus.nii.gz"
   ni <- oro.nifti::readNIfTI(fn)
   m <- read_mask(fn)
   fromstr <- mask_nii(fn, m)
   fromoro <- mask_nii(ni, m)
   expect_equal(length(which(ni@.Data==1)),length(fromstr),label="mask length")  
   expect_true(all(fromstr==1),label="mask of mask is all ones")  
   expect_true(all(fromstr==fromoro),label="can read string or nifti")
})
test_that("mask_nii ts", {
   ts <- oro.nifti::readNIfTI("../mni2009c_exmple_ts.nii.gz")
   m <- read_mask("../HarOx-sub-2.3mm.striatumplusthalamus.nii.gz")
   mat <- mask_nii(ts, m)
   expect_equal(label="time dim",ts@dim[4],dim(mat)[1])
   expect_equal(label="mask dim",length(which(m)),dim(mat)[2])
})

test_that("mask_nii", {
   ts <- oro.nifti::readNIfTI("../mni2009c_exmple_ts.nii.gz")
   m <- read_mask("../HarOx-sub-2.3mm.striatumplusthalamus.nii.gz")
   ts_masked <- mask_nii(ts, m)
   ijk <- which(m, arr.ind=T)
   # very first value
   expect_equal(label="first val, hardcode",ts[ijk[1, 1], ijk[1, 2], ijk[1, 3], 1], unname(ts_masked[1, 1]))
   # first nonzero element
   i1st <- Position(function(x) !is.na(x) && x !=0, ts_masked[1,])
   firstval <- ts[ijk[i1st, 1], ijk[i1st, 2], ijk[i1st, 3], 1]
   ijkname <- paste(ijk[i1st, 1], ijk[i1st, 2], ijk[i1st, 3], sep="_")
   # all as expected
   expect_equal(label="first val, pos",firstval, unname(ts_masked[i1st, 1]))
   expect_equal(label="all values",dimnames(ts_masked)[[2]][i1st], ijkname )
   # all second tr equal
   ijk2 <- rbind( ijk[, 1], ijk[, 2], ijk[, 3] )
   ts2 <- apply(ijk2,MARGIN=2,function(x) ts[x[1],x[2],x[3],2])
   expect_equal(label="second tr",unname(ts_masked[2,]), ts2)
})
test_that("vox_cor", {
   ts <- oro.nifti::readNIfTI("../mni2009c_exmple_ts.nii.gz")
   m <- read_mask("../HarOx-sub-2.3mm.striatumplusthalamus.nii.gz")
   ts_masked <- mask_nii(ts, m)
})
