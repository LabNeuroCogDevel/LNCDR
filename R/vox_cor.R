#
# Author: Will Foran
# 
# 20180523 WF 
require(oro.nifti)

#' read_mask: read a nifti file as a mask, return "logi" (boolean) array
#' @param fname  file path to nifti file
#' @export
#' @examples 
#'    \dontrun{
#'     seed <- read_mask("striatum_mask.nii.gz")
#'     str(seed) # logi [1:84, 1:100, 1:84]
#'    }
read_mask <- function(fname) oro.nifti::readNIfTI(fname)@.Data == 1

#' mask_nii:  mask a nii by a mask, returns num matrix w/ rows of time, columns of i_j_k voxels
#' @param fname_or_oro  a nifti filename or oro.nifti object
#' @param mask   a "logi" booleen matrix (likely from \code{\link{read_mask}})
#' @export
#' @examples 
#'    \dontrun{
#'     mask <- read_mask("striatum_mask.nii.gz") # 1827 vx in mask
#'     data <- mask_nii("subj_ts.nii.gz",mask)  # 320 trs
#'     # data:     num [1:320, 1:1827] 
#'     # colnames: chr [1:1827] "47_61_30" ...
#'    }
mask_nii <- function(fname_or_oro, mask) {
   if (class(fname_or_oro) == "character")
     d <- oro.nifti::readNIfTI(fname_or_oro)@.Data
   else
     d <- fname_or_oro

   if (length(dim(d)) < 4)
      ntr <- 1
   else
      ntr <- dim(d)[4]
   m <- t(matrix(d[mask], ncol=ntr))
   colnames(m) <- apply(which(mask, arr.ind=T), 1, paste, collapse="_")
   return(m)
}


#' vox_cor: voxelwise correlation for timeseries between two masks
#' @param tsnii  4d timeseries as file name, oronifti object, or 4d timeseries matrix
#' @param seed   1d numeric matrix or "logi" boolean matrix (seed_mask@.Data==1, see \code{\link{read_mask}}) 
#' @param trgmsk "logi" boolean matrix (trg_mask@.Data==1, see \code{\link{read_mask}})
#' @export
#' @examples 
#'    # usage
#'    \dontrun{
#'    seed <- read_mask("striatum_mask.nii.gz")
#'    target <- read_mask("gm_mask.nii.gz")
#'    target <- target & ! seed
#'    allcors <- vox_cor("subj_ts.nii.gz",seed,target)
#'    # passing seed as a vector
#'    allcors_mean <- vox_cor("subj_ts.nii.gz",apply(seed,2,mean),target)
#'    }
vox_cor <- function(tsnii, seed, trgmsk) {
    # what do do with ts nii
    if (class(tsnii) == "character")
       ts <- oro.nifti::readNIfTI(as.character(tsnii))@.Data
    else if (class(tsnii) == "nifti")
       ts <- tsnii@.Data
    else
       ts <- tsnii

    # read in target timeseries
    trg_ts <- mask_nii(ts, trgmsk)

    # seed is a single timeseries or a boolean matrix
    if (is.vector(seed))
      stm_ts <- seed
    else
      stm_ts <- mask_nii(ts, seed)

    # do the correlation
    return(cor(stm_ts, trg_ts))
}
