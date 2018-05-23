#
# Author: Will Foran
# 
# 20180523 WF 
require(oro.nifti)

#' vox_cor: voxelwise correlation for timeseries between two masks
#' @param tsnii file name to 4d timeseries  nifti
#' @param seed   boolean matrix (seed_mask@.Data==1) or timeseries (1d numeric matrix)
#' @param trgmsk boolean matrix (trg_mask@.Data==1)
#' @export
#' @examples 
#'    # usage
#'    \dontrun{
#'    seed <- read_mask("striatum_mask.nii.gz")
#'    target <- read_mask("striatum_mask.nii.gz")
#'    target <- target & ! seed
#'    allcors <- vox_cor("subj_ts.nii.gz",seed,target)
#'    # passing seed as a vector
#'    allcors_mean <- vox_cor("subj_ts.nii.gz",apply(seed,2,mean),target)
#'    }
vox_cor <- function(fname, seed, trgmsk) {
    sub_4d <- oro.nifti::readNIfTI(as.character(fname))@.Data
    ntr <- dim(sub_4d)[4]
    trg_ts <- t(matrix(sub_4d[trgmsk], ncol=ntr))
    # seed is a single timeseries or a boolean matrix
    if (is.vector(seed))
      stm_ts <- seed
    else
      stm_ts <- t(matrix(sub_4d[seed], ncol=ntr))
    return(cor(stm_ts, trg_ts))
}

#' read_mask: read a nifti file as a mask, return "logi" (boolean) matrix
#' @param fname  file path to nifti file
#' @export
#' @examples 
#'    \dontrun{
#'     seed <- read_mask("striatum_mask.nii.gz")
#'    }
read_mask <- function(fname) oro.nifti::readNIfTI(fname)@.Data == 1
