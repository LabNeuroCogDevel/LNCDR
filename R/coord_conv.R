# Author: Will Foran
# 20180109
#require(oro.nifti)

#' ijk.afni2oro: given afni gui voxel dims, get MNI (LPI) dims for oro.nifiti::readNifTI's @.Data matrix
#' @export
#' @param ijk: i,j,k vector read from afni gui
#' @param dm:  dim(x@.Data)
#' @examples
#' \dontrun{
#'  x <- oro.nifti::readNIfTI('betas.nii.gz')
#'  idx <- ijk.afni2oro(c(25,20,30), dim(x) )
#' }
ijk.afni2oro<-function(ijk,dm) {
    t(abs(c(-dm[1],1,1) + ijk ))
}

#' ijk.oro2afni: given index of oro nifti @.Data matrix, get index to give to AFNI's "jump to ijk"
#' @export
#' @param ijk: i,j,k index of oro nifti matrix
#' @param dm:  dim of oro nifti matrix [dim(x@.Data)]
#' @examples
#' \dontrun{
#'  x <- oro.nifti::readNIfTI('betas.nii.gz')
#'  dm <- dim(x)
#'  mx <- arrayInd(which.max(d),dm)
#'  ijk.oro2afni(mx[1:3], dm )
#' }
ijk.oro2afni<-function(ijk,dm) {
   abs(ijk - c(dm[1],1,1)) 
}

