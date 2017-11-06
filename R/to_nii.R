# Author: Will Foran
# 20171103
require(oro.nifti)

#' to_nii: give a mask and a dataframe (row per voxel: i j k value), write out a nifti file
#' @export
#' @param examplenii an oro.nifti object with the header (orientation, pixdim, etc) representing voxel data
#' @param d voxelwise dataframe: row with i j k and voxel value
#' @param valcol column in d to use as voxel value
#' @param icol,jcol,kcol columns to use for indexing
#' @examples
#' \dontrun{
#'  mask.file <- '/Volumes/Phillips/mMR_PETDA/scripts_BTC/HarOx-sub-2mm.striatumplusthalamus.nii.gz'
#'  mask <- readNIfTI(mask.file) # 96x114x96: 5370 nonzero
#'  d<-data.frame(i=1:60,j=1:60,k=1:60,v=100)
#'  to_nii(mask.file,d,'test')
#' }

to_nii <- function(examplenii,d,fileout,valcol=4,icol='i',jcol='j',kcol='k') {
 # read in data if given a file name (otherwise expect oronifti object)
 if(class(examplenii) == "character" )  {
     examplenii <- readNIfTI(examplenii) 
 }

 # create a zero value matrix to populate with dataframe values
 #md <- examplenii@.Data
 #md[,,] <- 0
 dm <- dim(examplenii@.Data)
 md <- rep(0,prod(dm))
 dim(md) <- dm

 # indexes should all be numeric
 for ( coln in c(icol,jcol,kcol)){ d[,coln] <- as.numeric(d[,coln]) }

 # put the value in the dataframe into the nifti matrix
 for(idx in 1:nrow(d) ){
   md[ d[idx,icol],d[idx,jcol],d[idx,kcol] ] <- d[idx,valcol]
 }

 # save it
 examplenii@.Data <- md
 writeNIfTI(examplenii,fileout)
}

