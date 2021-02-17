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

to_nii <- function(examplenii, d, fileout,
                   valcol=4, icol="i", jcol="j", kcol="k") {

 # track audit trail defaults -- reset at end of function
 def <- options("niftiAuditTrail")

 # get note before we do too much with the input variables
 note <-  paste0("to_nii(", substitute(examplenii),
                 ",", substitute(d), ",...)")

 # read in data if given a file name (otherwise expect oronifti object)
 if (class(examplenii) == "character")  {
     examplenii <- readNIfTI(examplenii)
 }

 # create a zero value matrix to populate with dataframe values
 dm <- dim(examplenii@.Data)
 md <- rep(0, prod(dm))
 dim(md) <- dm

 # indexes should all be numeric
 for (coln in c(icol, jcol, kcol)) d[, coln] <- as.numeric(d[, coln])

 # put the value in the dataframe into the nifti matrix
 for (idx in 1:nrow(d)) {
   md[d[idx, icol], d[idx, jcol], d[idx, kcol]] <- d[idx, valcol]
 }

 # save it
 examplenii@.Data <- md
 # reset bounds
 # https://nifti.nimh.nih.gov/pub/dist/src/niftilib/nifti1.h
 #  The cal_min and cal_max fields (if nonzero) are used for mapping (possibly
 #  scaled) dataset values to display colors
 examplenii@cal_max <- max(md, na.rm=T)
 examplenii@cal_min <- min(md, na.rm=T)
 # add note/trial (specific to  dcemri). see out@trail
 out <- add_trail(examplenii, note)
 # clear any previous afni info -- max/min values and notes
 out@extensions <- list()
 writeNIfTI(out, fileout)
 # reset options. default to TRUE causes slow read issues?
 options(def)
 # calling 3dNotes also build back afni's stored min/max (but correct now)
 # see: 3dBrickStat vs 3dBrickStat -slow
 add_note(paste0(fileout, ".nii.gz"), paste0("R: ", note))
}

add_note <- function(fname, note) {
 if (grepl("'", note))
    stop("note cannot contain single quote: ", note)

 if (length(system("which 3dNotes", intern=T)) == 0L){
    warning("3dNotes is not installed!")
    return()
 }

 system(sprintf("3dNotes -h '%s' '%s'", note, fname))
}

add_trail <- function(nim, note) {
 # https://cran.r-project.org/web/packages/oro.nifti/oro.nifti.pdf
 # if we dont have an niftiauditTrial object:
 # probably haven't turned on the feature
 # enable audit and recreate class to get 'trail' slot
 if (!is(nim, "niftiAuditTrail")) {
    options("niftiAuditTrail"=TRUE)
    enableAuditTrail()
    nim <- as(nim, "niftiAuditTrail")
 }
 slot(nim, "trail") <-
    niftiAuditTrailEvent(niftiAuditTrailCreated(), "processing", note)
 return(nim)
}
