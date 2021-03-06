% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/to_nii.R
\name{to_nii}
\alias{to_nii}
\title{to_nii: give a mask and a dataframe (row per voxel: i j k value), write out a nifti file}
\usage{
to_nii(examplenii, d, fileout, valcol = 4, icol = "i", jcol = "j", kcol = "k")
}
\arguments{
\item{examplenii}{an oro.nifti object with the header (orientation, pixdim, etc) representing voxel data}

\item{d}{voxelwise dataframe: row with i j k and voxel value}

\item{valcol}{column in d to use as voxel value}

\item{icol, jcol, kcol}{columns to use for indexing}
}
\description{
to_nii: give a mask and a dataframe (row per voxel: i j k value), write out a nifti file
}
\examples{
\dontrun{
 mask.file <- '/Volumes/Phillips/mMR_PETDA/scripts_BTC/HarOx-sub-2mm.striatumplusthalamus.nii.gz'
 mask <- readNIfTI(mask.file) # 96x114x96: 5370 nonzero
 d<-data.frame(i=1:60,j=1:60,k=1:60,v=100)
 to_nii(mask.file,d,'test')
}
}
