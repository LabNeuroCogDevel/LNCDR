% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/afni_spectrum.R
\name{nii_range}
\alias{nii_range}
\title{NIFTI range using afni's 3dBrickStat}
\usage{
nii_range(nii)
}
\arguments{
\item{nifti}{file}
}
\description{
NIFTI range using afni's 3dBrickStat
}
\examples{
 nii_range('myfile.nii.gz')
 nii_range('myfile.nii.gz[0]')
}
