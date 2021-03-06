% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/afni_spectrum.R
\name{afni.color_at_value}
\alias{afni.color_at_value}
\title{the hex color value of any point on the spectrum read from afni.spectrum}
\usage{
afni.color_at_value(colorval, val)
}
\arguments{
\item{colorval}{the list output of afni.spectrum}

\item{val}{the value for which a color (hex value) position on the spectrum is desired}
}
\description{
the hex color value of any point on the spectrum read from afni.spectrum
}
\examples{
 colorval <- afni.spectrum(-5:5)
 afni.color_at_value(colorval,6) # out of range, so highest value 
 afni.color_at_value(colorval,0) # middle value is 
}
