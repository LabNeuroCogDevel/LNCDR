% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/growthrate_gam.R
\name{gam_growthrate_plot_combine}
\alias{gam_growthrate_plot_combine}
\title{combine age plot and tile slop heatmap into one figure (w/grob and grid)}
\usage{
gam_growthrate_plot_combine(ageplot_luna, tile_luna, PDFout = NA)
}
\arguments{
\item{ageplot_luna}{ggplot plot of subject coef by age (top part of figure)}

\item{tile_luna}{tile heatmap of slope  (bottom part of figure)}

\item{PDFout}{PDF name to save output into, NA no saved, NULL not plotted}
}
\description{
save two figures (only use if you need to mess with titles)
}
\examples{
 data <- data.frame(age=1:100,fd_mean=1:100,subj=as.factor(letters[1:25]), conn_ahpc_vmpfc=randu[1:100,1])
 mod<-mgcv::gam(conn_ahpc_vmpfc~s(age)+s(fd_mean)+s(subj, bs="re"), data=data)
 ci<-LNCDR::gam_growthrate(mod, 'age', n = 10000, qnt = c(0.025, 0.975), idvar='subj')
 plist <- gam_growthrate_plot(data, mod, ci, 'age', idvar='subj')
 plist$tile <- plist$tile + xlab('AGE')
 g <- gam_growthrate_plot_combine(plist$ageplot, plist$tile, 'gammod.pdf')
}
