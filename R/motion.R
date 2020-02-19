
#' extract FD from fd file
#' @param f - 'fd.1D' file 
#' @export
motion_readfd <- function(f) read.table(f,header=F)$V1

#' extract motion paramiters ordered like mcflirt
#' @param f - 'motion.par' file 
#' @export
motion_readpar <- function(f) {
   d <- read.table(f, header=F) 
   names(d) <- c('Rx','Ry','Rz','Tx','Ty','Tz')
   return(d)
}

#' number fd > treshold (def .1mm)
#' @param x - vector or fd file
#' @param thres - count values above this 
#' @export
motion_nfdabove <- function(x, thres=.1){
   if(is.character(x)) x <- motion_readfd(x)
   length(which(x>thres))
}
#' mean Euler Angle
#' see http://dx.doi.org/10.1016/j.neuroimage.2011.07.044
#' @param x - motion dataframe w/first 3 columsn are rots or motion.par file
motion_eangle <- function(d) {
   if(is.character(d)) d <- motion_readpar(x)
   # http://dx.doi.org/10.1016/j.neuroimage.2011.07.044
   # first 3 are trans, next 3 are rot
   names(d) <- c('phi','theta','psi')
   mean(with(d, acos((cos(phi)*cos(theta) +
         cos(phi)*cos(psi) +
         cos(theta)*cos(psi) +
         sin(phi)*sin(psi)*sin(theta)-1)/2)),
        na.rm=T)
}
#' root mean square
#' @param x - motion dataframe w/first 3 columsn are rots or motion.par file
motion_rms <- function(d) {
   if(is.character(d)) d <- motion_readpar(x)
   mean(sqrt(
     diff(d$Tx)^2 +
     diff(d$Ty)^2 +
     diff(d$Tz)^2),na.rm=T)
}
