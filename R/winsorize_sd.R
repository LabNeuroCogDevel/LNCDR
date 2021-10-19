# Author: Orma Ravindranath 
# Packager: Will Foran

#' winsozrize_sd - set above/below devation range to range max/min
#'
#' dev depending on which type of deviation should be used to winsorize
#' @param x vector to be winsorized
#' @param devno number of deviations to winsorize
#' @param dev "sd" (standard deviation) or "mad" (median absolute deviation)
#' @examples
#' w <- winsorize_sd(c(1:10), devno=1, dev="sd")
#' @export
winsorize_sd <- function(x, devno=3, dev="sd") {
  if (dev=="sd") {
    devrange <- sd(x, na.rm=TRUE) * devno
    mid <- mean(x, na.rm=TRUE)
  } else if (dev=="mad") {
    devrange <- stats::mad(x, na.rm=TRUE) * devno
    mid <- median(x, na.rm=TRUE)
  } else {
     stop("dev only supports 'sd' or 'mad'")
  }
  topval <- mid + devrange
  bottomval <- mid - devrange
  x[x > topval] <- topval
  x[x < bottomval] <- bottomval
  return(x)
}
