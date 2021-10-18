#' winsozrize_sd - set above/below devation range to range max/min
#' @param x vector to be winsorized
#' @param devno number of deviations to winsorize
#' @param dev "sd" (standard deviation) or "mad" (median absolute deviation)
#' @example
#' w <- winsorize_sd(c(1:10), 1, "sd")
#' @export
#' dev depending on which type of deviation should be used to winsorize
winsorize_sd <- function(x, devno, dev="sd") {
  # TODO: dev could be givne as a function instead
  if (dev=="sd")  devrange <- sd(x, na.rm=TRUE) * devno
  else if (dev=="mad") devrange <- stats::mad(x, na.rm=TRUE) * devno
  else stop("dev only supports 'sd' or 'mad'")

  avg <- mean(x, na.rm=TRUE)
  topval <- avg + devrange
  bottomval <- avg - devrange
  x[x > topval] <- topval
  x[x < bottomval] <- bottomval
  return(x)
}
