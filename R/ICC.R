# Author:  Brenden Tervo-Clemmens
# Packager: Will Foran

#' ICClong
#'
#' @description ICC wrapper for wide format data frame
#' @param d            long format (row per visit, possibly many rows per subject)
#' @param vars         which columns to return ICC values for
#' @param subjectvar   column uniquely identifying subject
#' @param visitvar     column uniquely identifying visit
#' @param maxvisits    cutoff for number of visits any one subject contributes
#' @importFrom dplyr bind_rows
#' @importFrom tidyr spread
#' @importFrom psych ICC
#' @export
#' @examples
#' d <- data.frame(id=rep(1:6, 3), visit=rep(1:3, each=6),
#'                 a=runif(18,0,10),
#'                 b=runif(18,0,10))
#' d_ICCs <- ICCforlongformat(d, c('a','b'), "id", "visit")

ICCforlongformat<-function(d, vars, subjectvar, visitvar, maxvisits=2){
  bind_rows(lapply(vars, function(v) {
    # subset to just the var we want ICC for
    # id is need to spread but don't want in ICC
    ICCdflong <- d[, c(subjectvar, visitvar, v)]
    ICCdfwide <- spread(ICCdflong, visitvar, v)
    # remove subjectvar (don't want in ICC) and now columns are only visits
    ICCdfwidethreshold <- ICCdfwide[, -1]
    # keep only visits up to our maxvisits
    ICCdfwidethreshold <- ICCdfwidethreshold[, 1:maxvisits]
    # remove incomplete
    complete_only <- complete.cases(ICCdfwidethreshold)
    if (!any(complete_only))
       stop(v, ": no subjects with value for all visits!\n\t",
            paste(capture.output(print.data.frame(ICCdfwidethreshold)),
                  collapse="\n\t"))
    ICCdfwidethreshold <- ICCdfwidethreshold[complete_only, ]
    # perform ICC
    ICCout <- ICC(ICCdfwidethreshold, missing=TRUE)
    ICCsum <- as.data.frame(ICCout$results)
    ICCwide <- as.data.frame(t(ICCsum$ICC))
    names(ICCwide) <- ICCsum$type
    # return details
    ICCinfo<-data.frame(var=v,
                        observations_used=nrow(ICCdfwidethreshold),
                        visits_used=maxvisits,
                         stringsAsFactors=FALSE)
    # bind info
    ICCout <- cbind(ICCinfo, ICCwide)
  }))
}
