#
# Author:  Brenden Tervo-Clemmens
# Packager: Will Foran
# 
#' zscores within factor for long format/nested data
#' @param fact is grouping factor
#' @param score is variable
#' @export
#' @examples 

zscorewithinfactor<-function(fact,score){
  factors<-unique(fact)
  fzscore<-score*0
  for (f in factors){
    print(f)
    findex<-which(fact==f)
    fdata<-score[findex]
    fzscore[findex]<-(fdata-mean(fdata,na.rm=TRUE))/sd(fdata,na.rm=TRUE)
  }
  return(fzscore)
}
