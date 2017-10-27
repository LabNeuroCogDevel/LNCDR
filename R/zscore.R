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

#20171027  BTC 
#' zscores all columns in a df except those whose names are passed as "ignorecolnames"
#' @param df is dataframe which columns to zscore
#' @param ignorecolnames is column vector of strings with column names to ignore
#' @export
#' @examples 
#'   d <- data.frame(a=1:10, b=seq(10,100,by=10), c=rep('a',10) )
#'   LNCDR::zscorecols( d, 'c' )

zscorecols<-function(df,ignorecolnames){
   for (c in 1:ncol(df)){
    if(names(df)[c] %in% ignorecolnames){next}
     print(c)
     df[,c]<-(df[,c]-(mean(df[,c],na.rm=TRUE)))/sd(df[,c],na.rm=TRUE)
   }
   return(df)
}
