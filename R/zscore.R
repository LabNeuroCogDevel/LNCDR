#
# Author:  Brenden Tervo-Clemmens
# Packager: Will Foran
# 

# 20171027 WF 
#  use general zscore function below (instead of rewritting)
#' zscore: (vector - mean)/sd 
#' @param x is the numeric vector to zscore
#' @export
#' @examples 
#'    z<-zscore(1:10)
#'    mean(z) == 0

zscore <- function(x) (x - mean(x, na.rm=T) )/sd(x, na.rm=T)

#' zscores within factor for long format/nested data
#' @param fact is grouping factor
#' @param score is variable
#' @param VERBOSE flag for printing factor during loop iteration
#' @export
#' @examples 
#'   x <- 1:10; f <- sample(c(rep('x',5),rep('y',5)))
#'   z.vec  <- zscorewithinfactor(f, x)
#'   z.list <- tapply(x,f,zscore)
zscorewithinfactor<-function(fact, score, VERBOSE=T){
  factors<-unique(fact)
  fzscore<-score*0
  for (f in factors){
    if(VERBOSE) print(f)
    findex<-which(fact==f)
    fdata<-score[findex]
    fzscore[findex]<-zscore(fdata)
  }
  return(fzscore)
}

#20171027  BTC 
#' zscores all columns in a df except those whose names are passed as "ignorecolnames"
#' @param df is dataframe which columns to zscore
#' @param ignorecolnames is column vector of strings with column names to ignore
#' @param VERBOSE flag for printing factor during loop iteration
#' @export
#' @examples 
#'   d <- data.frame(a=1:10, b=seq(10,100,by=10), c=rep('a',10) )
#'   d.z1 <- LNCDR::zscorecols( d, 'c' )
#' \dontrun{
#'   library(dplyr)
#'   d.z2 -> df %>% mutate_at( vars( matches('a|b') ) ,LNCDR::zscore)
#' }

zscorecols<-function(df,ignorecolnames,VERBOSE=T){
   for (ci in 1:ncol(df)){
    if(names(df)[ci] %in% ignorecolnames){next}
     if(VERBOSE) print(ci)
     df[,ci]<-zscore(df[,ci])
   }
   return(df)
}
