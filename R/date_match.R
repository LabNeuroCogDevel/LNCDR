#
# Author:  Will Foran
# 
#' match dates in a data frame by one-to-many merge and rank. Useful to match behavioral to brain over multiple timepoints.
#' @param d1 is the main dataframe 
#' @param d2 is the supplimental dataframe with dates to match
#' @param datecol1 is date column one, this will be the id column
#' @param datecal2 is date column two. this will be the many in one-to-many merge. defaults to same name as datacol1
#' @param maxdatediff is the maximum time between matches (default 180 days)
#' @param maxrank is highest index value of datediff ranked. set to 1 to ingore ties (deafult is 1.999)
#' @export
#' @examples 
#'   dummy1 <- data.frame(id=c(1,2,3,3),d=c(20120302,20120302,20120302,20150302),j=1)
#'   dummy2 <- data.frame(id=c(1,2,3,3,3),d=c(20120202,20120402,20120902,20130902,20150102),j2=2)
#'   dummy1$d <- lubridate::ymd(dummy1$d)
#'   dummy2$d <- lubridate::ymd(dummy2$d)
#'   date_match(dummy1,dummy2,'id','d')
#' 
#' 

date_match <-function(d1,d2,idcol,datecol1,datecol2=datecol1,maxdatediff=180,maxrank=2-1e-3){
   require(dplyr)

   # change name if datecolumns are the same
   if(datecol1==datecol2) {
      datecol2=paste0(datecol1,'.y')
      names(d2)[ which(names(d2)==datecol1) ] <- datecol2
   }

   # check data time. we dont _need_ columns to be Date type, but it'll probably be an issue
   if(class(d1[1,datecol1]) != 'Date' ||
      class(d2[1,datecol2]) != 'Date') { 
        warning('not all specified date columns are "Date" type. maxdatediff will be weird.\n\t** consider e.g. d$datecol <- lubridate::ymd(datecol)  **')
   }

   # merge all the d2 onto each potential d1 match
   diffstr <- sprintf("%s-%s",datecol1,datecol2)
   onetomany <- 
      merge(d1,d2,by=idcol) %>%
      group_by_(idcol,datecol1) %>%
      mutate_(datediff=diffstr)  %>%
      mutate(r= datediff %>% abs %>% rank )
   
   # todo: report ties with warning??

   returnval <- onetomany %>% filter(r<=maxrank, abs(datediff) <= maxdatediff) %>% select(-r) %>% ungroup

   # check repeated date2 (best match is not unique)
   d2check <- returnval[,c(idcol,datecol2)]
   d2reps   <- duplicated(d2check)
   if(any(d2reps)) {
      dispreps <- merge( returnval[,c(idcol,datecol1,datecol2) ], d2check[d2reps,], by=c(idcol,datecol2) )
      dfstr <- capture.output( dispreps %>% print.data.frame(row.names=F) ) %>%  paste(collapse="\n")
      warning( 'best match is not unique: repeated second date for subject+first date\n', dfstr, '\n')
   }

   return(returnval)
}


