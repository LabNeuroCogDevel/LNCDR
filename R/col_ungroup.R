#
# Author: Will Foran
# 

# 20171031 WF 

#' col_ungroup: pull unique bits out of redudant column names to make dataframe longer
#'  e.g. Contact1Phone Contac2Phone -> Phone grpvar
#' @param d is the dataframe with redudant columns
#' @param patt is the pattern that matches redudant colnames (e.g. 'Contact[12]')
#' @param grpvarcolname is name to give to the new column (def. 'grpvar')
#' @export
#' @examples 
#'    # as prefix
#'    d2   <- data.frame(x=1:3,Contact1Phone=4:6,Contact2Phone=7:9,Contact1Address=2:4)
#'    d2.ug<- col_ungroup(d2,'Contact[12]')
#'    # with suffix
#'    d1   <- data.frame(x=1:3,measure.a=4:6,measure.b=7:9,m2.a=2:4,m2.b=5:7)
#'    d1.ug<- col_ungroup(d1,'\\.(a|b)$')
#'    # watch out for patterns that match multiple parts of the same string
#'    # e.g. 'a|b' will be removed from 'measure.a' to make 'mesure'
#'
#


col_ungroup <- function(d,patt,grpvarcolname="grpvar") {
   
   # get all the unique matches of pattern in the column names
   # make sure we have at least 2 different matches
   # and that there is stuff after the pattern to work with (Contact1Phone, not juct Contat1)
   cn <- names(d)
   r <- regexpr(patt,cn,perl=T)
   grpvars <- unique(regmatches(cn,r))
   if( length(grpvars) < 2L ) { stop('no point; less than 2 matches to pattern "',patt,'": ',grpvars) }

   idvars <- which(r==-1)
   newvars <- unique(gsub(patt,'',cn[-idvars]))
   if( length(newvars) < 1L ) { stop('no point; no columns match pattern "',patt,'" and still have anything left') }

   # list of dataframes, one for each var in grpvars, that we can rbind together
   df.list <- 
      lapply(grpvars, function(g) {
             # which of the column names do we actually want
             # all that match e.g. Contact1
             # rename to common name (e.g Contact1Phone -> Phone)
             this.grpvar.idx <- grep(g,cn,fixed=T)

             #this.newvars <- gsub(g,'',cn[this.grpvar.idx])  # BAD
             # if g is say 'a', we might replace 'measure.a' to 'mesure.'
             this.newvars <- gsub(patt,'',cn[this.grpvar.idx])

             long.sub <- d[,c(idvars, this.grpvar.idx )]
             names(long.sub)[-idvars] <- this.newvars 

             # if this group var is missing some of the columns the other has, make them NA
             # (so we can rbind without trouble later)
             emptynewvars <- setdiff(newvars,this.newvars)
             if(length(emptynewvars)>0L) long.sub[,emptynewvars]<-NA

             # add our group var as a new column
             long.sub[,grpvarcolname] <- g
             return(long.sub)
   })

   # rbind all dataframes together to one long df
   allnames<-unique(unlist(sapply(df.list,FUN=names)))
   d.long <- tryCatch( 
               Reduce(rbind,df.list),
               error=function(e){ 
                  stop('your pattern "', patt,'" ',
                       'might have gotten too much! rbind is failing. ',
                       'Various splits have the names:',
                       paste(collapse=" ",sep=" ",allnames),
                       '\n',e) }) 
   return(d.long)

}
