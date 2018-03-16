#
# Author:  Will Foran
# Packager: Will Foran
# 

#' Create .1D files froma data frame
#'  * works on a dataframe with 'block' column
#'  * cats a 1D file, optionally sink'ed to a specified file
#' @param d dataframe with manditory 'block' column
#' @param colname is the column to use for onset time
#' @param fname is where to save the output 1D. if NULL does not save to a file
#' @param dur if not null, what to set as the duration part of onset:duration 
#' @param amp if not null, what to set as the amplitude part of onset*amplitude
#' @param nblocks expected number of blocks or runs. if null will use max
#' @export
#' @examples 
#' save1D( data.frame(onset=1:9,durcol=1,block=1:3), 'onset',dur='durcol') 
save1D <- function(d,colname=1,fname=NULL,dur=NULL,amp=NULL,nblocks=NULL){
   # make sure we dont have a data.table
   d<- as.data.frame(d)
  
   # we require a block column 
   # this could also have been call 'run' or 'run.number'
   if(! 'block' %in% names(d) ) stop('save1D needs input dataframe to have a column named "block"')

   ## check that we have the colname and durname in the datarfame passed
   # TODO: allow numeric column name?
   if( ! colname %in% names(d)) stop('cannot find ',colname, ' in dataframe')
   if(!is.null(dur) && ! dur %in% names(d)) stop('cannot find ', dur, ' in dataframe')
   if(!is.null(amp) && ! amp %in% names(d)) stop('cannot find ', amp, ' in dataframe')

   ## remove NA and -1
   badidx <- is.na(d[,colname]) | d[,colname]<0                        # colname (onsettime)
   if(!is.null(dur)) { badidx <- badidx | is.na(d[,dur]) | d[,dur]<0 } # duration  if specified
   if(!is.null(amp)) { badidx <- badidx | is.na(d[,amp]) | d[,amp]<0 } # amplitude if specified
   d <- d[!badidx,]


   # arrange by block and onset
   d <- d[order(d$block,d[,colname]),]

   # where to write stimetimes (filename or stdout)
   if(!is.null(fname)) sink(fname)


   linePerblock(d,colname,dur=dur,amp=amp,nblocks=nblocks) 
   cat("\n")
   # turn of sink if we had it on
   if(!is.null(fname)) sink()
}

p0fmt <- function(fmt,...){
 paste0(collapse=' ',sprintf(fmt,...))
}

# collapse trials into lines per blocks
linePerblock <- function(d,colname=1,nblocks=NULL,dur=NULL,amp=NULL) {
  # if number of blocks is not set, use max
  if(is.null(nblocks)) nblocks=max(d$block)

  cat(
    paste(
      collapse="\n",
      sapply(1:nblocks,
        function(b) {
          bd <- as.data.frame(subset(d,block==b,select=-block))
          if(nrow(bd) > 0L) {
            if(is.null(dur)&&is.null(amp))
              return(p0fmt('%0.2f',bd[,colname]))
            else if(!is.null(dur)&&is.null(amp))
              return(p0fmt('%0.2f:%0.2f',bd[,colname],bd[,dur]))
            else
              return(p0fmt('%0.2f:%0.2f*%0.2f',bd[,colname],bd[,dur],bd[,amp]))
          } else {
            return(sprintf('*'))
          }
        }
      )
   )
 )
}

