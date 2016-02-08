# Author: Will Foran

#' generate a sequence and minor/major tick labs for a ggplot axis
#' @param st start of sequence
#' @param ed end of sequence
#' @param by interval steps
#' @param labels.at value-of/where-to-place labels
#' @export
#' @examples 
#'  ts <- tick_seq(-10,10,5, c(-10,10,0) )
#'  # RETURNS
#'  #  ts$labels "-10" ""  "0"   ""  "10" 
#'  #  ts$breaks  -10  -5   0    5   10
#'  p <- ggplot()
#'  p + scale_x_continuous(breaks=ts$breaks,labels=ts$labels)
# 
tick_seq <- function(st,ed,by,labels.at) {
  # make sure we get sorted numeric input
  labels.at <- sort(as.numeric(labels.at))
  # make a sequence 
  s<-seq(st,ed,by=by)
  # make empty labs for each item in seq
  labs<-rep("",length(s))
  # find where our labels are in the seq
  idx <- s %in% labels.at;

  # check all desired labels are in the sequence
  # remove labels.at not in the sequence
  if(length(which(idx)) != length(labels.at) ) {
     warning(sprintf('desired labels %s are not in seq %d to %d by %d',
                     paste(labels.at[!labels.at%in%s],collapse=','),
                      st,ed,by))
    labels.at <- labels.at[labels.at%in%s]
  }

  # add numeric labels to otherwise empty ("") labels
  labs[idx] <- labels.at
  # give a list with labels and sequence
  return(list(labels=labs, breaks=s))
}

