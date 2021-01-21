# Author: BTC
# 20190105 WF - added to lncdr

#' interactive_label_match:  match labels from one string vector with another
#' @param labels1 are the set of labels to match
#' @param labels2 are where to pull potential matches from
#' @param prompt_equal default FALSE, set to true if you want to confirm all matches
#' @export
#' @examples 
#'    # as prefix
#'    labels1 <- ysr_datadict_nondirect$Field.Label
#'    label2 <- asr_datadict_nondirect$Field.Label
#'    interactive_label_match(labels1, labels2)

interactive_label_match <- function(labels1, labels2, prompt_equal=FALSE, diffprint=TRUE){
   partialmatches_output<-NULL

   # first input should probably be the longer one
   # we are trying to match all of the first to any of the second
   if(length(labels1) < length(labels2))
       warning("iterating through first list but it has fewer items than second")

   # TODO: truncate if these names are too long
   list1name <- substitute(labels1)
   list2name <- substitute(labels2)

   for (Fl in labels1){
      partialmatch <- agrep(Fl, labels2, value=TRUE)
      n_matches <- length(partialmatch)
      if(n_matches == 1 && partialmatch == Fl && ! prompt_equal) {
        cat("# auto accepting only and prefect match:", Fl ,"\n")
        partialmatch_fl<-c(Fl, partialmatch)
        partialmatches_output<-rbind(partialmatches_output, partialmatch_fl)
        next
      }
      if (n_matches != 0){
        cat("# have", n_matches, "partial matches in ...\n")
        cat(paste0(list1name, ":\n    ", Fl, "\n"))
        cat("partial matches in", list2name, "\n")


        cat(paste(collapse="\n",sep=":",
                  c(1:n_matches),
                  gsub("^","  ",partialmatch)),
            "\n")

        # show context diff if ansi terminal (default to TRUE)
        if(diffprint)
           cat(paste(collapse="\n",tail(n=-3, capture.output(
             diffobj::diffChr(rep(Fl,n_matches),
                              partialmatch,
                              pager="off", color.mode="rgb", mode="context")))))

        partialmatchindex<-readline(prompt = "Index of match (0=None/Skip, default=1): ")
        if(length(partialmatchindex) == 0L) partialmatchindex==1
        else as.numeric(partialmatchindex)

        if(partialmatchindex < 1) next

        partialmatchindex<-as.numeric(partialmatchindex)
        partialmatch<-partialmatch[partialmatchindex]
        partialmatch_fl<-c(Fl, partialmatch)
        partialmatches_output<-rbind(partialmatches_output, partialmatch_fl)
        
      } else {
          cat("# no matches in", list2name, "for", Fl)
      }
   }
   return(partialmatches_output)
}
