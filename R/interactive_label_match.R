# Author: BTC
# 20190105 WF - added to lncdr

#' interactive_label_match:  match labels from one string vector with another
#' @param labels1 are the set of labels to match
#' @param labels2 are where to pull potential matches from
#' @export
#' @examples 
#'    # as prefix
#'    labels1 <- ysr_datadict_nondirect$Field.Label
#'    label2 <- asr_datadict_nondirect$Field.Label
#'    interactive_label_match(labels1, labels2)

interactive_label_match <- function(labels1, labels2){
   partialmatches_output<-NULL
   for (Fl in labels1){
      print(Fl)
      print(length(agrep(Fl, labels2)))
      if (length(agrep(Fl, labels2))!=0){
         partialmatch<-agrep(Fl, labels2, value=TRUE)
         print(partialmatch)
         usepartialmatch<-readline(prompt = "Use Match (0:No; 1:Yes)")
         usepartialmatch<-as.numeric(usepartialmatch)
         if (usepartialmatch==1){
            if (length(partialmatch)>1){
               partialmatchindex<-readline(prompt = "which match (index)")
               partialmatchindex<-as.numeric(partialmatchindex)
               partialmatch<-partialmatch[partialmatchindex]
            }
            partialmatch_fl<-c(Fl, partialmatch)
            partialmatches_output<-rbind(partialmatches_output, partialmatch_fl)
         }
      }
   }
   return(partialmatches_output)
}
