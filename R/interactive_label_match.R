# Author: BTC
# 20190105 WF - added to lncdr
# 20210121 WF - better UI, fewer prompts. add accept_single, prompt_equal, and diffprint

nonewline <- function(x) {
    gsub('\n','¶', x)
}

#' interactive_label_match:  match labels from one string vector with another
#' @param labels1 are the set of labels to match
#' @param labels2 are where to pull potential matches from
#' @param accept_single default FALSE. if only one match, accept it.
#' @param prompt_equal default FALSE. set to true if you want to confirm all matches
#' @param diffprint default TRUE. show colored diff. only useful in terminal
#' @export
#' @examples 
#'    # as prefix
#'    labels1 <- ysr_datadict_nondirect$Field.Label
#'    label2 <- asr_datadict_nondirect$Field.Label
#'    interactive_label_match(labels1, labels2)
interactive_label_match <- function(labels1, labels2, accept_single=FALSE, prompt_equal=FALSE, diffprint=TRUE){
   partialmatches_output<-NULL

   # first input should probably be the longer one
   # we are trying to match all of the first to any of the second
   n_labels1 <- length(labels1)
   if(n_labels1 < length(labels2))
       cat("#WARNING: iterating through first list but it has fewer items than the second. consider switching fist and second argument.")

   # TODO: truncate if these names are too long
   list1name <- substitute(labels1)
   list2name <- substitute(labels2)

   for (i in 1:n_labels1){
      Fl <- unname(labels1[i])
      disp_Fl <- nonewline(Fl)
      exact_match_idx <- which(Fl == labels2)
      if(length(exact_match_idx) == 1L) {
        cat("# auto accepting exact match:", disp_Fl ,"@", exact_match_idx, "\n")
        partialmatch <- labels2[exact_match_idx]
        partialmatch_fl<-c(Fl, partialmatch)
        partialmatches_output<-rbind(partialmatches_output, partialmatch_fl)
        next
      }

      partialmatch <- agrep(Fl, labels2, value=TRUE)
      n_matches <- length(partialmatch)
      if (n_matches == 1 && accept_single) {
        cat("# accepting only match:", disp_Fl ," == ", nonewline(partialmatch), "\n")
        partialmatch_fl<-c(Fl, partialmatch)
        partialmatches_output<-rbind(partialmatches_output, partialmatch_fl)
        next
      }
      if (n_matches != 0){
        cat("# have", n_matches, "partial matches in ...\n")
        cat(paste0(list1name, ":\n    '", disp_Fl, "'\n"))
        cat("# partial matches in", list2name, "\n")


        # add extra new line if matches also have new lines
        endsep <- "'"
        if(any(grepl('\n', partialmatch))) endsep <- "'\n"

        cat(paste(collapse="\n",sep=":",
                  c(1:n_matches),
                  gsub("^","  '", gsub("$", endsep, partialmatch))),
            "\n")

        # show context diff if ansi terminal (default to TRUE)
        if(diffprint)
           cat(paste(collapse="\n",
             c(tail(n=-3, capture.output(
                diffobj::diffChr(
                             rep(Fl, n_matches),
                             partialmatch,
                             pager="off", color.mode="rgb", mode="context"))),
               "")))

        ptext <- sprintf("[%d/%d] Index of match (0=None/Skip, default=1, max=%d): ", i, n_labels1, n_matches)
        partialmatchindex<-readline(prompt = ptext)
        if(length(partialmatchindex) == 0L) partialmatchindex==1
        else as.numeric(partialmatchindex)

        if(partialmatchindex < 1) next

        partialmatchindex<-as.numeric(partialmatchindex)
        partialmatch<-partialmatch[partialmatchindex]
        partialmatch_fl<-c(Fl, partialmatch)
        partialmatches_output<-rbind(partialmatches_output, partialmatch_fl)
        
      } else {
          cat("# no matches in", list2name, "for", Fl, "\n")
      }
   }
   return(partialmatches_output)
}
