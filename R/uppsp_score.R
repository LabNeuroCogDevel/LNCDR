#' upps_scoring: score upps 
#' @param uppspdf - dataframe of recorded uppsp responses
#' @export
#' @example
#'   uppsp<-readxl::read_excel("/Volumes/Phillips/mMR_PETDA/scripts/txt/PET_Sheets.xlsx",sheet="UPPS-P")
#'   uppsp<-uppsp[!is.na(uppsp$ID),]
#'   uppsp_scoring(uppsp)
uppsp_scoring<-function(uppspdf){
   ######BTC & WF#########
   ####scores uppsp 59-item version ####wide format data only ###items must start with column number######
   ####returns five factor scores and total#########
   ##last updated 11202017########

   uppscols<-grep("([0-9]+).*$",names(uppspdf),value=TRUE)
   ####scales
   revitems<-c(2,3,5,7,8,9,10,12,13,15,17,18,20,22,23,25,26,29,30,31,34,35,36,39,40,41,44,45,46,47,49,50,51,52,54,56,57,58,59)
   nonrevitems<-c(1,4,6,11,14,16,19,21,24,27,28,32,33,37,38,42,43,48,53,55)

   uppspdf_adjust<-uppspdf
   for (item in uppscols){
      print(item)

      itemnumber<-gsub("[^0-9]","",item)
      print(itemnumber)
      scores<-as.numeric(unlist(uppspdf[,item]))
      if (as.numeric(itemnumber) %in% revitems){print("rev item")
      revfinaloutscore<-(5-scores)
      uppspdf_adjust[,item]<-revfinaloutscore
      }
   }
   uppspdf_adjust_jd<-uppspdf_adjust[,uppscols]



   score_upps_scales<-function(itemnumbers){
      items_match<-sapply(FUN=function(x){sprintf("^%d\\.",x)},itemnumbers)
      items_match_reg<-paste(items_match,collapse="|")
      rowSums(uppspdf_adjust[,grep(items_match_reg,names(uppspdf_adjust))],na.rm=TRUE)
   }

   ############scales####################################
   NUitems<-c(2,7,12,17,22,29,34,39,44,50,53,58)
   NUscores<-score_upps_scales(NUitems)
   Premeditems<-c(1,6,11,16,21,28,33,38,43,48,55)
   PREscores<-score_upps_scales(Premeditems)
   Persitems<-c(4,9,14,19,24,27,32,37,42,47)
   PERSscores<-score_upps_scales(Persitems)
   SSitems<-c(3,8,13,18,23,26,31,36,41,46,51,56)
   SSscores<-score_upps_scales(SSitems)
   PUitems<-c(5,10,15,20,25,30,35,40,45,49,52,54,57,59)
   PUscores<-score_upps_scales(PUitems)

   totalscores<-score_upps_scales(seq(1,59,1))
   #################################
   UPPS_scales_returndf<-data.frame(NUscores,PREscores,PERSscores,SSscores,PUscores,totalscores)
   names(UPPS_scales_returndf)<-c("upps_negurg","upps_pre","upps_pers","upps_ss","upps_pu","upps_tot")
   return(UPPS_scales_returndf)
}
