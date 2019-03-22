#!/usr/bin/env Rscript
######################

#' growthrate_gam
#'
#' https://stats.stackexchange.com/questions/190348/can-i-use-bootstrapping-to-estimate-the-uncertainty-in-a-maximum-value-of-a-gam
#' @description confidence intervals change of gam modeled variable (age)
#' @param m -- gam model object
#' @param agevar -- variable for growthrate
#' @param n -- number of iterations to run (quick)
#' @param qnt -- quantiles to use for confidence interval
#' @import stringr
#' @export
#' @examples
#'  d <- read.csv("/Volumes/Phillips/R03_BehavioralBTC/data/btc_R03scoredmeasures_20190313.csv") %>%
#'    group_by(id) %>%
#'    mutate(visit=rank(d8))
#'  f <- f1score ~ s(Ageatvisit) + s(visit) + s(id, bs="re")
#'  m <- gam(f, data=d)
#'  ci <- growthrate_gam(m, 'Ageatvisit')
growthrate_gam <- function(m, agevar, n=10000, qnt=c(.025,.975)) {
  simdiff <- sim_diff1_from_gam(m, agevar, n.iterations=n)
  ci <- ci_from_simdiff1(simdiff$pred, simdiff$ages, qnt=qnt)
}



#find vars in a formula surround by s(...), ignore s(...,bs="re")
find_covars_gam <- function(fml, ...) {
   require(stringr)
   ind <- as.character(fml)[3] 
   vars <- stringr::str_extract_all(ind, '(?<=s\\().*?(?=\\))')[[1]] 
   no_re <- grep("re[\"']", vars, value=T, invert=T)
   # remove anything else
   if(length(list(...)) != 0L) {
     no_re <- no_re[ ! no_re  %in% c(...)]
   }
   return(no_re)
}

sim_diff1_from_gam <- function(m, agevar, id='id', n.iterations=10000) {
   ## From 
   # https://stats.stackexchange.com/questions/190348/can-i-use-bootstrapping-to-estimate-the-uncertainty-in-a-maximum-value-of-a-gam

   v <- m$model[, agevar]
   cond_list <- list(seq(min(v), max(v), by=.1))
   pp <- data.frame(a=cond_list[[1]], b=Inf)
   # names should match what went into the model
   names(pp) <- c(agevar, id)
   # for all 
   for(cv in find_covars_gam(m$formula, agevar)) pp[,cv] <- mean(m$model[,cv]) 

   Xp <- predict(m, pp, type="lpmatrix")

   mu_beta <- coef(m)
   sigma_Vb <- vcov(m) #  variance-covariance matrix of the main parameters  fitted model
   # used as: a positive-definite symmetric matrix specifying the covariance matrix of the variables.

   set.seed(10)
   mrand <- mvrnorm(n.iterations, mu_beta, sigma_Vb) 

   ilink <- family(m)$linkinv

   # only want inetercept and agevar
   keep_cols <- grep(paste0('Intercept|',agevar),dimnames(Xp)[[2]],value=T)
   Xp_agevar <- Xp[, keep_cols]
   mrand_agevar <- mrand[, keep_cols]

   # generate a whole bunch of plausable values, get the diff
   pred <- lapply(seq_len(n.iterations), function(i)  {
                  pred <- ilink(Xp_agevar %*% mrand_agevar[i, ])
                  dff <- c(NA,diff(pred))
                  #ci <- quantile(dff, c(.025,.975)) ## get 95% CI
                 })

   return(list(pred=pred, ages=pp[,1]))
}

ci_from_simdiff1 <- function(pred, ages, qnt=c(.025,.975)) {

   names(pred) <- 1:length(pred)
   mm <- t(bind_rows(pred))

   # this is the ouptut !
   mean_dff <- apply(mm, 2, mean)
   ci <- apply(mm, 2, quantile, qnt, na.rm=T)
   colnames(ci) <- ages
   out <- data.frame(mean_dff=mean_dff, ages=ages)
   ci_out <- t(ci)
   dimnames(ci_out)[[2]] <- c('ci_low','ci_high')
   return(cbind(out,ci_out))

   # old: return just ci and mean_dff
   return(list(ci=ci,mean_dff=mean_dff))

   # this is for fun
   ages[which.min(ci[1,])]
   ages[which.min(ci[2,])]

   plot(ages, means_dff)
   for(i in 1:10) lines(ages, pred[[i]])
}

#      #####factor 1############
#      m1_factormodel<-gam(f1score~s(Ageatvisit)+s(id, bs="re"),data=coglongdf)
#      derivsfactor1<-ci_from_simgam(m1_factormodel, 'Ageatvisit')
#      
#      
#      plotgammfactorwithderiv<-function(df,model,derivs){
#      ci<-data.frame(derivs$ci)
#      derivages<-as.numeric(gsub("X","",names(ci)))
#      names(ci)<-derivages
#      
#      cit<-as.data.frame(t(ci))
#      names(cit)<-c("low","high")
#      cit$ages<-row.names(cit)
#      
#      meanderivdf<-as.data.frame(derivs$mean_dff)
#      names(meanderivdf)<-"deriv"
#      meanderivdf$age<-derivages
#      sigages<-
#      
#      
#      ggplot(meanderivdf,aes(x=age,y=1,fill=deriv))+geom_tile()+scale_fill_gradient2(low = "blue", high = "red", mid = "white",midpoint = 0, space = "Lab",breaks=c(max(meanderivdf$deriv,na.rm=TRUE),0,min(meanderivdf$deriv,na.rm=TRUE)),limits=c(min(meanderivdf$deriv,na.rm=TRUE),max(meanderivdf$deriv,na.rm=TRUE)))
#      
#      
#      
#      
#      }
#      
#      
#      cifactor1<-derivsfactor1$ci
#      
#      derivsfactor1$mean_dff
#      
#      
#      
#      
#      
#      ######
#      
#      
#      
#      
#      
#      
#      
#      
#      
#      
#      
#      
#      
#      
#      
#      
#      
#      
#      
#      
#      
#      ### NAIVE ATTEMPT 1
#      periodofsagechange<-function(d, fml, idvar, outcomevar, agevar,
#                                   covars=NULL, n.iterations=5000, outputtrajectories=FALSE,
#                                   njobs=10){
#      ####computes boot-strapped semi parametric age-trajectories with GAMM
#      ###and finds points of growth and maturation based on derivate of age- effect
#      #### based on Simmonds, D. J., Hallquist, M. N., & Luna, B. (2017).Neuroimage, 157, 695-704.
#      ### for variable agevar predicting outcome var with GAMM######
#      #### additional covar(s) passed as a list######
#      
#      ###bootstrapps across SUBJECTS (idvar), respecting nested data#######
#      
#      ####ouptuts include derivative and associated boostrapped p-value (uncorrected) per agebin
#      ####optional output of overall bootsrapped age p-value
#      #####optional output of all boostrapped trajectories (LARGE FILE ALERT!)
#      
#      
#      # fml <- f1score~s(Ageatvisit)+s(id, bs="re")
#      # agevar <- 'Ageatvisit'
#      
#      
#         # RESAMPLE
#         # get a list of indexes of rows to use for bootstrap
#         # # TODO: maybe use more sophisticated method
#         # all_idxs <- 1:nrow(d)
#         # perm_idxs <- lapply(1:n.iterations,
#         #                     function(i){sample(x=all_idxs, size=nrow(d), replace=T)})
#      
#         # # ALTERNATIVE: SHUFFLE -- reassign ages blindly
#         # d[, agevar] <- sample(x=d[, agevar], size=nrow(d))
#         # perm_idxs <- lapply(1:n.iterations, function(i) 1:nrow(d))
#      
#         # # ALTERNATIVE 2: SHUFFLE within subjects
#         # d %>%
#         #    group_by(!!id) %>% 
#         #    mutate(!!agevar := sample(!!agevar, size=n(), n=n())
#         # d[d$n==1, agevar] <- sample(d[d$n==1, agevar], size=length(which(d$n==1))) 
#      
#         # #  
#      
#         # # run model n times
#          models <- mclapply(mc.cores = njobs, perm_idxs, function(i){ gam(fml, data=d[i,]) })
#      
#      
#          ## use each models pred to get derivitive
#          # use same range for all predictions
#          v <- d[, agevar]
#          cond_list <- list(seq(min(v), max(v), by=.1))
#          names(cond_list) <- agevar
#          # get diff of preds
#          driv <- 
#             lapply(models, function(m) {
#                       p <- get_predictions(m, cond=cond_list,
#                                            rm.ranef=TRUE, print.summary=FALSE)
#                       d <- c(diff(p$fit),NA)
#                    })
#      
#      
#      }

plotgammfactorwithderiv<-function(df,model,derivs,agevar,yvar,idvar,plotsavename, xplotname="Age",yplotname="fit",savename="growthrate"){
  # plotsavename<-paste0("/Users/brendenclemmens/Desktop/Projects/R03_behavioral/Data/",paste0(savename,".pdf"))
  ci<-data.frame(derivs$ci)
  derivages<-as.numeric(gsub("X","",names(ci)))
  names(ci)<-derivages
  
  cit<-as.data.frame(t(ci))
  names(cit)<-c("low","high")
  cit$age<-row.names(cit)
  
  meanderivdf<-as.data.frame(derivs$mean_dff)
  names(meanderivdf)<-"deriv"
  meanderivdf$age<-derivages
  sigages<-merge(meanderivdf,cit,by="age")
  sigages$derivthresh<-sigages$deriv
  sigages$derivthresh[sign(sigages$low)!=sign(sigages$high)]<-0
  
  ## setup derivitive raster plot
  maturation_pnt <- min(sigages$age[sigages$derivthresh==0])
  deriv_range <- range(meanderivdf$deriv,na.rm=T)
  tile <- 
     ggplot(sigages) +
     aes(x=age, y=1, fill=derivthresh) +
     geom_raster(interpolate=TRUE) +
     scale_fill_gradient2(
        low = "blue", mid = "white", high = "red", 
        midpoint = 0,
        space = "Lab",
        breaks=sort(c(0,deriv_range)), # assumes range covers 0
        limits=deriv_range
      ) +
      # draw dotted line where maturation point is
      geom_segment(
          aes(x=maturation_pnt ,
              xend=maturation_pnt ,
              y=.5, yend=1.5),
          linetype=2,
          colour="black") +
       xlab("\nAge")
  # lunaize the figure
  tile_luna <- lunaize_geomraster(tile) +
      theme(text = element_text(size=36))

  modeldata<-data.frame(ydata=model$y,agevar=model$model[,agevar])
  agepred<-get_predictions(model,cond=list(Ageatvisit=derivages))
  
  ageplot<-
     ggplot(agepred) +
     aes_string(x=agevar, y='fit') + 
     # solid bold line for fitted model
     geom_line(colour="black", size=2)+
     # individual points for actual data
     geom_point(data=modeldata, aes(y=ydata), alpha=.2)+
     # connect individual with opaque lines 
     geom_line(data=df, aes_string(y=yvar,group=idvar), alpha=.2) +
     # label plot
     ylab(yplotname)+
     xlab(xplotname)
  # lunaize main plot
  ageplot_luna<-LNCDR::lunaize(ageplot)+
      theme(text = element_text(size=36),
            axis.title.x=element_blank(),
            axis.text.x=element_blank())

  library(grid)
  library(gridExtra)
  tilegrob<-ggplotGrob(tile_luna)
  agegrob<-ggplotGrob(ageplot_luna)
  
  g<-rbind(agegrob,tilegrob,size="first")
  panels <- g$layout$t[grep("panel", g$layout$name)]
  g$heights[panels] <- unit(c(1,.1),"null")
  pdf(plotsavename, height = 9, width = 12)
  grid.draw(g)
  dev.off()

}

lunaize_geomraster<-function(x){
  x+
   theme_bw()+
   theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.ticks.y     = element_line(colour="white"),
    axis.title.y     = element_blank(),
    axis.ticks.y     = element_blank(),
    axis.text.y      = element_blank(),
    legend.position  = "none")
  #+theme(axis.title.x=element_blank(),axis.text.x=element_blank())
}
