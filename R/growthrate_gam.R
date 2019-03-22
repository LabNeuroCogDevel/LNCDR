#!/usr/bin/env Rscript
######################

#' growthrate_gam
#'
#' https://people.maths.bris.ac.uk/~sw15190/mgcv/tampere/mgcv-advanced.pdf
#' https://stats.stackexchange.com/questions/190348/can-i-use-bootstrapping-to-estimate-the-uncertainty-in-a-maximum-value-of-a-gam
#' @description posterior simulation for confidence intervals of local slope/growth rate (deriv) of mgvc gam modeled variable (age) - random effects: intercepts only (not predicted) 
#' @param m            mgcv gam model object (only)
#' @param agevar       variable for growth rate
#' @param nnumber      of iterations to run (quick)
#' @param qntquantiles to use for confidence interval
#' @import stringr
#' @export
#' @examples
#'  d <- read.csv("/Volumes/Phillips/R03_BehavioralBTC/data/btc_R03scoredmeasures_20190313.csv") %>%
#'    group_by(id) %>%
#'    mutate(visit=rank(d8))
#'  f <- f1score ~ s(Ageatvisit) + s(visit) + s(id, bs="re")
#'  m <- gam(f, data=d)
#'  ci <- gam_growthrate(m, 'Ageatvisit')
gam_growthrate <- function(m, agevar, n=10000, qnt=c(.025,.975)) {
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


#' plot gam factor with deriv
#'
#' @description plot output of growthrate_gam
#' @export
#' @importFrom itsadug get_predictions 
#' @param d      dataframe model was built on (for actual points)
#' @param model  gam model (for predicted line)
#' @param ci     growthrate_gam output (confidence interval and derivitive)
#' @param agevar column name of age var e.g. 'Ageatvisit'
#' @param yvar   model yvar e.g. 'f1score'
#' @param idvar  line grouping var e.g., 'lunaid'
#' @param plotsavename pdf output name e.g. 'growth.pdf', not saved when NULL
#' @param xplotname 'Age'
#' @param yplotname  'f1score'
#' @examples
#'  
#'  m <- gam(f1score ~ s(Ageatvisit) + s(visit) + s(id, bs="re"), data=d)
#'  ci <- gam_growthrate(m, 'Ageatvisit')
#'  gam_growthrate_plot(d, m, ci, 'Ageatvisit','f1score','id')
gam_growthrate_plot <-
   function(d, model, ci, agevar, yvar, idvar,
            plotsavename=NULL, xplotname="Age", yplotname=yvar){

  require(ggplot2)
  require(grid)
  require(gridExtra)
  require(itsadug)

  # TODO:
  # remove or replace first row mean_dff
  # NA draws weird first color on spectrum
  # TODO:
  # get yvar from model

  ci$mean_dff_clip <- ci$mean_dff
  # when ci bounds include 0 (different sign), no longer signficant
  not_sig <- sign(ci$ci_low)!=sign(ci$ci_high) # with(ci,ci_high*ci_low < 0)
  ci$mean_dff_clip[not_sig] <- 0

  ## setup derivitive raster plot
  maturation_pnt <- min(ci$ages[ci$mean_dff_clip==0], na.rm=T)
  deriv_range <- range(ci$mean_dff, na.rm=T)
  tile <-
     ggplot(ci) +
     aes(x=ages, y=1, fill=mean_dff_clip) +
     geom_raster(interpolate=TRUE) +
     scale_fill_gradient2(
        low = "blue", mid = "white", high = "red",
        midpoint = 0,
        space = "Lab",
        breaks=sort(c(0, deriv_range)), # assumes range covers 0
        limits=deriv_range
      ) +
      # draw dotted line where maturation point is
      geom_segment(
          linetype=2, colour="black",
          aes(x=maturation_pnt, xend=maturation_pnt, y=.5, yend=1.5)) +
       xlab("\nAge")
  # lunaize the figure
  tile_luna <- lunaize_geomraster(tile) +
      theme(text = element_text(size=36))

  # predictions
  modeldata<-data.frame(ydata=model$y, agevar=model$model[, agevar])
  condlist <- list(a=ci$ages)
  names(condlist) <- agevar
  agepred <- get_predictions(model, cond=condlist)

  ageplot<-
     ggplot(agepred) +
     aes_string(x=agevar, y='fit') +
     # solid bold line for fitted model
     geom_line(colour="black", size=2)+
     # individual points for actual data
     geom_point(data=modeldata, aes(y=ydata, x=agevar), alpha=.2)+
     # connect individual with opaque lines
     geom_line(data=d, aes_string(y=yvar, group=idvar), alpha=.2) +
     # label plot
     ylab(yplotname)+
     xlab(xplotname)
  # lunaize main plot
  ageplot_luna<-LNCDR::lunaize(ageplot)+
      theme(text = element_text(size=36),
            axis.title.x=element_blank(),
            axis.text.x=element_blank())

  tilegrob<-ggplotGrob(tile_luna)
  agegrob<-ggplotGrob(ageplot_luna)
  
  g<-rbind(agegrob,tilegrob,size="first")
  panels <- g$layout$t[grep("panel", g$layout$name)]
  g$heights[panels] <- unit(c(1,.1),"null")
  if(!is.null(plotsavename)) pdf(plotsavename, height = 9, width = 12)
  grid.draw(g)
  if(!is.null(plotsavename)) dev.off()

}

lunaize_geomraster<-function(x){
  x+
   theme_bw()+
   theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y     = element_blank(),
    axis.ticks.y     = element_blank(),
    axis.text.y      = element_blank(),
    legend.position  = "none")
  #+theme(axis.title.x=element_blank(),axis.text.x=element_blank())
}
