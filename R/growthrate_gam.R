# Author:  Brenden Tervo-Clemmens
# Packager: Will Foran

#' gam_growthrate
#'
#' https://people.maths.bris.ac.uk/~sw15190/mgcv/tampere/mgcv-advanced.pdf
#' https://stats.stackexchange.com/questions/190348/can-i-use-bootstrapping-to-estimate-the-uncertainty-in-a-maximum-value-of-a-gam
#' @description posterior simulation for confidence intervals of local slope/growth rate (deriv) of mgcv gam modeled variable (age) - random effects: intercepts only (not predicted)
#' @param m            mgcv gam model object (only)
#' @param agevar       variable for growth rate
#' @param idvar        random effects/subject id variable, set to NULL if none
#' @param nnumber      of iterations to run (quick)
#' @param qntquantiles to use for confidence interval
#' @importFrom MASS mvrnorm
#' @export
#' @examples
#'  d <- read.csv("/Volumes/Phillips/R03_BehavioralBTC/data/btc_R03scoredmeasures_20190313.csv") %>%
#'    group_by(id) %>%
#'    mutate(visit=rank(d8))
#'  f <- f1score ~ s(Ageatvisit) + s(visit) + s(id, bs="re")
#'  m <- mgcv::gam(f, data=d)
#'  ci <- gam_growthrate(m, 'Ageatvisit', 'id')
gam_growthrate <- function(m, agevar, idvar=NULL, n.iterations=10000, qnt=c(.025, .975)) {
  simdiff <- sim_diff1_from_gam(m, agevar, idvar, n.iterations=n.iterations)
  ci <- ci_from_simdiff1(simdiff$pred, simdiff$ages, qnt=qnt)
}



#find vars in a formula surround by s(...), ignore s(...,bs="re")
# remove any additionaly columns provided as 2nd or more arguments
#        probably agevar

#' find_covars_gam
#'
#' @description pull out covariests from mgcv model formula
#' @param fml  formula to get covariates from
#' @param ...  covariates to ignore (like agevar)
#' @export
#' @examples
#'  fml <- y ~ s(x) + x2 + s(x3, bs="re") + x4
#'  covars <- find_covars_gam(fml, x4) # x, x2
find_covars_gam <- function(fml, ...) {
   ind <- as.character(fml)[3]
   # s(x1) + x2 +s(x3,"re") -> x1, x2
   vars <- unlist(strsplit(ind, "\\+"))  # formula split by +
   vars <- gsub(" ", "", vars) # remove spaces
   vars <- gsub("\\w+\\((.*?)\\)", "\\1", vars) # remove surrounding s()
   # remove random effect
   no_re <- grep("re[\"']", vars, value=T, invert=T)
   no_re <- gsub(",.*", "", no_re) # nothing after comma
   # remove anything else (likely passed in agevar)
   if (length(list(...)) != 0L) {
     no_re <- no_re[ ! no_re  %in% c(...)]
   }
   return(no_re)
}

sim_diff1_from_gam <- function(m, agevar, id="id", n.iterations=10000) {
   v <- m$model[, agevar]
   cond_list <- list(seq(min(v), max(v), by=.1))
   pp <- data.frame(a=cond_list[[1]], b=Inf)
   # names should match what went into the model
   names(pp) <- c(agevar, id)

   # for all covars, pick out the mean
   for (cv in find_covars_gam(m$formula, agevar)) {
      x <- m$model[, cv]
      if (is.character(x) || is.factor(x) ){
         warning("gam w/factor covar, setting all sim to the first!")
         y <- x[1]
         # TODO: maybe pracma::Mode ?
      } else {
          y <- mean(x, na.rm=T)
      }
      pp[, cv] <- y
   }

   Xp <- predict(m, pp, type="lpmatrix")

   mu_beta <- coef(m)
   sigma_Vb <- vcov(m)
   # variance-covariance matrix of the main parameters  fitted model
   # used as: a positive-definite symmetric matrix specifying
   #  the covariance matrix of the variables.

   # set.seed(10)
   mrand <- MASS::mvrnorm(n.iterations, mu_beta, sigma_Vb)

   ilink <- family(m)$linkinv

   # only want inetercept and agevar
   keep_cols <- grep(paste0("Intercept|", agevar), dimnames(Xp)[[2]], value=T)
   Xp_agevar <- Xp[, keep_cols]
   mrand_agevar <- mrand[, keep_cols]

   # generate a whole bunch of plausable values, get the diff
   pred <- lapply(seq_len(n.iterations), function(i)  {
                  pred <- ilink(Xp_agevar %*% mrand_agevar[i, ])
                  dff <- c(NA, diff(pred))
                  return(dff)
                 })

   return(list(pred=pred, ages=pp[, 1]))
}

ci_from_simdiff1 <- function(pred, ages, qnt=c(.025, .975)) {

   names(pred) <- 1:length(pred)
   mm <- t(dplyr::bind_rows(pred))

   # this is the ouptut !
   mean_dff <- apply(mm, 2, mean)
   ci <- apply(mm, 2, quantile, qnt, na.rm=T)
   colnames(ci) <- ages
   out <- data.frame(mean_dff=mean_dff, ages=ages)
   ci_out <- t(ci)
   dimnames(ci_out)[[2]] <- c("ci_low", "ci_high")
   return(cbind(out, ci_out))

   # NEVER REACHED -- left as bad documentation
   # old: return just ci and mean_dff
   return(list(ci=ci, mean_dff=mean_dff))

   # this is for fun
   ages[which.min(ci[1, ])]
   ages[which.min(ci[2, ])]

   plot(ages, mean_dff)
   for (i in 1:10) lines(ages, pred[[i]])
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
#' @param idvar  line grouping var e.g., 'lunaid', set to NULL if no random effect in model
#' @param yvar   model yvar e.g. 'f1score', default pulled from model formula
#' @param plotsavename PDF output name e.g. 'growth.pdf', not saved when NULL
#' @param xplotname 'Age'
#' @param yplotname  'f1score', default is yvar (model yvar)
#' @param draw_maturation T|F, show dotted line on first maturation point
#' @param draw_points T|F, show individual points as scatter plot over gam fit line
#' @param show_all_fill T|F, should we clip the raster fill to only significant ages?
#' @examples
#'
#'  mod <- mgcv::gam(conc~s(uptake), data=CO2)
#'  ci <- LNCDR::gam_growthrate(mod, 'uptake', n = 10000, qnt = c(0.025, 0.975))
#'  plist <- gam_growthrate_plot(cars, mod, ci, 'uptake', xplotname='uptake')
#'
#'  m <- mgcv::gam(f1score ~ s(Ageatvisit) + s(visit) + s(id, bs="re"), data=d)
#'  ci <- gam_growthrate(m, 'Ageatvisit')
#'  gam_growthrate_plot(d, m, ci, 'Ageatvisit', 'id')
#'
#'  # need to explicity set id to NULL if no random effect
#'  m <- mgcv::gam(f1score ~ s(Ageatvisit), data=d)
#'  ci <- gam_growthrate(m, 'Ageatvisit', idvar=NULL)
#'  gam_growthrate_plot(d, m, ci, 'Ageatvisit', idvar=NULL)
gam_growthrate_plot <-
   function(d, model, ci, agevar, idvar=NULL,
            yvar=as.character(model$formula[2]),
            plotsavename=NULL, xplotname="Age", yplotname=yvar,
            draw_maturation=T, draw_points=T, show_all_fill=F){

  require(ggplot2)
  require(itsadug)

  # TODO:
  # remove or replace first row mean_dff
  #   NA draws weird first color on spectrum

  # make sure we have what we say we want
  if (! yvar %in% names(model$model) ) stop(yvar, "not in model dataframe!")

  ci$mean_dff_clip <- ci$mean_dff
  # when ci bounds include 0 (different sign), no longer signficant
  not_sig <- sign(ci$ci_low)!=sign(ci$ci_high) # with(ci,ci_high*ci_low < 0)
  ci$mean_dff_clip[not_sig] <- 0

  # find maturation point after the first signficant age
  onset_sig <- ci$ages[ci$mean_dff_clip != 0]
  maturation_pnt <- NA
  if (length(onset_sig)<=0L || all(is.na(onset_sig))) {
     mat_points_idx <- ci$mean_dff_clip==0 & ci$ages > onset_sig[1]
     if (any(mat_points_idx))
        maturation_pnt <- min(ci$ages[mat_points_idx], na.rm=T)
  }
  # warn about no matruation point
  if (is.na(maturation_pnt) && draw_maturation) {
     warning("No maturation point!")
     draw_maturation <- F
  }

  # show even unsignficant change in raster if show_all_fill
  fill_column <- ifelse(show_all_fill, "mean_dff", "mean_dff_clip")

  ## setup derivitive raster plot
  deriv_range <- range(ci$mean_dff, na.rm=T)
  tile <-
     ggplot(ci) +
     aes_string(x="ages", y=1, fill=fill_column) +
     geom_raster(interpolate=TRUE) +
     scale_fill_gradient2(
        low = "blue", mid = "white", high = "red",
        midpoint = 0,
        space = "Lab",
        breaks=sort(c(0, deriv_range)), # assumes range covers 0
        limits=deriv_range
      ) +
     xlab(sprintf("\n%s", xplotname))

  # draw dotted line where maturation point is
  if (draw_maturation)
   tile <- tile +
      geom_segment(
          linetype=2, colour="black",
          aes(x=maturation_pnt, xend=maturation_pnt, y=.5, yend=1.5))

  # lunaize the figure
  tile_luna <- lunaize_geomraster(tile) +
      theme(text = element_text(size=36))


  # predictions
  modeldata<-data.frame(ydata=model$y, agevar=model$model[, agevar])
  condlist <- list(a=ci$ages)
  names(condlist) <- agevar
  agepred <- itsadug::get_predictions(model, cond=condlist)

  ageplot<-
     ggplot(agepred) +
     aes_string(x=agevar, y="fit") +
     # solid bold line for fitted model
     geom_line(colour="black", size=2) +
     # label plot
     ylab(yplotname) +
     xlab(xplotname)

  # individual points for actual data
  if (draw_points) ageplot <- ageplot +
     geom_point(data=modeldata, aes(y=ydata, x=agevar), alpha=.2)

  # add connecting lines if we have an idvar
  if (!is.null(idvar) && draw_points)
     ageplot <- ageplot +
        geom_line(data=d, aes_string(y=yvar, group=idvar), alpha=.2)

  # lunaize main plot
  ageplot_luna<-LNCDR::lunaize(ageplot)+
      theme(text = element_text(size=36),
            axis.title.x=element_blank(),
            axis.text.x=element_blank())

  # save to file if we have plotsavename
  g <- gam_growthrate_plot_combine(ageplot_luna, tile_luna, plotsavename)

  # give back everything we created
  return(list(tile=tile_luna, ageplot=ageplot_luna, both=g))
}


#' combine age plot and tile slop heatmap into one figure (w/grob and grid)
#'
#' @description save two figures (only use if you need to mess with titles)
#' @export
#' @param ageplot_luna     ggplot plot of subject coef by age (top part of figure)
#' @param tile_luna        tile heatmap of slope  (bottom part of figure)
#' @param PDFout           PDF name to save output into
#' @examples
#'  data <- data.frame(age=1:100,fd_mean=1:100,subj=1:25, conn_ahpc_vmpfc=randu[1:100,1])
#'  mod<-mgcv::gam(conn_ahpc_vmpfc~s(age)+s(fd_mean)+s(subj, bs="re"), data=data)
#'  ci<-LNCDR::gam_growthrate(mod, 'age', n = 10000, qnt = c(0.025, 0.975), idvar='subj')
#'  plist <- gam_growthrate_plot(data, mod, ci, 'age', idvar='subj')
#'  plist$tile <- plist$tile + xlab('AGE')
#'  g <- gam_growthrate_plot_combine(plist$ageplot, plist$tile, 'gammod.pdf')
gam_growthrate_plot_combine <- function(ageplot_luna, tile_luna, PDFout=NULL) {
  require(grid)
  require(gridExtra)

  tilegrob<- ggplotGrob(tile_luna)
  agegrob <- ggplotGrob(ageplot_luna)


  g<-rbind(agegrob, tilegrob, size="first")
  panels <- g$layout$t[grep("panel", g$layout$name)]
  g$heights[panels] <- unit(c(1, .1), "null")
  if (!is.null(PDFout))  {

     # check we are saving pdf
     ext <- rev(strsplit(PDFout, "\\.")[[1]])[1]
     if (ext != "pdf") stop(PDFout, " must end in .pdf!")

     # draw into pdf
     pdf(PDFout, height = 9, width = 12)
     grid.draw(g)
     dev.off()
  } else {
     grid.draw(g)
  }
  return(g)
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
}
