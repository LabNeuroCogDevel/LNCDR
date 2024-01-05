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
#' @importFrom gratia derivatives
#' @export
#' @examples
#'  d <- read.csv("tests/btc_r03_scored.csv") %>%
#'    group_by(id) %>%
#'    mutate(visit=rank(d8))
#'  f <- f1score ~ s(Ageatvisit) + s(visit) + s(id, bs="re")
#'  m <- mgcv::gam(f, data=d)
#'  ci <- gam_growthrate(m, 'Ageatvisit', 'id')
#'  m <- gamm4::gamm4(f, data=d)
#'  ci <- gam_growthrate(m, 'Ageatvisit', 'id')
gam_growthrate <- function(m, agevar, idvar=NULL, qnt=.975) { # TODO: interval_inc
  m <- gam_extract_model(m) # gam 'model' is within list, mgcv has model at top

  varying_list <- list(.1); names(varying_list) <- agevar # eg. list(age=.1)
  age_step_df <- gen_predict_df(m, varying_list)

  # gamm4  'from' must be a finite number; mgcv: random effect lunaid not found
  f1deriv <- gratia::derivatives(m,
                               term=agevar,
                               partial_match=TRUE, # BTC: agrep?, doesn't matter here
                               interval="simultaneous",
                               level=qnt,
                               type="backward", # NA was first in original code
                               data=age_step_df)
 # f1deriv
 #   smooth        var        by_var fs_var  data derivative     se  crit lower upper
 #   <chr>         <chr>      <chr>  <chr>  <dbl>      <dbl>  <dbl> <dbl> <dbl> <dbl>
 # 1 s(Ageatvisit) Ageatvisit <NA>   <NA>    8.01      0.374 0.0615  3.27 0.173 0.575
 # 2 s(Ageatvisit) Ageatvisit <NA>   <NA>    8.11      0.374 0.0615  3.27 0.173 0.57
  #names(f1deriv) <- c("smooth","term","ages","mean_dff","se","crit","ci_low","ci_high")
                   # c("smooth", "var", "by_var", "fs_var", "data", "derivative","se", "crit", "lower", "upper")
  return(f1deriv)
}

#' gen_predict_df dataframe of model with variable(s) in fixed steps. use for prediction or model plotting
#' @param m gam model
#' @param varying list(coluname=stepsize, col2=step2, ...) default list(age=.1)
#' @export
gen_predict_df <- function(m, varying=list(age=.1)){
   have_varying <- names(varying) %in% names(m$model)
   if(!all(have_varying)) {
      cat("MISSING: not in model but want to vary ",
          paste(collapse=", ", names(varying)[have_varying]),
          "\n")
      stop('creating prediciton df: all columns to vary must be in model!')
   }

   # create dataframe with each step of each predition needed
   # can get very large if many varying columns with small interval
   vars_list <-
      mapply(function(colname,interval)
                seq(min(m$model[,colname],na.rm=T),
                    max(m$model[,colname],na.rm=T),
                    by=interval),
             names(varying), varying, SIMPLIFY=F)
   pred_at_df <- do.call(expand.grid,vars_list)

   # add missing
   lhs_terms <- labels(terms(m))
   center_terms <- setdiff(lhs_terms, names(varying))

   # for factors, use the row closest to the median of the first varying column
   # TODO: euclidian distand of each varying median?
   median_of <- names(varying)[1]
   first_var_vals <- m$model[,median_of]
   median_of_first <- median(first_var_vals)
   middle_idx <- which.min(abs(median_of_first -first_var_vals))

   # intertiavely build each column by mean
   for(colname in center_terms){
      if(is.null(colname)) next
      #colclass <- attr(terms(m),'dataClass')[colname])
      colclass <- class(m$model[,colname])
      if(any(c("integer","numeric") %in% colclass)) {
          center <- mean(m$model[,colname],na.rm=T)
          # ugly to print something for each column
          # but likely this doing something unintended
          cat(glue::glue("# column '{colname}' (type '{colclass}') centered at {center}"),"\n")
      } else {
         center <- m$model[middle_idx,colname]
         warning(sprintf("non-numeric '%s' column! set all %s=%s (closet to center %s@%.2f)",
                         colclass, colname, center, median_of,median_of_first))
      }
      pred_at_df[,colname] <- center
   }
   return(pred_at_df)
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

too_small <- function(x) abs(x) < 10^-15
#' add sig column:  0 not significant; 1 is signficiant
is_sig <- function(ci){ # 20231205 now for gratia
  # if confidence interval includes zero
  # signs of x and y will be different, -x * +y  < 0
  # or if both high and low are extremly close to zero
  not_sig <- ci$lower * ci$upper < 0 |
    (too_small(ci$lower) & too_small(ci$upper))
  return(!not_sig)
  #ci$sig <- 1
  #ci$sig[not_sig] <- 0
  #return(ci)
}


#' gam_maturation_point
#' 
#' @description get maturation point from confidence interval dataframe
#' @param ci     growthrate_gam output (confidence interval and derivitive)
#' @export
gam_maturation_point <- function(ci) {
  # ci has cols: smooth, var, data, derivative, se, crit, lower, upper

  # when ci bounds include 0 (different sign), no longer signficant
  # clip out insignificant derivitive
  if (is.na(ci$lower[1])) ci <- ci[-1, ]

  # get mean_df_clip column
  if (! "sig" %in% names(ci)) ci$sig <- is_sig(ci)

  # find maturation point after the first signficant age
  onset_sig <- ci$data[ci$sig]
  maturation_pnt <- NA
  if (length(onset_sig)>0L && !all(is.na(onset_sig))) {
     mat_points_idx <- !ci$sig & ci$data > onset_sig[1]
     if (length(mat_points_idx) > 0L && any(mat_points_idx))
        maturation_pnt <- min(ci$data[mat_points_idx], na.rm=T)
  }

  return(maturation_pnt)
}


#' plot gam factor with deriv
#'
#' @description plot output of growthrate_gam and list for gam_growthrate_plot_combine
#' @param d      dataframe model was built on (for actual points)
#' @param model  gam model (for predicted line)
#' @param ci     growthrate_gam output (confidence interval and derivitive)
#' @param agevar column name of age var e.g. 'Ageatvisit'
#' @param idvar  line grouping var e.g., 'lunaid', set to NULL if no random effect in model
#' @param yvar   model yvar e.g. 'f1score', default pulled from model formula
#' @param plotsavename PDF output name e.g. 'growth.pdf', not saved when NA, not ploted when NULL
#' @param xplotname 'Age'
#' @param yplotname  'f1score', default is yvar (model yvar)
#' @param draw_maturation T|F, show dotted line on first maturation point
#' @param draw_points T|F, show individual points as scatter plot over gam fit line
#' @param show_all_fill T|F, should we clip the raster fill to only significant ages?
#' @param ci_plot T|F, plot 95 percent confidence interval with geom_ribbon?
#' @param theme_func what theme to apply, default theme_bw. also see 'gam_plot_raster_theme'
#' @param font_size, size of 
#' @export
#' @importFrom itsadug get_predictions
#' @examples
#'  # no random effects
#'  m <- mgcv::gam(f1score ~ s(Ageatvisit), data=d)
#'  ci <- gam_growthrate(m, 'Ageatvisit')
#'  gam_growthrate_plot(d, m, ci, 'Ageatvisit')
#'
#'  # w/random effects 'id'
#'  m <- mgcv::gam(f1score ~ s(Ageatvisit) + s(visit) + s(id, bs="re"), data=d)
#'  ci <- gam_growthrate(m, 'Ageatvisit')
#'  gam_growthrate_plot(d, m, ci, 'Ageatvisit', 'id')
#'
#'  # replot example, see gam_growthrate_plot_combine
#'  mod <- mgcv::gam(conc~s(uptake), data=CO2)
#'  ci <- LNCDR::gam_growthrate(mod, 'uptake', n = 10000, qnt = c(0.025, 0.975))
#'  plist <- gam_growthrate_plot(cars, mod, ci, 'uptake', xplotname='uptake')
#'  plist$ageplot <- plist$ageplot + xlab('foobar')
#'  gam_growthrate_plot_combine(plist$ageplot, plist$tile)
gam_growthrate_plot <-
   function(d, model, ci, agevar, idvar=NULL,
            yvar=as.character(model$formula[2]),
            plotsavename=NA, xplotname="Age", yplotname=yvar,
            draw_maturation=T, draw_points=T, show_all_fill=F,
            ci_plot=T, theme_func=theme_bw, fontsize=36){

  require(ggplot2)
  require(itsadug)

  # TODO:
  # remove or replace first row mean_dff
  #   NA draws weird first color on spectrum

  # make sure we have what we say we want
  if (! "gam" %in% class(model) ) stop("model given must be a gam model!")
  if (! "data.frame" %in% class(d) ) stop("d given must be a data.frame!")
  if (! "data.frame" %in% class(ci) ) stop("ci is not growthrate_gam() output")
  if (! yvar %in% names(model$model) ) stop(yvar, "not in model dataframe!")

  # when ci bounds include 0 (different sign), no longer signficant
  ci$sig <- is_sig(ci)
  maturation_pnt <- gam_maturation_point(ci)

  # warn about no matruation point
  if (is.na(maturation_pnt) && draw_maturation) {
     warning("No maturation point!")
     draw_maturation <- F
  }

  # show even unsignficant change in raster if show_all_fill
  # was mean_dff vs mean_dff_clip, now derivative or clip (TODO: not ci$data?)
  fill_column <- "derivative"
  if(!show_all_fill) {
      fill_column <- "deriv_clip"
      ci$deriv_clip <- ci$derivative
      ci$deriv_clip[!ci$sig] <- 0 # should be NA? will plot as grey in tile
   }

  ## setup derivitive raster plot
  deriv_range <- range(ci[,fill_column], na.rm=T)
  tile <-
     ggplot(ci[-1, ]) + # don't plot first row (is NA)
     aes(x=data, y=1, fill=!!sym(fill_column)) +
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
      geom_segment( # TODO: modify NA to clear grey as option
          linetype=2, colour="black",
          aes(x=maturation_pnt, xend=maturation_pnt, y=.5, yend=1.5))

  # styleize theme
  tile_themed <- tile + theme_func() + gam_plot_raster_theme(text = element_text(size=fontsize))

  # predictions
  modeldata<-data.frame(ydata=model$y, agevar=model$model[, agevar])
  condlist <- list(a=ci$data)
  names(condlist) <- agevar
  # 20190826 BTC - remove random effects (bug fix)
  agepred <- itsadug::get_predictions(model, cond = condlist, rm.ranef=TRUE)

  ageplot<-
     ggplot(agepred) +
     aes(x=!!sym(agevar), y=fit) +
     # solid bold line for fitted model
     geom_line(colour="black", linewidth=2) +  # TODO: change me
     # label plot
     ylab(yplotname) +
     xlab(xplotname)

  if (ci_plot) {
     ageplot <- ageplot +
       geom_ribbon(aes(ymin=fit - CI, ymax=fit + CI), alpha=.3)
  }

  # individual points for actual data
  if (draw_points) ageplot <- ageplot +
     geom_point(data=modeldata, aes(y=ydata, x=agevar), alpha=.2)

  # add connecting lines if we have an idvar
  if (!is.null(idvar) && draw_points)
     ageplot <- ageplot +
        geom_line(data=d, aes(y=!!sym(yvar), group=!!sym(idvar)), alpha=.2)

  # lunaize main plot
  ageplot_luna<- ageplot + theme_func() + theme(
            text = element_text(size=fontsize),
            axis.title.x=element_blank(),
            axis.text.x=element_blank())

  # save to file if we have plotsavename
  g <- gam_growthrate_plot_combine(ageplot_luna, tile_themed, plotsavename)

  list_of_plots <- list(tile=tile_themed, ageplot=ageplot_luna, both=g)
  # give back everything we created
  return(list_of_plots)
}


#' combine age plot and tile slop heatmap into one figure (w/grob and grid)
#'
#' @description save two figures (only use if you need to mess with titles)
#' @export
#' @param ageplot_luna     ggplot plot of subject coef by age (top part of figure)
#' @param tile_luna        tile heatmap of slope  (bottom part of figure)
#' @param PDFout           PDF name to save output into, NA no saved, NULL not plotted
#' @examples
#'  data <- data.frame(age=1:100,fd_mean=1:100,subj=as.factor(letters[1:25]), conn_ahpc_vmpfc=randu[1:100,1])
#'  mod<-mgcv::gam(conn_ahpc_vmpfc~s(age)+s(fd_mean)+s(subj, bs="re"), data=data)
#'  ci<-LNCDR::gam_growthrate(mod, 'age', n = 10000, qnt = c(0.025, 0.975), idvar='subj')
#'  plist <- gam_growthrate_plot(data, mod, ci, 'age', idvar='subj')
#'  plist$tile <- plist$tile + xlab('AGE')
#'  g <- gam_growthrate_plot_combine(plist$ageplot, plist$tile, 'gammod.pdf')
gam_growthrate_plot_combine <- function(ageplot_luna, tile_luna, PDFout=NA) {
  require(grid)
  require(gridExtra)

  tilegrob<- ggplotGrob(tile_luna)
  agegrob <- ggplotGrob(ageplot_luna)


  g<-rbind(agegrob, tilegrob, size="first")
  panels <- g$layout$t[grep("panel", g$layout$name)]
  g$heights[panels] <- unit(c(1, .1), "null")

  # NULL is no draw
  # NA is draw to screen
  # filename is save to pdf
  if (is.null(PDFout)){
     return(g)
  } else if (is.na(PDFout))  {
     grid.draw(g)
  } else {

     # check we are saving pdf
     ext <- rev(strsplit(PDFout, "\\.")[[1]])[1]
     if (ext != "pdf") stop(PDFout, " must end in .pdf!")

     # draw into pdf
     pdf(PDFout, height = 9, width = 12)
     grid.draw(g)
     dev.off()
  }
  return(g)
}

#' gam_plot_raster_theme removes panel and axis ticks and legend
#' @export
gam_plot_raster_theme <- function(...) {
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y     = element_blank(),
    axis.ticks.y     = element_blank(),
    axis.text.y      = element_blank(),
    legend.position  = "none", ...)
}

#' gam_extract_model abstract over gamm4 and mgcv gam models
#' @param m  either a mgcv::gam or a gamm4::gamm4 model
#' @return model componet (gamm4$gam) or pass through
gam_extract_model <- function(m) {
   # class(mgcv::gam(...)) # "gam" "glm" "lm"
   if("gam" %in% class(m)) return(m)

   # m <- gamm4::gamm4(data=d, random=~(1|lunaid), value ~ s(age,k=5) + hemi)
   # class(gamm4::gamm4(...)) # 
   if("list" %in% class(m) && "gam" %in% names(m)) return(m$gam)

   if(! "model" %in% names(m)) stop("input model not gam, does not have model, cannot use!")
   return(m)
}

mgcvgampreddata<-function(model,predvar,idvar=NULL,interval_inc=.1,varycovarsname=NULL,varycovarlevels=NULL){
 # if (class(model)[1]=="gamm" || (class(model)=="list" && "gam" %in% class(model[['gam']]))){
 #   model<-model$gam
 # }
  # TODO:gamm4 vs mgcv
  modeldata<-data.frame(ydata=model$y, predvar=model$model[, predvar])
  preddata<-data.frame(var=seq(min(modeldata$predvar,na.rm=T),
                               max(modeldata$predvar,na.rm=T),
                               by=interval_inc))
  names(preddata)[1]<-predvar
  if (!identical(find_covars_gam(model$formula, predvar),character(0))){
   # if (!(length(find_covars_gam(model$formula, predvar))==1 && find_covars_gam(model$formula, predvar)[1]==varycovarsname)){
    for (cv in find_covars_gam(model$formula,predvar)){
      x <- model$model[, cv]
      if (is.character(x) || is.factor(x)){
        warning("gam w/character or factor covar, setting all sim to the first obs for character/first level if factor")
        y <- x[1]
        if (class(x)=="factor"){y<-levels(x)[1]}
        print(sprintf("covar % set to level %s",cv,y))
      } else {
        y <- mean(x, na.rm=T)
      }
      preddata[, cv] <- y
    }
    #}
  }else if(is.null(varycovarsname)){
    preddata$cov<-"no cov"
  }

  #if (!(length(find_covars_gam(model$formula, predvar))==1 && find_covars_gam(model$formula, predvar)[1]==varycovarsname)){
  names(preddata)<-c(predvar,find_covars_gam(model$formula, predvar))
  #}
  if (identical(find_covars_gam(model$formula, predvar),character(0)) && is.null(varycovarsname)){
    names(preddata)<-c(predvar,"nullcovar")
  }

  if (!is.null(varycovarsname)){
    require(reshape)
    orignameorder<-names(preddata)
    preddata[,varycovarsname]<-NULL
    preddata<-reshape::expand.grid.df(preddata,data.frame(varycovar=varycovarlevels))
    names(preddata)[names(preddata)=="varycovar"]<-varycovarsname
    #preddata<-preddata[,orignameorder]
  }

  yhats <- predict(model,preddata,se.fit=TRUE)
  preddata<-cbind(preddata,yhats$fit,yhats$se.fit)
  names(preddata)<-c(predvar,find_covars_gam(model$formula, predvar),"fit","se")
  if (identical(find_covars_gam(model$formula, predvar),character(0)) && is.null(varycovarsname)){
    names(preddata)<-c(predvar,"nullcovar","fit","se")
  }else if(identical(find_covars_gam(model$formula, predvar),character(0)) && !is.null(varycovarsname)){
    names(preddata)<-c(predvar,varycovarsname,"fit","se")
  }
  preddata$CI<-2*preddata$se
  return(preddata)
}
