errorvar_fromICCandrandomeffectvar<-function(ICC,randvar=1){
  errorvar=(randvar-(ICC*randvar))/ICC
  return(data.frame(variance=errorvar,sd=sqrt(errorvar)))
}
retentionadjustvisits<-function(nsubject,nvisits,retentionrate,totalvisits=NULL){
  if (is.null(totalvisits)){
    retentionadjust=retentionrate
    adjustedtotalvisits<-round(retentionadjust*(nsubject*nvisits), digits = 0)
    print(sprintf("%s total visits based on %s rention rate",adjustedtotalvisits,retention))
  }else{
    print(sprintf("%s total visits requested",totalvisits))
    retentionadjust=totalvisits/(nsubject*nvisits)
    print(sprintf("calculated retention rate is %s",retentionadjust))
    adjustedtotalvisits<-round(retentionadjust*(nsubject*nvisits), digits = 0)
  }
  return(c(retentionadjust,adjustedtotalvisits))
}
sim_multilevel_bivariatenormal_randintercept<-function(nsubject,nvisits,interceptcor,ICCs){
  n = nsubject 
  timepoints = nvisits
  time = rep(0:(max(timepoints)-1), times=n)
  subject = rep(1:n, each=timepoints)
  
  randomEffectsCorr = matrix(c(1,interceptcor,interceptcor,1),ncol=2)
  randomEffects = MASS::mvrnorm(n, mu=c(0,0), Sigma = randomEffectsCorr, empirical=FALSE) %>% data.frame()
  colnames(randomEffects) = c('Int1', 'Int2')
  
  #set fixed effects to mu zero for sim
  intercept = 0 # mu alpha
  slope = 0 # mu beta
  sigma1 = errorvar_fromICCandrandomeffectvar(ICC=ICCs[1],randvar=var(randomEffects$Int1)) # standard deviation of residual variances for indicators (time-specific error)
  #compute based on desired ICC
  # fixed intercept + (random) subject-specific intercept deviation + fixed slope  +  level-1 error aka measurement error
  outcome1 = (intercept + randomEffects$Int1[subject]) + 
    (slope)+
    rnorm(n*timepoints, mean=0, sd=sigma1$sd)
  
  sigma2 = errorvar_fromICCandrandomeffectvar(ICC=ICCs[2],randvar=var(randomEffects$Int2)) # standard deviation of residual variances for indicators (time-specific error)

  # fixed intercept + (random) subject-specific intercept deviation + # fixed slope + #level-1 error aka measurement error
  outcome2 = (intercept + randomEffects$Int2[subject]) +
    (slope)+
    rnorm(n*timepoints, mean=0, sd=sigma2$sd) 
  
  d = data.frame(subject, time, outcome1,outcome2)
  return(d)
}
sim_multilevel_bivariatenormal_randinterceptandslope<-function(nsubject,nvisits,interceptcor,slopecor,slopeintcorbetween,slopeintcorwithin,ICCs){
  n = nsubject 
  timepoints = nvisits
  time = rep(0:(max(timepoints)-1), times=n)
  subject = rep(1:n, each=timepoints)

  randomEffectsCorr = matrix(c(1,slopeintcorwithin,slopecor,slopeintcorbetween,slopeintcorwithin,1,slopeintcorbetween,interceptcor,
                               slopecor,slopeintcorbetween,1,slopeintcorwithin,slopeintcorbetween,interceptcor,slopeintcorwithin,1),ncol=4)
  
  randomEffects = MASS::mvrnorm(n, mu=c(0,0,0,0), Sigma = randomEffectsCorr, empirical=FALSE) %>% data.frame()
  colnames(randomEffects) = c('Slope1', 'Int1',"Slope2","Int2")
                              
  #set fixed effects to mu zero for sim
  intercept = 0 # mu alpha
  slope = 0 # mu beta
  sigma1 = errorvar_fromICCandrandomeffectvar(ICC=ICCs[1],randvar=var(randomEffects$Int1)) # standard deviation of residual variances for indicators (time-specific error)
  #compute based on desired ICC
   # fixed intercept + (random) subject-specific intercept deviation
   # fixed slope + (random) subject-specific slope deviation
   #level-1 error aka measurement error
  outcome1 = (intercept + randomEffects$Int1[subject]) + 
    (slope + randomEffects$Slope1[subject]) +
    rnorm(n*timepoints, mean=0, sd=sigma1$sd) 
  
  sigma2 = errorvar_fromICCandrandomeffectvar(ICC=ICCs[2],randvar=var(randomEffects$Int2)) # standard deviation of residual variances for indicators (time-specific error)
  # fixed intercept + (random) subject-specific intercept deviation
  # fixed slope + (random) subject-specific slope deviation
  #level-1 error aka measurement error
  outcome2 = (intercept + randomEffects$Int2[subject]) + 
    (slope + randomEffects$Slope2[subject]) +
    rnorm(n*timepoints, mean=0, sd=sigma2$sd)
  
  d = data.frame(subject, time, outcome1,outcome2)
  return(d)
}
