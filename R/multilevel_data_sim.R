# Author:  Brenden Tervo-Clemmens
# Packager: Will Foran

# cal error: used by either simulation method
errvar_ICCandrandomeffectvar <- function(ICC, randvar = 1) {
    errorvar <- (randvar - (ICC * randvar))/ICC
    return(data.frame(variance = errorvar, sd = sqrt(errorvar)))
}


#' sim_multilevel_bivariatenormal_randintercept 
#'
#' @description simulate multilevel bivariate normal dist with a random intercept
#' @param  nsubject        how many subjects
#' @param  nvisits         how many visits each
#' @param  interceptcor    intercept correlation
#' @param  ICCs            inter class correlation
#' @importFrom MASS mvrnorm
#' @export
#' @examples
#'   sim_multilevel_bivariatenormal_randintercept(100,2,.3,.4)
sim_multilevel_bivariatenormal_randintercept <-
   function(nsubject, nvisits, interceptcor, ICCs) {
    n <- nsubject
    timepoints <- nvisits
    time <- rep(0:(max(timepoints) - 1), times = n)
    subject <- rep(1:n, each = timepoints)

    rand_eff_corr <- matrix(c(1, interceptcor, interceptcor, 1), ncol = 2)
    random_effects <- data.frame(MASS::mvrnorm(n, mu = c(0, 0),
                                               Sigma = rand_eff_corr,
                                               empirical = FALSE))
    colnames(random_effects) <- c("Int1", "Int2")

    # set fixed effects to mu zero for sim
    intercept <- 0  # mu alpha
    slope <- 0  # mu beta
    # std dev of residual variances for indicators (time-specific error)
    sigma1 <- errvar_ICCandrandomeffectvar(ICC = ICCs[1], randvar = var(random_effects$Int1))
    # compute based on desired ICC fixed intercept + (random) subject-specific
    # intercept deviation + fixed slope + level-1 error aka measurement error
    outcome1 <- (intercept + random_effects$Int1[subject]) +
                (slope) +
                rnorm(n * timepoints, mean = 0, sd = sigma1$sd)

    # std dev of residual variances for indicators (time-specific error)
    sigma2 <- errvar_ICCandrandomeffectvar(ICC = ICCs[2], randvar = var(random_effects$Int2))

    # fixed intercept + (random) subj-specific intercept deviation + fixed slope
    # + #level-1 error aka measurement error
    outcome2 <- (intercept + random_effects$Int2[subject]) +
                (slope) +
                rnorm(n * timepoints, mean = 0, sd = sigma2$sd)

    d <- data.frame(subject, time, outcome1, outcome2)
    return(d)
}



#' sim_multilevel_bivariatenormal_randinterceptandslope
#'
#' @description simulate multilevel bivariate normal dist with a random intercept AND SLOPE
#' @param  nsubject           how many subjects
#' @param  nvisits            how many visits each
#' @param  interceptcor       intercept correlation
#' @param  slopecor           slope correlation
#' @param  slopeintcorbetween slope between subject correlation
#' @param  slopeintcorwithin  slope within subject correlation
#' @param  ICCs               inter class correlation
#' @importFrom Mass mvrnorm
#' @export
#' @examples
#'  sim_multilevel_bivariatenormal_randinterceptandslope(100,2,.1,.5,.6,.3,.4)
sim_multilevel_bivariatenormal_randinterceptandslope <-
   function(nsubject, nvisits, interceptcor, slopecor,
            slopeintcorbetween, slopeintcorwithin, ICCs) {
    n <- nsubject
    timepoints <- nvisits
    time <- rep(0:(max(timepoints) - 1), times = n)
    subject <- rep(1:n, each = timepoints)

    rand_eff_corr <-
       matrix(c(1, slopeintcorwithin, slopecor, slopeintcorbetween,
                slopeintcorwithin, 1, slopeintcorbetween, interceptcor,
                slopecor, slopeintcorbetween, 1, slopeintcorwithin,
                slopeintcorbetween, interceptcor, slopeintcorwithin, 1),
              ncol = 4)

    random_effects <- data.frame(MASS::mvrnorm(n, mu = c(0, 0, 0, 0),
                                               Sigma = rand_eff_corr,
                                               empirical = FALSE))
    colnames(random_effects) <- c("Slope1", "Int1", "Slope2", "Int2")

    # set fixed effects to mu zero for sim
    intercept <- 0  # mu alpha
    slope <- 0  # mu beta
    # std dev of residual variances for indicators (time-specific error)
    sigma1 <- errvar_ICCandrandomeffectvar(ICC = ICCs[1],
                                           randvar = var(random_effects$Int1))
    # compute based on desired ICC fixed intercept + (random) subject-specific
    # intercept deviation fixed slope + (random) subj-specific slope deviation
    # level-1 error aka measurement error
    outcome1 <- (intercept + random_effects$Int1[subject]) +
                (slope + random_effects$Slope1[subject]) +
                rnorm(n * timepoints, mean = 0, sd = sigma1$sd)

    # std dev of residual variances for indicators (time-specific error)
    sigma2 <- errvar_ICCandrandomeffectvar(ICC = ICCs[2],
                                           randvar = var(random_effects$Int2))
    # fixed intercept + (random) subj-specific intercept deviation fixed slope +
    # (random) subj-specific slope deviation level-1 error aka measurement error
    outcome2 <-
       (intercept + random_effects$Int2[subject]) +
       (slope + random_effects$Slope2[subject]) +
       rnorm(n * timepoints, mean = 0, sd = sigma2$sd)

    d <- data.frame(subject, time, outcome1, outcome2)
    return(d)
}

#' retentionadjustvisits
#'
#' @description simulate multilevel bivariate normal dist with a random intercept
#' @param  nsubject             how many subjects
#' @param  nvisits              how many visits each
#' @param  retention_or_total   ratio of retained (0<x<1) or total (x>=1)
#' @export
#' @examples
#' retentionadjustvisits(100,2,.8)  # using retention
#' retentionadjustvisits(100,2,160) # using total
retentionadjustvisits <- function(nsubject, nvisits, retention_or_total) {
    if (retention_or_total < 1) {
        retentionadjust <- rate_or_total
        print(sprintf("%s total visits based on %s retention rate",
                      adjustedtotalvisits, retention))
    } else {
        totalvisits <- rate_or_total
        print(sprintf("%s total visits requested", totalvisits))
        retentionadjust <- totalvisits/ (nsubject * nvisits)
        print(sprintf("calculated retention rate is %s", retentionadjust))
    }
    adjustedtotalvisits <- round(retentionadjust * (nsubject * nvisits),
                                 digits = 0)
    return(c(retentionadjust, adjustedtotalvisits))
}

