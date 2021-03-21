#
# Author:  Brenden Tervo-Clemmens
# Packager: Will Foran
# 
#' lmer_extract
#'  get (t, chisq, p) values from a single variable in a model
#'  ONLY USE WHERE FACTOR has more than two levels -- chisq will report without any factor instead of without the specified factor
#' @param model is a lm model
#' @param varname is the variable of interest (string). ie. rowname in car::Anova to extract Chisq and Pr>Chisq
#' @param factorname is optional. factor of var (string) as named by summary(model), e.g. 'Female01female'.  ie. rowname in summary to extract tvalue
#' @importFrom car Anova
#' @export
#' @examples
#'   m <- lme4::lmer(uptake ~ conc + Type + (1 | Treatment), CO2)
#'   lmer_extract(m, "conc")
#'   lmer_extract(m, "Type", "TypeMississippi")

lmer_extract <- function(model, varname, factorname=NULL){

  m_s <- summary(model)

  # make sure we have t value -- otherwise we'd spit out incorrect info
  tcolname = dimnames(m_s$coefficients)[[2]][3]
  # maybe should test class(model) %in% c("lm", "lmerMod")
  if(tcolname != "t value")
      stop("lmer_extract expects summary coefficents column 3 to be 't value' but have ", tcolname,
           ". Your model should be built with lme4::lmer")

  if (is.null(factorname)) factorname <- varname

  # [,3] == [,"t value"]
  tall <- m_s$coefficients[, "t value"]
  t_at_name <- tall[names(tall) == factorname]

  # warn if t_at_name does not exist
  if (length(t_at_name) == 0L) {
     warning(
      sprintf('no tval returned! Factor name "%s" not in model output: %s',
         factorname,
         paste(sep=", ", collapse=", ", names(tall))))
  }

  m_ca <- as.data.frame(car::Anova(model))
  cs   <- m_ca$Chisq[row.names(m_ca) == varname]
  p    <- m_ca$"Pr(>Chisq)"[row.names(m_ca) == varname]

  # todo: error out if factorname and more than two catagories
  #       or warn and drop tvalue
  r <- c(tval = unname(t_at_name), chisq = cs, p = p)
  names(r) <- paste(sep = ".", varname, names(r))
  return(r)
}
