#
# Author:  Brenden Tervo-Clemmens
# Packager: Will Foran
# 
#' lmer_extract
#'  get (t, chisq, p) values from a single variable in a model
#' @param model is a lm model
#' @param varname is the variable of interest (string). ie. rowname in car::Anova to extract Chisq and Pr>Chisq
#' @param factorname is optional. factor of var (string) as named by summary(model), e.g. 'Female01female'.  ie. rowname in summary to extract tvalue
#' @export
#' @examples
#'   m <- lme4::lmer(uptake ~ conc + Type + (1 | Treatment), CO2)
#'   lmer_extract(m, "conc")
#'   lmer_extract(m, "Type", "TypeMississippi")

lmer_extract <- function(model, varname, factorname=NULL){
  if (is.null(factorname)) factorname <- varname

  m_s <- summary(model)
  tall <- m_s$coefficients[, 3]
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

  r <- c(tval = unname(t_at_name), chisq = cs, p = p)
  names(r) <- paste(sep = ".", varname, names(r))
  return(r)
}
