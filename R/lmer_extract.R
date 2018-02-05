#
# Author:  Brenden Tervo-Clemmens
# Packager: Will Foran
# 
#' lmer_extract
#'  get (t, chisq, p) values from a single variable in a model
#' @param model is a lm model
#' @param varname is the variable of interest (string)
#' @param factorname is optional factor of var
#' @export
#' @examples
#'   m <- lme4::lmer(uptake ~ conc + Type + (1 | Treatment), CO2)
#'   lmer_extract(m, "conc")

lmer_extract <- function(model, varname, factorname=NULL){
  if (is.null(factorname)) factorname <- varname

  m_s <- summary(model)
  tall <- m_s$coefficients[, 3]
  t_at_name <- tall[names(tall) == factorname]

  m_ca <- as.data.frame(car::Anova(model))
  cs   <- m_ca$Chisq[row.names(m_ca) == varname]
  p    <- m_ca$"Pr(>Chisq)"[row.names(m_ca) == varname]

  r <- c(tval = unname(t_at_name), chisq = cs, p = p)
  names(r) <- paste(sep = ".", varname, names(r))
  return(r)
}
