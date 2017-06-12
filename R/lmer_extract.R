#
# Author:  Brenden Tervo-Clemmens
# Packager: Will Foran
# 
# TODO: test. get car (nloptr) installed on local musl install
#' get pvalues from a model
#' @param model is a lm model
#' @param var is the variable of interest (string)
#' @param factorname is optional factor of var
#' @export
#' @examples 
#' m <- lm(model=
#' lmer_extract(m

lmer_extract<-function(model,var,factorname=NULL){
  if(is.null(factorname)){factorname=var}

  m_s<-summary(model)
  tall<-m_s$coefficients[,3]
  t.at.name<-tall[names(tall)==factorname]

  m_ca<-as.data.frame(car::Anova(model))
  cs<-m_ca$Chisq[row.names(m_ca)==var]
  p<-m_ca$'Pr(>Chisq)'[row.names(m_ca)==var]

  return(c(t.at.name,cs,p))

}

