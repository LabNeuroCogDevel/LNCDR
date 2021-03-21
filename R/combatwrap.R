#
# Author:  Brenden Tervo-Clemmens
# Packager: Will Foran
# 
# 20201218WF - init upload
# 20210321WF - small cleanup

#' haromize: wrap around neuroCombat
#' @param d dataframe
#' @param combatcols columns to use from d
#' @param batchvar str column name TODO: describe
#' @param covars   str column names TODO: describe
#' @param eb  TODO: describe
#' @importFrom neuroCombat neuroCombat
#' @export
#' @examples 
#' # TODO: provide example
combatwrap <- function(d, combatcols, batchvar, covars, eb){
  require(neuroCombat)
  # cannot run with NAs
  d <- d[complete.cases(d[, c(combatcols, batchvar, covars)]), ]
  dati <- t(as.matrix(d[, combatcols]))
  batchi <- as.factor(d[, batchvar])

  # model matrix wants a formula like
  #   ~sex+covar2+....
  covarchars <- paste(unlist(lapply(covars, as.character)), collapse="+")
  fml <- as.formula(sprintf("~%s", covarchars))
  modi <- model.matrix(fml, data=d)

  data.harmonized <- neuroCombat::neuroCombat(dat=dati, batch=batchi, mod=modi, eb=eb)
  dft <- t(data.harmonized$dat.combat)
  colnames(dft) <- paste0("combat_", colnames(dft))
  dfharmonized <- cbind(d, dft)
}
