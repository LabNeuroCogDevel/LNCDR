#' lm_list
#' @description build a list of lm models from group columns
#' @param d  input long format data.frame w/grouping columns specified later
#' @param formula  lm formula to model each split
#' @param ...  columns in d to split dataframe (unique groups)
#' @export
#' @examples
#'  mpg_cyl_mdls <- lm_list(mtcars, mpg~cyl, "gear", "am")
#'  mpg_cyl_mdls <- lm_list(mtcars, mpg~cyl, c("gear", "am")) # same as above
lm_list <- function(d, formula, ...) {
    cols <- c(...)
    splits_vec <-
         if(length(cols) > 1L)  do.call(paste,d[,cols])
         else d[,cols]
    lapply(split(d, splits_vec), lm, formula=formula)
}

#' lm_list_pval
#' @description get pvalues from a list of lm modles (see lm_list)
#' @param lmlist output of lm_list
#' @param ... paramters to lm_pval, namely 'coef_row' and 'pr_name'
#' @export
#' @examples
#'  mpg_cyl_mdls <- lm_list(mtcars, mpg~cyl, "gear", "am")
#'  lm_list_pval(mpg_cyl_mdls)
#           3 0         4 0         4 1         5 1 
#   0.009420695 0.040804193 0.114832314 0.009153118 
lm_list_pval <- function(lmlist, ...) {
    sapply(lmlist, lm_pval)
}

#' lm_pval
#' @description extract single pvalue from lm model. shortcut for coef(summary(m))
#' @param m lm model
#' @param coef_row what row to extract pvalue from (def=2, likely slope)
#' @param pr_name  column name for pvalue. default="Pr(>|t|)"
#' @export
#' @examples
#'  lm_pval(lm(mpg~cyl, mtcars)) # 6.112687e-10
lm_pval <- function(m, coef_row=2, pr_name="Pr(>|t|)")
    coef(summary(m))[coef_row, pr_name]
