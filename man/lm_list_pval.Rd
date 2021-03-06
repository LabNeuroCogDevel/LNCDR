% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lm_tools.R
\name{lm_list_pval}
\alias{lm_list_pval}
\title{lm_list_pval}
\usage{
lm_list_pval(lmlist, ...)
}
\arguments{
\item{lmlist}{output of lm_list}

\item{...}{paramters to lm_pval, namely 'coef_row' and 'pr_name'}
}
\description{
get pvalues from a list of lm modles (see lm_list)
}
\examples{
 mpg_cyl_mdls <- lm_list(mtcars, mpg~cyl, "gear", "am")
 lm_list_pval(mpg_cyl_mdls)
}
