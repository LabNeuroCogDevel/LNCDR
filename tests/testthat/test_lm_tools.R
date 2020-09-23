library(LNCDR)
context("lm_utils")

pval3_0 <- 0.009420695
test_that("lm_pval: mtcars group:gearXam", {
  cars3_0 <- subset(datasets::mtcars, gear==3 & am==0)
  m3_0 <- lm(mpg~cyl, cars3_0)
  expect_equivalent(lm_pval(m3_0), pval3_0)
})

test_that("lm_list_pval: mtcars group:gearXam", {
  mpg_cyl_mdls <- lm_list(datasets::mtcars, mpg~cyl, c("gear", "am")) # same as above
  pvals <- lm_list_pval(mpg_cyl_mdls)
  expect_equivalent(pvals["3 0"], pval3_0)
})
