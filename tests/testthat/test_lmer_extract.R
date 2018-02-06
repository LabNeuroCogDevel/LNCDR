library(LNCDR)
context("lmer_extract")
# test with
# cd ../; Rscript testthat.R

test_that("simple cars", {
 m <- lm(data = cars, dist ~ speed)
 p <- lmer_extract(m, "speed")
})

test_that("mixed effects", {
 m <- lme4::lmer(uptake ~ conc + Type + (1 | Treatment), CO2)
 p <- lmer_extract(m, "conc")
})

test_that("mixed effects w/factor", {
 m <- lme4::lmer(uptake ~ conc + Type + (1 | Treatment), CO2)
 p <- lmer_extract(m, "conc", "Type")
})
