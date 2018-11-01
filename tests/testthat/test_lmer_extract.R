library(LNCDR)
context("lmer_extract")
# test with
# cd ../; Rscript testthat.R

test_that("simple cars", {
 m <- lm(data = cars, dist ~ speed)
 p <- lmer_extract(m, "speed")
 expect_equal(unname(p),
              summary(m)$coefficients["speed","t value"])
 expect_equal(names(p),"speed.tval")
})

test_that("mixed effects", {
 m <- lme4::lmer(uptake ~ conc + Type + (1 | Treatment), CO2)
 p <- lmer_extract(m, "conc")

 expect_equal(unname(p[1]),
              summary(m)$coefficients["conc","t value"])
 expect_equal(names(p),paste(sep=".","conc",c("tval","chisq","p")))
})

test_that("mixed effects w/factor", {
 m <- lme4::lmer(uptake ~ conc + Type + (1 | Treatment), CO2)
 p <- lmer_extract(m, "conc", "TypeMississippi")
 # TODO: CHECK ME! this is weird? no?
 expect_equal(unname(p["conc.tval"]),
              summary(m)$coefficients["TypeMississippi","t value"])
})
