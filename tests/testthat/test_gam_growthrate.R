library(LNCDR)
context("gam_growthrate")
# test with
# cd ../; Rscript testthat.R

test_that("vars", {
  fml <- y ~ s(x) + x2 + s(x3, bs="re") + x4
  covars <- find_covars_gam(fml, "x4")
  expect_equal(covars, c("x", "x2"))

  covars <- find_covars_gam(fml, "x2")
  expect_equal(covars, c("x", "x4"))
})

test_that("deriv", {
  d <- data.frame(y=seq(1, 1000, by=10),
                  x=1:100,
                  x2=1:2,
                  id=sample(1:100),
                  f=as.factor(c("M", "F")))
  m <- mgcv::gam(y ~ s(x) + f + s(id, bs="re"), data=d)
  ci <- gam_growthrate(m, "x", "id", n.iterations=10)
  expect_equal(mean(ci$mean_dff, na.rm=T), 1)
})



test_that("rand effects factor", {
  d <- data.frame(y=seq(1, 1000, by=10),
                  x=1:100,
                  x2=1:2,
                  id=as.factor(sample(letters[1:25])),
                  f=as.factor(c("M", "F")))
  m <- mgcv::gam(y ~ s(x) + f + s(id, bs="re"), data=d)
  ci <- gam_growthrate(m, "x", "id", n.iterations=10)
  expect_equal(mean(ci$mean_dff, na.rm=T), 1)
})

test_that("flat -- no mat point", {
  d <- data.frame(y=rep(1, 1000),
                  x=1:100,
                  x2=1:2,
                  id=as.factor(sample(letters[1:25])),
                  f=as.factor(c("M", "F")))
  m <- mgcv::gam(y ~ s(x) + f + s(id, bs="re"), data=d)
  ci <- gam_growthrate(m, "x", "id", n.iterations=10)
  expect_warning(
     p <- gam_growthrate_plot(d, m, ci, "x", "id", plotsavename=NULL),
     "No maturation point!")
})

test_that("rand effects factor w/mat", {
  d <- data.frame(y=c(seq(1, 700, by=10), rep(700, 10)),
                  x=1:80,
                  x2=1:2,
                  id=as.factor(sample(letters[1:20])),
                  f=as.factor(c("M", "F")))
  m <- mgcv::gam(y ~ s(x) + f + s(id, bs="re"), data=d)
  # even number, picking near median id = p
  ci <- gam_growthrate(m, "x", "id", n.iterations=10)
  expect_warning(
     p <- gam_growthrate_plot(d, m, ci, "x", "id", plotsavename=NULL),
     NA)
})
