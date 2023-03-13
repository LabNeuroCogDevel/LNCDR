library(LNCDR)
context("age_animate")

test_that("age_animate_weight", {
  d <- data.frame(age=seq(10,30, length.out=4))
  frames_at <-  c(10,20,30)
  movdata <- age_animate_weight(d, frames_at, 2)
  # have a whole dataframe for each frame
  expect_equal(nrow(movdata), length(frames_at)*nrow(d))
  # have new columns frameage and w
  with(movdata, qassertm(frameage='n', w='n'))
})
