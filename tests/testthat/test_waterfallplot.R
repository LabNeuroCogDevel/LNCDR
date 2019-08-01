context("waterfall_plot")

test_that("group data", {
  d <- data.frame(age=c(10, 20, 25, 13, 14, 10),
                  id=c(100, 100, 100, 200, 200, 300))
  dg <- waterfall_group(d)
  expect_equal(dg$age_id, c(1, 1, 1, 3, 3, 2))
  expect_equal(dg$minage, c(10, 10, 10, 13, 13, 10))
})
test_that("plot", {
  d <- data.frame(age=c(10, 20, 25, 13, 14, 10),
                  id=c(100, 100, 100, 200, 200, 300))
  p <- waterfall_plot(d)
  expect_true(!is.null(p))
})
