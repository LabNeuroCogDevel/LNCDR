library(LNCDR)
context("xlsx_date")

test_that("parse dates", {
    expect_equal(xlsx_date(c("41977","42068",NA)),
                  c("2014-12-04","2015-03-05",NA))
})

test_that("outofrange", {
    expect_error(xlsx_date(c("41977","12068")), ".*outside of range.*")
    expect_error(xlsx_date(c("41977"), minyear=2015), ".*outside of range.*")
    expect_error(xlsx_date(c("41977"), maxyear=2013), ".*outside of range.*")
})
