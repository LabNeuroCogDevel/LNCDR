library(LNCDR)
context("date_match")
# test with
# cd ../; Rscript testthat.R

# example data
d.o<-data.frame(id=c(1, 2, 2, 3),
                thing=8:11,
                ymd=lubridate::ymd(c("20180910", "20171011",
                                    "20161213", "20150102")))
dm<-data.frame(id=c(1, 1, 1, 1),
               otherfield=1:4,
               ymd=lubridate::ymd(c("20180926", "20171010",
                                    "20000102", "20190905")))
dm2<-data.frame(id=c(1, 1, 2, 2),
                otherfield=1:4,
                ymd=lubridate::ymd(c("20180926", "20171010",
                                    "20000102", "20190905")))

test_that("remove missing", {
 d <- date_match(d.o, dm, "id", "ymd")
 expect_equal(nrow(d), 1)
 expect_equal(d$id, 1)
 expect_equal(d$ymd.y, lubridate::ymd("20180926"))
 expect_true(all(c("thing", "otherfield") %in% names(d)))
})

test_that("keep missing", {
 d <- date_match(d.o, dm, "id", "ymd", all.x=T)
 expect_equal(nrow(d), 4)
 expect_true(d$ymd.y[d$id==1] == lubridate::ymd("20180926"))
 expect_true(is.na(d$ymd.y[d$id==3]))
})

test_that("same matching date within max", {
 d <- date_match(d.o, dm2, "id", "ymd")
 expect_equal(nrow(d), 1)
 # both id=2 are out of max range
})

test_that("same matching date no max", {
 d <- date_match(d.o, dm2, "id", "ymd", maxdatediff=Inf, all.x=T)
 expect_equal(nrow(d), 4)
 expect_true(is.na(d$ymd.y[d$id==3]))
 # both id==2 will have the same match
})

test_that("same matching date, nanmax ", {
 d <- date_match(d.o, dm2, "id", "ymd", all.x=T)
 # have all 4 ids (we ketp all)
 expect_equal(nrow(d), 4)
 # all of 2 are too old so we NA otherfield
 expect_true(all(is.na(d$otherfield[d$id==2])))
 # but keep date and datediff
 expect_true(all(!is.na(d$ymd.y[d$id==2])))
})

test_that("error if d1$datecol1 repeats", {
 reps <- rbind(d.o,d.o)
 expect_error(date_match(reps, dm2, "id", "ymd", all.x=T),
              regexp="repeat")
})

test_that("warn about nondates", {
 d.o$notadate <- 1:nrow(d.o)
 dm$notadate2  <- 1:nrow(dm)
 expect_warning(date_match(d.o, dm, "id", "notadate", "notadate2"),
              regexp="notadate: integer.*notadate2: integer")
})

# tooo: only one dm date matching 2 do dates
