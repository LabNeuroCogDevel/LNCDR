library(LNCDR)
context("db_query")
# test with
# cd ../; Rscript testthat.R


test_that("can query", {
 skip_if_not(file.exists("~/.pgpass") &&
             any(grepl("lncddb", readLines("~/.pgpass"))))
 d <- db_query("select study from study")
 expect_true("CogR01" %in% d$study)
})

test_that("close connection if not provided", {
 # previously if not closed got error
 # RS-DBI driver: (@cannot allocate a new connection -- maximum of 16 connections ...
 skip_if_not(file.exists("~/.pgpass") &&
             any(grepl("lncddb", readLines("~/.pgpass"))))
 for (i in 1:100) {
    d <- db_query("select study from study")
    expect_true("CogR01" %in% d$study)
 }
})
