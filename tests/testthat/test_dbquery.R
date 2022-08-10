library(LNCDR)
context("db_query")
# test with
# cd ../; Rscript testthat.R

dbipath <- function()
    system("perl -MURI::Encode=uri_encode -slanF: -e 'if(/lncddb/){print \"postgres://$F[3]:\",uri_encode($F[4], { encode_reserved => 1 }), \"\\@$F[0]:$F[1]/$F[2]\"; exit}' ~/.pgpass",intern=TRUE)
db_exists <- function()
   file.exists("~/.pgpass") &&
   any(grepl("lncddb", readLines("~/.pgpass"))) &&
    system(paste0("psql ", dbipath(), " -c 'select pid from enroll limit 1'"), timeout=5) == 0

test_that("can query", {
 skip_if_not(db_exists())
 d <- db_query("select study from study")
 expect_true("CogR01" %in% d$study)
})

test_that("close connection if not provided", {
 # previously if not closed got error
 # RS-DBI driver: (@cannot allocate a new connection -- maximum of 16 connections ...
 skip_if_not(db_exists())
 for (i in 1:100) {
    d <- db_query("select study from study")
    expect_true("CogR01" %in% d$study)
 }
})
