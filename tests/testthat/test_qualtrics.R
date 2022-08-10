library(LNCDR)
context("qualtrics")

test_that("can connect", {
 skip_if_not(file.exists("~/.qualtrics.ini"))
 known_survey <- "Parent of Youth Participant Battery_Covid-19"
 survey_list <- qualtrics_surveys(known_survey)
 expect_true(length(survey_list) >= 1)
})
