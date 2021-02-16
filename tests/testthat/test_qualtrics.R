library(LNCDR)
context("qualtrics")

test_that("can connect", {
 config <- "../../qualtrics.ini"
 known_survey <- "Parent of Youth Participant Battery_Covid-19"
 skip_if_not(file.exists(config))
 survey_list <- qualtrics_surveys(known_survey, config=config)
 expect_true(length(survey_list) >= 1)
})
