## ReadR Tests ##
#' @importFrom testthat test_that expect_equal expect_error
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @importFrom pkgload system.file
#'

# if(!require(testthat)){
#   install.packages("testthat")
#   library(testthat)
# }
#
#
# if(!require(tidyr)){
#   install.packages("tidyr")
#   library(tidyr)
# }

#use_data(testData)
#use_data(testDataShort)
#use_data(testDataLong)
#use_data(testDataLongShort)
#use_data(testDataLongErrorSNA)
#use_data(testDataLongErrorTNA)
#use_data(testDataLongErrorTCH)
#use_data(testDataLongErrorHNA)
#use_data(testDataLongErrorHCH)
#use_data(expOutput)


#testData <- read.csv("./data/testData.csv")
#testDataShort <- read.csv("./data/testDataShort.csv")
#testDataLong <- pivot_longer(testData, cols = c("rep1", "rep2", "rep3", "rep4", "rep5", "rep6", "rep7", "rep8", "rep9", "rep10"), names_to = "rep", values_to = "time")
#testDataLongShort <- testDataLong[c(-145),]
#write.csv(testDataLong, "./data/longTestData.csv")
#testDataLongErrorSNA <- read.csv("./data/longTestDataErrorStreamMissing.csv")
#testDataLongErrorTNA <- read.csv("./data/longTestDataErrorMissingTime.csv")
#testDataLongErrorTCH <- read.csv("./data/longTestDataErrorCharTime.csv")
#testDataLongErrorHNA <- read.csv("./data/longTestDataErrorMissingHour.csv")
#testDataLongErrorHCH <- read.csv("./data/longTestDataErrorCharHour.csv")
#expOutput <- testDataLong
#colnames(expOutput) <- c("timing", "streams", "VoI")

load(test_path("testdata", "expOutput.rda"))
load(test_path("testdata", "testData.rda"))
load(test_path("testdata", "testDataShort.rda"))
load(test_path("testdata", "testDataLong.rda"))
load(test_path("testdata", "testDataLongShort.rda"))
load(test_path("testdata", "testDataLongErrorSNA.rda"))
load(test_path("testdata", "testDataLongErrorTNA.rda"))
load(test_path("testdata", "testDataLongErrorTCH.rda"))
load(test_path("testdata", "testDataLongErrorHNA.rda"))
load(test_path("testdata", "testDataLongErrorHCH.rda"))

## function(dataFrame, timing, streams, VoI = NA, type="long", median0 = NA, delta = 3) ##

testthat::test_that("dataRead program works", {

  testthat::expect_message(dataRead(testData, timing="hour", streams=c("rep1", "rep2", "rep3", "rep4", "rep5", "rep6", "rep7", "rep8", "rep9", "rep10"), type="wide", median0 = .8), "Data is wide")      # Check for wide data
  testthat::expect_error(dataRead(testData, streams=c("rep1", "rep2", "rep3", "rep4", "rep5", "rep6", "rep7", "rep8", "rep9", "rep10"), type="wide"), "Input dataframe needs timing variable")            # Check missing time variable
  testthat::expect_error(dataRead(testData, "hour", type="wide"), "Input dataframe needs stream identifier variable")          # Check for stream identifier
  testthat::expect_error(dataRead(testDataLongShort, "hour", "time", streams="rep", type="long"), "Input dataframe needs equal sample count")       # Check for equal sample count in long data
  testthat::expect_error(dataRead(testDataShort, timing="hour", streams=c("rep1", "rep2", "rep3", "rep4", "rep5", "rep6", "rep7", "rep8", "rep9", "rep10"), type="wide", median0 = .8), "Input dataframe needs equal sample count")             # Check for equal sample count in wide data
  #expect_error(dataRead(xlerror1, xlerror1$hour, streams=xlerror1$rep, xlerror1$time, type="long"), "Input variable of interest is non-numeric or has missing values")  #Missing value test
  testthat::expect_error(dataRead(testDataLongErrorSNA, "hour", streams="rep", "time", type="long"), "Input stream variable has missing values")  #Char value test
  testthat::expect_error(dataRead(testDataLongErrorHNA, "hour", "time", streams="rep", type="long"), "Input time variable has missing values")  #Missing value test ########
  testthat::expect_error(dataRead(testDataLongErrorHCH, "hour", "time", streams="rep", type="long"), "Input time variable is non-numeric")  #Char value test ########
  testthat:: expect_error(dataRead(testDataLongErrorTNA, "hour", "time", streams="rep", type="long"), "Input variable of interest has missing values")  #Missing value test Target
  testthat::expect_error(dataRead(testDataLongErrorTCH, "hour", "time", streams="rep", type="long"), "Input variable of interest is non-numeric")  #Char value test Target
  testthat::expect_error(dataRead(testDataLong, "hour", "time", streams="rep", type="long"), "Target median is missing")             # Check for target median
  testthat::expect_error(dataRead(testDataLong, "hour", "time", streams="rep", type="long", median0 = "hello"), "Inputted target median is non-numeric")             # Check for target median numeric
  #expect_error(dataRead(testDataLong, "hour", "time", streams="rep", type="long", median0 = 3), "Please input a target median less than 1")             # Check for target median less than 1
  #expect_error(dataRead(testDataLong, "hour", "time", streams="rep", type="long", median0 = -1), "Please input a target median greater than 0")             # Check for target median greater than 0
  testthat::expect_error(dataRead(testDataLong, "hour", "time", streams="rep", type="long", median0 = .8, delta = "hello"), "Inputted target delta is non-numeric")             # Check for delta numeric
  testthat::expect_equal(expOutput, dataRead(testData, "hour", streams=c("rep1", "rep2", "rep3", "rep4", "rep5", "rep6", "rep7", "rep8", "rep9", "rep10"), type="wide", median0 = .8))             # Check for wide output dataframe
  testthat::expect_equal(expOutput, dataRead(testDataLong, "hour", "time", streams="rep", type="long", median0 = .8))             # Check for long output dataframe
})
