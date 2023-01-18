#' @importFrom testthat test_that expect_equal
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#'
#'

load("./data/testData.rda")
load("./data/testDataLong.rda")
load("./data/expOutput.rda")
load("./data/nemt_plot.rda")

df <- testData
dfL <- testDataLong

testthat::test_that("nemtr works", {

    testthat::expect_equal(nemt_plot, nemtr(testData, timing="hour", streams=c("rep1", "rep2", "rep3", "rep4", "rep5", "rep6", "rep7", "rep8", "rep9", "rep10"), type="wide", median0 = 2))
})

