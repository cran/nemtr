## dataRead ##
#'
#' Read and Validate Dataframe
#'
#' Read in data and validate before analysis is conducted
#' @param dataFrame A user inputted dataframe, can be wide or long
#' @param timing A string of the timing variable name
#' @param streams A string of the streams variable name
#' @param VoI A string of the Variable of Interest name
#' @param type A string of the type of data (default long)
#' @param median0 A value for expected median
#' @param delta A value for delta (default 3)
#'
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#'
#' @return A validated dataframe in long format
#' @export
#'
#' @examples
#' set.seed(795014178)
#' streams <- 20
#' time <- 60
#' samples <- 15
#' mu0 <- 3
#' delta <- 3
#' library(dplyr)
#' turnstiles <- tibble(
#'   turnstile = rep(rep(1:streams,each=samples),time),
#'   hour = rep(1:time,each=streams * samples),
#'   sample = rep(rep(1:samples), times = streams * time),
#'   waitTime = rexp(streams * time * samples,rate=.22985)
#'   ) %>% mutate(waitTime = if_else(hour == 38, waitTime * 2,waitTime))
#' dataRead(turnstiles, timing="hour", streams="sample", VoI="waitTime", type="long", median0 = 3)
#'
dataRead <- function(dataFrame, timing, streams, VoI = NA, type="long", median0 = NA, delta = 3){
  if(missing(timing) == TRUE){
    stop("Input dataframe needs timing variable")
  }
  if(missing(streams) == TRUE){
    stop("Input dataframe needs stream identifier variable")
  }

  if(type=="wide"){
    message("Data is wide")
    #for(i in 1:length(streams)){
    #  if(length(streams[[i]])!=length(streams[[1]])){
    #    stop("Input dataframe needs equal sample count")
    #  }
    #}
    dataFrame <- tidyr::pivot_longer(dataFrame, cols = streams, names_to = "streams", values_to = "VoI")
    ## Do conversion
    colnames(dataFrame) <- c("timing", "streams", "VoI")
  }

  if(type=="long"){
  names(dataFrame)[names(dataFrame) == timing] <- "timing"
  names(dataFrame)[names(dataFrame) == streams] <- "streams"
  names(dataFrame)[names(dataFrame) == VoI] <- "VoI"
  }


  ifelse(dataFrame$streams == "", stop("Input stream variable has missing values"), NA)
  ifelse(is.numeric(dataFrame$timing), NA, stop("Input time variable is non-numeric"))
  ifelse(is.na(dataFrame$timing), stop("Input time variable has missing values"), NA)
  ifelse(is.numeric(dataFrame$VoI), NA, stop("Input variable of interest is non-numeric"))
  ifelse(is.na(dataFrame$VoI), stop("Input variable of interest has missing values"), NA)

  if(length(dataFrame[[1]])%%length(unique(dataFrame$timing))!=0){
    stop("Input dataframe needs equal sample count")
  }else
  if(length(dataFrame[[1]])%%length(unique(dataFrame$streams))!=0){
    stop("Input dataframe needs equal sample count")
  }
  if(is.na(median0) == TRUE){
    stop("Target median is missing")
  }
  if(is.numeric(median0) != TRUE){
    stop("Inputted target median is non-numeric")
  }
  #if(median0 < 0){
  #  stop("Please input a target median greater than 0")
  #}
  #if(median0 > 1){
  #  stop("Please input a target median less than 1")
  #}
  if(is.numeric(delta) != TRUE){
    stop("Inputted target delta is non-numeric")
  }

  x <- dataFrame
  return(x)


}
