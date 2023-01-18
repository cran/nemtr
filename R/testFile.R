#nemtr(df, timing="hour", streams=c("rep1", "rep2", "rep3", "rep4", "rep5", "rep6", "rep7", "rep8", "rep9", "rep10"), type="wide", median0 = .8)



#median0 = 0.8
#n <- length(unique(expOutput$streams))
#expOutput %>%
#  filter(VoI > median0) %>%
#  group_by(streams,timing) %>%
#  summarise(frequency = n()) %>%
#  mutate(zt = (frequency - (n*0.50)/(n*2.5)**0.50))  -> nemt_cusum

#turnstiles %>% group_by(turnstile,hour) %>% summarise(num.streams = n()) -> e
#expOutput  %>% group_by(streams) %>% summarise(num.streams = n()) -> c

#n <- e[1,3]

# generateTurnstileData <- function(){
#   set.seed(795014178)
#   streams <- 20
#   time <- 60
#   samples <- 15
#   mu0 <- 3
#   delta <- 3
#
#   library(dplyr)
#   turnstiles <- tibble(
#     turnstile = rep(rep(1:streams,each=samples),time),
#     hour = rep(1:time,each=streams * samples),
#     sample = rep(rep(1:samples), times = streams * time),
#     waitTime = rexp(streams * time * samples,rate=.22985)
#   ) %>% mutate(waitTime = if_else(hour == 38, waitTime * 2,waitTime))
#
#
#   mean(turnstiles$waitTime)
#   median(turnstiles$waitTime)
#
#   quantile(turnstiles$waitTime,.80)
# }

#dfL %>% filter(time > 0.8) -> greaterM0
#colnames(greaterM0) <- c("timing", "streams", "VoI")
#greaterM0 %>% group_by(streams,timing) %>% summarise(frequency = n()) %>% mutate(zt = (frequency - (10*0.50))/((10*.25)**0.50))  -> nemt_cusums
#c <- 10
#delta <- 3

#nemt_cusums %>%
#  group_by(timing) %>%
#  summarise(EMT = sum(zt)) %>%
#    mutate(st = cumsum(EMT),
#           stLag = lag(st),
#           UL = ifelse(is.na(stLag), delta*(c**0.5), stLag + delta*(c**0.5)),
#           LL = ifelse(is.na(stLag), -1*delta*(c**0.5), 2*stLag - UL),
#           OOC = ifelse(st > UL | st < LL, timing,NA)) -> nemt_plot

#
# nemtr(df, timing="hour", streams=c("rep1", "rep2", "rep3", "rep4", "rep5", "rep6", "rep7", "rep8", "rep9", "rep10"), type="wide", median0 = 2)
#
# nemtr(dfL, "hour", "time", streams="rep", type="long", median0 = 2)
#
# dataRead(df, timing="hour", streams=c("rep1", "rep2", "rep3", "rep4", "rep5", "rep6", "rep7", "rep8", "rep9", "rep10"), type="wide", median0 = 2)
#
# median0 = 2
# delta = 3
#
# dataProc <- dataRead(dfL, "hour", "time", streams="rep", type="long", median0 = 2)
# dataProc  %>% group_by(streams) %>% summarise(num.streams = n()) -> c
# c <- length(c$num.streams)
# dataProc %>%  group_by(timing) %>% summarise(num.streams = n()) -> t
# t <- length(t$num.streams)
# n <- length(unique(dataProc$streams))
# dataProc %>%
#   filter(VoI > median0) %>%
#   group_by(streams,timing) %>%
#   summarise(frequency = n()) %>%
#   mutate(zt = (frequency - (n*0.50))/((n*0.25)**0.50))  -> nemt_cusum
#
# nemt_cusum %>%
#   group_by(timing) %>%
#   summarise(EMT = sum(zt)) %>%
#   mutate(st = cumsum(EMT),
#          stLag = lag(st),
#          UL = ifelse(is.na(stLag), delta*(c**0.5), stLag + (delta*(c**0.5))),
#          LL = ifelse(is.na(stLag), -1*delta*(c**0.5), (2*stLag) - UL),
#          OOC = ifelse(st > UL | st < LL, timing,NA)) -> nemt_plot
#
# ggplot(data = nemt_plot, aes(x = timing)) +
#   geom_line(aes(y = st)) +
#   geom_line(aes(y = UL)) +
#   geom_line(aes(y = LL)) +
#   geom_point(aes(y = st))

# df_long %>%
#   filter(value > mu0) %>%
#   group_by(rep,hour) %>%
#   summarise(frequency = n()) %>%
#   mutate(Z_t = (frequency - (n * 0.5)) / ((n*0.25)^.5)) -> nemt_cusum





