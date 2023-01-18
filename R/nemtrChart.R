### nemtrChart ###
#'
#'
#'
#'

nemtrChart <- function(nemt_plot){
  ggplot(data = nemt_plot, aes(x = timing)) +
  geom_line(aes(y = st)) +
  geom_line(aes(y = UL)) +
  geom_line(aes(y = LL)) +
  geom_point(aes(y = st))
}
