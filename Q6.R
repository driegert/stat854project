###
# Question 6
###

# TODO:
# - 

Q6 <- function(s.mt, R.B){
  R <- fft(s.mt)
  
  plot(R.B, ylim=c(min(c(R, R.B)), max(c(R, R.B))))
  line(R, col='red')
}