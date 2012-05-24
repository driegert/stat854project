###
# Question 6
###

# TODO:
# - 

Q6 <- function(s.mt, R.B, N){
  s <- c(s.mt,rev(s.mt[-c(1,length(s.mt))]))
  s.c <- complex(real=s,imaginary=rep(0.,length(s)))
  R.var <- Re(fft(s.c))/N
  R <- R.var/R.var[1]
  plot(R.B, ylim=c(min(c(R, R.B)), max(c(R, R.B))), type="h")
  lines(R, col='red')
data.frame( R.var, R )
}