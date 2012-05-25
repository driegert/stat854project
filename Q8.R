# Question 8
#############

Q8 <- function(y.t, phi){
  N <- length(y.t)
  M <- 2^(floor(log2(N)) + 2)
  
  est <- mt( y.t, N = N, NW = 10, K = 20 )
  corr <- complex(real=1, imaginary=0) - 
  spec.x <- est$data.mt / (abs(1 - fft(c(phi, rep(0, length(est$data.mt)-length(phi))))))^2
  H <- est$data.mt / spec.x
  
  list(res.spec=est, data.corr=spec.x, H=H)
}