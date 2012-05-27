# Question 8
#############

# This function ONLY works for an AR2 ... 
# I probably should have generalized it.

Q8 <- function(y.t, phi){
  N <- length(y.t)
  M <- 2^(floor(log2(N)) + 2)
  
  est <- mt( y.t, N = N, NW = 10, K = 20 )
  f <- seq(0, 1/(2*10), 1/(10*M))
  corr <- 1 - (2*phi[1]*cos(2*pi*f)) - (2*phi[2]*cos(4*pi*f)) +
    (phi[1])^2 + (phi[2])^2 + (2*phi[1]*phi[2]*cos(2*pi*f))
  spec.x <- est$data.mt / corr
  H <- est$data.mt / spec.x
  
  list(freq=f, res.spec=est, data.corr=spec.x, H=H)
}