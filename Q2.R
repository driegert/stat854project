###
# Question 2
###

# TODO:
# - I'm not sure if this is actually right.
#   Should this data be standardized? (divide off the
#   sample variance?)

Q2 <- function(demeaned){
  R.B <- rep(0,201)
  for (t in 0:200){
    R.B[t+1] <- (1/N) * sum(demeaned[1:(N-t)] * demeaned[(t+1):N])
  }
  R.B <- R.B/R.B[1]
  
  plot(0:200, R.B, ylab=expression(R[B]), xlab="Lag", type = "h",
       main="BMG microbarometer Autocorrelations")
  
  R.B
}