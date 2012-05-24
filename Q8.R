# Question 8
#############

Q8 <- function(y.t){
  N <- length(y.t)
  M <- 2^(floor(log2(N)) + 2)
  
  spec.Q8 <- Q3(y.t, N, M)
  est <- Q4(y.t, N, NW=6, K=10)
}