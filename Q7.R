# Question 7
############

# TODO:

Q7 <- function(data, R, AR_order){
  N <- length(data)
  
  ar.mod <- rep(0, N)
  ar.mod[1:AR_order] <- data[1:AR_order]
  
  for (i in (1+AR_order):N){
    ar.mod[i] <- sum(R[(AR_order+1):2] * data[(i-AR_order):(i-1)])
  }
  
  y.t <- data - ar.mod
  
y.t[(AR_order+1):N]
}