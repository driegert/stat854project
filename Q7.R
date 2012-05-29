# Question 7
############

Q7 <- function(data, R, AR_order){
  N <- length(data)
  
  ar.mod <- numeric(N)
  ar.mod[1:AR_order] <- data[1:AR_order]
  
  # Get coefficients for the AR
  # Build the correlation matrix
  cmat <- diag( rep( 1., AR_order + 1 ) )
  supdiag <- function( len, ord ) cbind( 1:(len-ord), (1+ord):len )
  subdiag <- function( len, ord ) cbind( (1+ord):len, 1:(len-ord) )
  for( i in 1:AR_order ){
    cmat[ supdiag( AR_order + 1, i) ] <- R[i+1]
    cmat[ subdiag( AR_order + 1, i) ] <- R[i+1]
  }
  # The Yule-Walkerish equation for the coefficients
  phi <- solve( cmat )%*%R[1:(AR_order+1)]

  # Calculate the AR-predicted series
  for (i in (1+AR_order):N){
    ar.mod[i] <- sum(phi[1:AR_order] * data[(i-AR_order):(i-1)])
  }
  # The whitened data series.
  y.t <- data - ar.mod
  y.t <- y.t - mean(y.t)
  
list(y.t=y.t[(AR_order+1):N], coef=phi[1:AR_order])
}