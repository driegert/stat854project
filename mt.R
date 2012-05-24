###
# Multitaper function based on
# assignment 4
###

# TODO:

mt <- function(data, N, NW, K=NULL, M=NULL, adaptiveWeight=TRUE, tol=0.01){

  W <- NW / N
  
  if (is.null(K)){
    K <- floor(2*NW - 1)
  }
  
  if (is.null(M)){
    M <- 2^(floor(log2(N)) + 2)
  }
  
  # Setup for dstebz
  D <- as.numeric((((N-1)/2) - (0:(N-1)) )^2 * cos(2*pi*W))
  E <- as.numeric(1:(N-1)) * (N - (1:(N-1))) * 0.5
  
  B.val <- dstebz(D, E)
  dpss <- dstein(D=D, E=E, N=N, W=B.val$W, IBLOCK=B.val$IBLOCK, 
                 ISPLIT=B.val$ISPLIT)
  
  # Check even eigenvectors (they are indexed from 0, but R
  # indexes from 1)
  for (i in seq(1, N, 2)){
    if (!sum(dpss$Z[,i]) > 0){
      dpss$Z[,i] <- (-1) * dpss$Z[,i]
    }
  }
  
  # Check odd labelled eigenvectors
  for (i in seq(2, N, 2)){
    sum <- 0
    for (j in 0:(N-1)){
      sum <- sum + ((N - 1 - (2*j)) * dpss$Z[(j+1),i])
    }
    
    if (!sum > 0){
      dpss$Z[,i] <- (-1) * dpss$Z[,i]
    }
  }

  data.mt <- rep(0, M)
  
  if( adaptiveWeight ){
  # Adaptively weight this to the max, yo
    # Create the tridiagonal matrix
    A <- matrix(0,N,N)
    for( row in 1:N ){
      for( col in 1:N ){
        A[row,col] <- sin( 2*pi*W*(row-col) )/(pi*(row-col))
      }
    }
    diag(A) <- 2*W
    # The lambdas
    lam <- colSums( (A%*%dpss$Z/dpss$Z) )/N
    lam <- rev(lam)[1:K]
    # Calculate the eigenspectra
    eS <- matrix( 0., nrow = M, ncol = K )
    for( i in 0:(K-1) ) eS[,i+1] <- (abs(fft(c(data * dpss$Z[,N-i], rep(0, M-N)))))^2
    # Use the first two eigenspectra as our initial estimate
    S <- rowMeans(eS[,1:2])
    # Starting value for the convergence diagnostic
    cd <- sum(S); cval <- tol + 1
    # Variance of the data
    sig2 <- (N-1)/N*var(data)
    
    while( cval > tol ){
      # Iterate
      # Calculate the adjusted weights
      d <- eS*0.
      for( i in 0:(K-1) ) d[,i+1] <- sqrt(lam[i+1])*S / ( lam[i+1]*S + (1-lam[i+1])*sig2 )
      # Calculate the re-weighted spectrum estimate
      S <- rowSums( d^2 * eS )/rowSums( d^2 )
      # Convergence diagnostic
      cval <- abs( cd - sum(S) )/cd
      cd <- sum(S)
    }  
  data.mt <- S  
  }else{
    # Compute the multitaper spectrum estimate.
    for (i in 0:(K-1)){
      data.mt <- data.mt + ((1/K) * (abs(fft(c(data * dpss$Z[,N-i], rep(0, M-N)))))^2)
    }
  }
  
  freq <- seq(0, 0.05, 1/(10*M))
  
  data.frame(freq=freq, data.mt=data.mt[1:(M/2 + 1)])
}
