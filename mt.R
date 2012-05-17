###
# Multitaper function based on
# assignment 4
###

# TODO:

mt <- function(data, N, NW, K=NULL, M=NULL, adaptiveWeight=TRUE){

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
  
  # Compute the multitaper spectrum estimate.
  for (i in 0:(K-1)){
    data.mt <- data.mt + ((1/K) * (abs(fft(c(data * dpss$Z[,N-i], rep(0, M-N)))))^2)
  }
  
  freq <- seq(0, 0.5, 1/M)
  
  list(freq, data.mt[1:(M/2 + 1)])
}
