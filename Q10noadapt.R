
Q10noadapt <- function( data.1, data.2, N, NW, K = NULL ){

  # Run mt() and get the spectra, eigencoefficients and weights
  mt.1 <- mt( data.1, N=N, NW=NW, K=K, adaptiveWeight=FALSE
              , forcoh = TRUE)
  mt.2 <- mt( data.2, N=N, NW=NW, K=K, adaptiveWeight=FALSE
              , forcoh = TRUE)
  S.1 <- c( mt.1$spec$data.mt, rev(mt.1$spec$data.mt[-1])[-1] ) 
  S.2 <- c( mt.2$spec$data.mt, rev(mt.2$spec$data.mt[-1])[-1] )  
  
  # Calculate the coherence
  eS <- mt.1$eC * Conj(mt.2$eC)
  c.num <- rowMeans( eS )
  c.den <- sqrt( S.1 * S.2 )
  coherency <- c.num / c.den
  
  # Get the phase
  c.norm <- c.num / Mod( c.num )
  phase <- 360/(2*pi)*atan2( Im(c.norm), Re(c.norm) )
  
  # Get the mag. sq. coherence
  msc <- ( abs( coherency ) )^2

list( msc = msc, phase = phase )  
}


