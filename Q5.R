###
# Question 5
###

# TODO:
# - off by a scale factor? ... feck

Q5 <- function(s.mt, data){
  M <- 2*( length( s.mt ) - 1 )
  N <- length( data )
  I.s <- ( sum( 2*s.mt[-c(1,length(s.mt))] ) + sum( s.mt[c(1,length(s.mt))] ) )/M
  S2 <- (N-1)/N*var(data)

  # Estimate quantization noise power
  # Use the last fifth of the spectrum
  qnp <- ( sum( 2*s.mt[-c(1:(floor(4*length(s.mt)/5)))] ) - s.mt[length(s.mt)] )/(ceiling(M/5)-1)

c(I.s, S2, qnp)
}