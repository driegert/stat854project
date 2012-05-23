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
  c(I.s, S2)
}