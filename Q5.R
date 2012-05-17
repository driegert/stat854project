###
# Question 5
###

# TODO:
# - off by a scale factor? ... feck

Q5 <- function(s.mt, data, N){
  I.s <- 2 * sum(s.mt) - s.mt[1]
  S2 <- (1/(N-1)) * sum( (data - mean(data))^2 )
  
  list(I.s, S2, 2 * (I.s - S2) / (I.s + S2))
}