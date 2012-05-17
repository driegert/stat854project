###
# Question 3
###

# TODO:
# - label the axes properly - need to make a note
#   of the frequencies being used (and time steps, etc).
# - Use the hanning taper and compare the two spectrum estimates

Q3 <- function(data, N, M){
  multi <- mt(data, N, NW=6, M=2048, K=3, adaptiveWeight=FALSE)
  
  plot(multi[[1]], multi[[2]], type='l', log='y',
       ylab="Spectrum", xlab="Frequency", main="BMG microbarometer")
  
  # Hanning window goodness
  win.han <- (1/2) * (1 - cos(2 * pi * ((1:N) + 0.5)/N))
  data.han <- data * win.han
  
  
  # Returns the spectrum estimate only, no frequency information
  multi[[2]]
}