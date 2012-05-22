###
# Question 3
###

Q3 <- function(data, N, M){
  
  multi <- mt(data, N, NW=6, M=2048, K=3, adaptiveWeight=FALSE)
  freq <- seq(0, 1/(2*10), 1/(10*M))
  
  plot(multi[[1]], multi[[2]], type='l', log='y',
       ylab="Spectrum", xlab="Frequency", main="BMG microbarometer - MT")
  
  # Hanning window goodness
  M2 <- 400
  win.han <- (1/2) * (1 - cos(2 * pi * ((1:M2) + 0.5)/M2))
  
  # I should have used a loop or something else here...
  data.han1 <- data[1:400] * win.han
  data.han2 <- data[201:600] * win.han
  data.han3 <- data[401:800] * win.han
  data.han4 <- data[601:1000] * win.han
  
  # Calculate the section spectra
  spec.han1 <- (abs(fft(c(data.han1, rep(0, M - M2)))))^2
  spec.han2 <- (abs(fft(c(data.han2, rep(0, M - M2)))))^2
  spec.han3 <- (abs(fft(c(data.han3, rep(0, M - M2)))))^2
  spec.han4 <- (abs(fft(c(data.han4, rep(0, M - M2)))))^2
  
  # Hanning - mean spectrum
  ##########
  spec.han_mean <- (1/4) * (spec.han1 + spec.han2 + spec.han3 + 
    spec.han4)
  plot(freq, spec.han_mean[1:1025], type='l', log='y', xlab="Frequency (Hz)",
       ylab="Spectrum", main="BMG microbarometer - Hanning Sections: Mean")
  
  # Hanning - median spectrum
  ###########
  spec.han_matrix <- matrix(data=c(spec.han1, spec.han2, spec.han3,
                                   spec.han4), nrow=4, ncol=2048,
                            byrow=TRUE)
  
  # Grab the median of each frequency bin (of the 4 sections)
  spec.han_med <- apply(spec.han_matrix, 2, median)
  plot(freq, spec.han_med[1:1025], type='l', log='y', xlab="Frequency (Hz)", 
       ylab="Spectrum", main="BMG microbarometer - Hanning Sections: Median")
  
  # Hanning - Exponential
  ###########
  spec.han_exp <- exp((1/4) * (log(spec.han1) + log(spec.han2) +
    log(spec.han3) + log(spec.han4)))
  plot(freq, spec.han_exp[1:1025], type='l', log='y', xlab="Frequency (Hz)",
       ylab="Spectrum", main="BMG microbarometer - Hanning Sections: Exponential")
  
  # Returns the spectrum estimate only, no frequency information
  data.frame(freq=freq, mt=multi[[2]][1:1025], han_mean=spec.han_mean[1:1025], 
             han_med=spec.han_med[1:1025], han_exp=spec.han_exp[1:1025])
}