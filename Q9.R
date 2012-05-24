

Q9 <- function( data, N, NW, K, fn = "Q9.pdf" ){
  # Calculate the spectrum for four sections of the data
  d.1 <- data[[1]][1:1000]; d.1 <- d.1 - mean(d.1)
  d.2 <- data[[1]][1001:2000]; d.2 <- d.2 - mean(d.2)
  d.3 <- data[[1]][2001:3000]; d.3 <- d.3 - mean(d.3)
  d.4 <- data[[1]][3001:4000]; d.4 <- d.4 - mean(d.4)
  
  s.1 <- mt( d.1, N = N, NW = NW, K = K )
  s.2 <- mt( d.2, N = N, NW = NW, K = K )
  s.3 <- mt( d.3, N = N, NW = NW, K = K )
  s.4 <- mt( d.4, N = N, NW = NW, K = K )
  
  s.2.d <- s.2$data.mt / s.1$data.mt
  s.3.d <- s.3$data.mt / s.1$data.mt
  s.4.d <- s.4$data.mt / s.1$data.mt
  
  # Plot the spectra together
  pdf( fn, width = 8, height = 12 )
  mar.d <- par("mar")
  par(mfrow=c(2,1), las=1, mar = mar.d + c(-1,2,0,0))
    ylim <- range( s.1$data.mt, s.2$data.mt, s.3$data.mt, s.4$data.mt)
    cols <- c( "black", "darkblue", "darkgreen", "red" )
    plot( s.1$freq, s.1$data.mt, ylim = ylim
          , log="y", type = "l", col = cols[1]
          , main = "Question 9", ylab = "", xlab = "Frequency" )
    mtext( "Power", 2, line = 4, las = 0 )
    lines( s.2$freq, s.2$data.mt, col = cols[2])
    lines( s.3$freq, s.3$data.mt, col = cols[3])
    lines( s.4$freq, s.4$data.mt, col = cols[4])

    # Plot the ratios between the spectra
    ylim <- range( s.2.d, s.3.d, s.4.d )
    plot( s.1$freq, s.1$data.mt, ylim = ylim, log="y"
          , type = "n", main = "", ylab = "", xlab = "Frequency" )
    mtext( "Ratio of Power", 2, line = 4, las = 0 )
    abline( h = 1, col = cols[1] )
    lines( s.2$freq, s.2.d, col = cols[2])
    lines( s.3$freq, s.3.d, col = cols[3])
    lines( s.4$freq, s.4.d, col = cols[4])
  
  dev.off()
  
}
