###
# Question 4
###

Q4 <- function(data, N, NW, K){
  est <- mt( data, N = N, NW = NW, K = K )
  plot(est$freq, est$data.mt, log="y", type="l"
       , main = "Question 4", xlab = "Frequency"
       , ylab = "Power")
est
}




