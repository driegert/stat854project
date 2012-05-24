###
# Question 1
###

# TODO:
# - proper axes labels on the plots

# Reads in the data, calculates the mean, 
# median, plots the data, and returns the data
# and demeaned data.

Q1 <- function(path, N){
  # Read in data
  # BMG microbarometer
  data <- read.table(path)[1]
  data <- data[[1]]

  # Q1
  plot(data[1:N], type='l')
  data_median <- median(data[1:N])
  print(c("Median:", data_median))
  
  data_mean <- mean(data[1:N])
  print(c("Mean: ", data_mean))
  
  data_demean <- data[1:N] - data_mean
  data_norm <- data_demean / sd(data)
  moment_2 <- sum((data_norm[1:N])^2) / N
  moment_3 <- sum((data_norm[1:N])^3) / N
  moment_4 <- sum((data_norm[1:N])^4) / N
  
  print(c("Second moment:", moment_2))
  print(c("Third moment:", moment_3))
  print(c("Fourth moment:", moment_4))
  
  data.svar <- var(data)
  data.norm <- (data - data_mean) / data.svar

  qqnorm(data_demean)
  qqline(data_demean)
  print( shapiro.test(data_demean) )

  # Reasonably clear from the moments that this data is
  # most likely not normal.
  # The Shapiro-Wilk test confirms this.
  
  data.frame(raw=data[1:N], demean=data_demean)
}