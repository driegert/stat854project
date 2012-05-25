####
# STATS 854 - Final Project
####

# setwd('/home/dave/school_lab/Courses/STAT854/Assignments/sp/')
library('lpstuff')
source('mt.R')

data_path <- "BFO_data.10s"
N <- 1000

source('Q1.R')
# Returns a list 
# - raw data in [[1]], demeaned data in [[2]]
data <- Q1(data_path, N)

# Computes first 200 autocorrelations
source('Q2.R')
R.B <- Q2(data[[2]][1:N])

# Computes a simple multi-taper estimate and 
# 3 types of sectioned Hanning spectral estimate.
source('Q3.R')
spec <- Q3(data[[2]][1:N], N, 2048)

source("Q4.R")
s.amt <- Q4(data, N, NW = 10, K = 20 )

source('Q5.R')
Q5(s.amt$data.mt, data[[2]][1:N])

source('Q6.R')
R <- Q6(s.amt$data.mt, R.B, N)

# Returns the prewhitened data with N-AR_order points
source('Q7.R')
y.t <- Q7(data[[2]][1:N], R[[2]], 3)

source('Q8.R')
Q8(y.t)

source("Q9.R")
q9 <- Q9( data, N, NW = 10, K = 20 )

# Let's plot all this business!

# Question 1
###
pdf("q1_data.pdf")
plot(data$raw, type='l', xlab="Time (UNITS)", ylab="Pressure (UNITS)",
     main="BMG microbarameter - April 4, 2006 - 0 to 2:46:40 (h:mm:ss)")
dev.off()

pdf("q1_norm.pdf")
qqnorm(data$demean)
qqline(data$demean)
dev.off()

# Question 2
####
pdf("q2_bartlett.pdf")
plot(0:200, R.B, ylab=expression(R[B]), xlab="Lag", type = "h",
     main="BMG microbarometer Autocorrelations")
dev.off()

# Question 3
####
pdf("q3_multitaper.pdf")
plot(spec$freq, spec$mt, type='l', log='y',
     ylab="Spectrum", xlab="Frequency (Hz)", 
     main="BMG microbarometer - MT",
     sub="NW=6; K=5; M=2048")
dev.off()

pdf("q3_hanMean.pdf")
plot(spec$freq, spec$han_mean, type='l', log='y', 
     xlab="Frequency (Hz)", ylab="Spectrum", 
     main="BMG microbarometer - Hanning Sections: Mean",
     sub="4 sections - 400 points each; 50% overlap; mean taken")
dev.off()

pdf("q3_hanMedian.pdf")
plot(spec$freq, spec$han_med, type='l', log='y',
     xlab="Frequency (Hz)", ylab="Spectrum", 
     main="BMG microbarometer - Hanning Sections: Median", 
     sub="4 sections - 400 points each; 50% overlap; median taken")
dev.off()

pdf("q3_hanGeoMean.pdf")
plot(spec$freq, spec$han_exp, type='l', log='y',
     xlab="Frequency (Hz)", ylab="Spectrum", 
     main="BMG microbarometer - Hanning Sections: Geometric Mean", 
     sub="4 sections - 400 points each; 50% overlap; geometric mean taken")
dev.off()

pdf("q3_hanningCompare.pdf")
plot(spec$freq, spec$han_mean, type='l', log='y', lwd=2, 
     xlab="Frequency (Hz)", ylab="Spectrum",
     main="BMG microbarometer - Hanning Sectioning Approach")
lines(spec$freq, spec$han_med, col='red', lty=1)
lines(spec$freq, spec$han_exp, col='blue', lty=2)
lines(spec$freq, spec$mt, col='darkgreen')
legend("topright", c("Arithmetic Mean", "Median", "Geometric Mean", 
                     "Multitaper"), 
       col=c("black", "red", "blue", "darkgreen"), lty=c(1,1,2, 1), 
       lwd=c(2,1,1, 1))
dev.off()

# Question 4
########
pdf("q4_multitaper.pdf")
plot(s.amt$freq, s.amt$data.mt, type='l', log='y',
     xlab="Frequency (Hz)", ylab="Spectrum", 
     main="BMG microbarometer - Multitaper, Adpt Weight",
     sub="1000 samples; NW=10; K=20; M=2048")
dev.off()

# Question 6
#####
pdf("q6_autoCompare.pdf")
plot(R.B, ylim=c(min(c(R$R, R.B)), max(c(R$R, R.B))), 
     ylab="Autocorrelation", xlab="Lag", 
     type="h", main="Bartlett Autocorrelation vs. FFT of Spectrum 
     Autocorrelation est.")
lines(R$R, col='red')
dev.off()

# Question 7
########
