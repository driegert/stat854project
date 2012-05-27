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
s.amt <- Q4(data[[2]][1:N], N, NW = 10, K = 20 )

source('Q5.R')
Q5(s.amt$data.mt, data[[2]][1:N])

source('Q6.R')
R <- Q6(s.amt$data.mt, R.B, N)

# Returns the prewhitened data with N-AR_order points
source('Q7.R')
y.t <- Q7(data[[2]][1:N], R[[2]], 3)

source('Q8.R')
spec.est <- Q8(y.t$y.t, y.t$coef)

source("Q9.R")
data9 <- read.table( data_path )
q9 <- Q9( data9, N, NW = 10, K = 20 )

source("Q10.R")
data.2 <- Q1(data_path, N, instrument = 2)
R.B.2 <- Q2(data.2[[2]][1:N])
s.amt.2 <- Q4(data.2, N, NW = 10, K = 20 )
R.2 <- Q6(s.amt.2$data.mt, R.B.2, N)
y.t.2 <- Q7(data.2[[2]][1:N], R.2[[2]], 3)

q10 <- Q10( y.t[[1]], y.t.2[[1]], N=length(y.t[[1]]), NW=10, K = 20 )
#source("Q10noadapt.R")
#q10 <- Q10noadapt( y.t[[1]], y.t.2[[1]], N=length(y.t[[1]]), NW=7 )


# Let's plot all this business!

# Question 1
###
pdf("q1_data.pdf")
plot(seq(0, 9999, 10), data$raw, type='l', xlab="Time (s)", ylab="Pressure (UNITS)",
     main="BMG microbarameter", 
     sub="April 4, 2006 - 0 to 2:46:40 (h:mm:ss)")
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
pdf("q7_residuals.pdf")
plot(seq(1000-length(y.t), 10*length(y.t), 10), y.t, type='l', xlab="Time (s)", 
     ylab="Prediction Residual", 
     main="BMG microbarometer - AR2 Prediction Residuals")
dev.off()

# Question 8
#######
pdf("q8_ResidSpec.pdf")
plot(spec.est$res.spec, type='l', log='y',
     xlab="Frequency (Hz)", ylab="Spectrum", 
     main="Spectrum of the AR2 Prediction Residuals")
dev.off()

# ABSOLUTE VALUE SYMBOL IN ylab NEEDED!!
pdf("q8_Hf.pdf")
plot(spec.est$freq, spec.est$H, type='l', xlab="Frequency (Hz)", 
     ylab=expression(H(f)^2), 
     main="TITLE HERE")
dev.off()

pdf("q8_correctedSpec.pdf")
plot(spec.est$freq, spec.est$data.corr, type='l', log='y', 
     xlab="Frequency (Hz)", ylab="Spectrum", 
     main="Comparison of Direct and Prewhitened Estimations")
lines(s.amt, col='red')
legend("topright", c("Prewhitened", "Direct"), col=c("black", "red"), 
       lwd=c(1,1))
dev.off()

# Question 9
######


# Question 10
######