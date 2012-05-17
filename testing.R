# Testing Script

library('multitaper')
library('lpstuff')

data_path <- '/home/dave/school_lab/Courses/STAT854/Assignments/Project/BFO_data.10s'
setwd('/home/dave/school_lab/Courses/STAT854/Assignments/Project/')

N <- 1000
M <- 2048

source('Q1.R')
# Returns a list - raw data in [[1]]
# demeaned data in [[2]]
data <- Q1(data_path, N)
data <- data[[1]][1:N]

spec.mtm(data, k=2, nFFT=2048)
dpss.mtm <- dpss(n=1000, k=2, nw=6, returnEigenvalues=FALSE)

NW <- 6
W <- NW / N
K <- 2

# Setup for dstebz
D <- as.numeric((((N-1)/2) - (0:(N-1)) )^2 * cos(2*pi*W))
E <- as.numeric(1:(N-1)) * (N - (1:(N-1))) * 0.5

B.val <- dstebz(D, E)
dpss <- dstein(D=D, E=E, N=N, W=B.val$W, IBLOCK=B.val$IBLOCK, 
               ISPLIT=B.val$ISPLIT)

win1 <- dpss$Z[,N]
win2 <- dpss$Z[,N-1]

mtm1 <- dpss.mtm$v[,1]
mtm2 <- dpss.mtm$v[,2]

data.win1 <- data * win1
data.win2 <- data * win2

data.mtm1 <- data * mtm1
data.mtm2 <- data * mtm2

data.win1 - data.mtm1
data.win2 - data.mtm2

data.s1 <- (abs(fft(c(data.win1, rep(0,M-N)))))^2
data.s2 <- (abs(fft(c(data.win2, rep(0,M-N)))))^2

data.s <- (1/2) * data.s1 + (1/2) * data.s2
plot(data.s[1:1025], type='l', log='y')