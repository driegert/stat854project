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
Q6(s.amt$data.mt, R.B, N)
