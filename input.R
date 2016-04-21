
library(MASS)
source('permTestStats.R')  #include the file which contains the function definitions for the statistics

m <- 5
n <- 5

mu1 <- c(2,-2)
mu2 <- c(2,-2)

#Sigma <- matrix(c(1,0,0,0,1,0,0,0,1),3,3)
Sigma <- matrix(c(1,0,0,1),2,2) #same covariance matrix

A <- mvrnorm(m,mu1,Sigma)    
B <- mvrnorm(n,mu2,Sigma)
#A<-rnorm(m,1,1)
#B<-rnorm(n,1,1)
# attach(chickwts)
# A <- sort(as.vector(weight[feed == "sunflower"]))
# B <- sort(as.vector(weight[feed == "linseed"]))
# detach(chickwts)


data<-permTestBoot(A,B,999,stat="hotel")

tb <- c(data$t, data$t0)
mean(tb >= data$t0)
