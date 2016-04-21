#Draws random multivariate normal samples from means with
#two different distrubtions.
#Plot them in different colors.

library(MASS)
source('permTestStats.R')  #include the file which contains the function definitions for the statistics

m <- 5
n <- 5

different <- 1
mu1 <- c(2,2,2)
mu2 <- c(2,1,2)

if(different == 0){
  mu1 <- c(2,2)
  mu2 <- c(2,2)  
}

Sigma <- matrix(c(1,0,0,0,1,0,0,0,1),3,3)
#Sigma <- matrix(c(1,0,0,1),2,2) #same covariance matrix
A <- mvrnorm(m,mu1,Sigma)    
B <- mvrnorm(n,mu2,Sigma)

#choose the statistic
permStat <- function(x,y){
  kernalstat(x,y)
}
#Kolmogorov-Smirnov test is just a permutation with the statistic that 
#looks at the max of the difference of the ecdfs

R <- 999   #how many perm to look at
z <- rbind(A,B)   #combine the data
K <- 1:(m+n)      #for indexing
S <- numeric(R)   #values of statistic for different perms
options(warn = 1)
 
SO <- permStat(A,B)  #statistic on original

for (i in 1:R) {
  #indices sample
  k <- sample(K,m,replace=F)
  x1 <- z[k,]
  y1 <- z[-k,]
  S[i] <- permStat(x1,y1)
}

p <- mean(c(SO,S) >= SO)
options(warn = 0)

print(p)

if(p < .05){
  print("Rejected")
}