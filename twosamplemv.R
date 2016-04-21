#Draws random multivariate normal samples from means with
#two different distrubtions.
#Plot them in different colors.

library(MASS)

#dist to use.
Sigma <- matrix(c(1,0,0,1),2,2) #same covariance matrix
A <- mvrnorm(250,c(2,2),Sigma)    # different means
B <- mvrnorm(250,c(-2,-2),Sigma)

plot(A[,1],A[,2],col="red",xlim=c(-4,4),ylim=c(-4,4)) #plot A red
par(new=T)   #put second plot on top
plot(B[,1],B[,2],col="blue",xlim=c(-4,4),ylim=c(-4,4))
