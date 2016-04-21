################################################################################
# This contains the tests for the Maximum Mean Discrepancy (MMD) test
#
# input: the two samples X and Y along with m the number of points in each sample (assumed to be equal)

#    Unbiased Quadratic Time Esimator:
# uMMDDecision(X,Y,m,R) does the tests for given number of boot comparisons R
# uMMD(X,Y,m) will give you the statistic for the original sample without giving you a value to compare it to
# uMMDP gives you the statistic given the kernel matrix 

#    Unbiased Linear Time Estimator:
# lMMD(X,Y,m) will give you the statistic
# lMMDDecision(X,Y,R)  Not a very fast method (just coded in the stuff for other statistics) 

##Modified for multi dim

#compute the radial basis function kernel for two vecs
rbfk <- function(a,b, sigma = 1){
  Diff <- a - b
  abDiffNorm <- Diff %*% Diff; #the inner product of a and b
  result <- exp(-1*abDiffNorm/(2*sigma))
}

#given the two samples X and Y, define the matrix P of the kernel on pairs

kernelMatrix <- function(X,Y,m){
  Z <- rbind(X,Y) #combine the rows of X and Y data points  
  P <- matrix(0,2*m,2*m)
  
  for(i in 1:(2*m)){
    for(j in 1:(2*m)){
      
      P[i,j] <- rbfk(Z[i,],Z[j,])    
    }
  }
  
  P <- P - diag(P)*diag(2*m) #make the diagonal elements 0
  result <- P
  
}


uMMD <- function(X,Y,m){
  P <- kernelMatrix(X,Y,m)
  xBlock <- P[1:m,1:m]
  yBlock <- P[(m+1):(2*m),(m+1):(2*m)]
  crossBlock <- P[1:m,(m+1):(2*m)]
  
  result <- (sum(xBlock)+sum(yBlock))/(m^2-m) - 2*sum(crossBlock)/(m^2)
}

uMMDP <- function(P,m){
  xBlock <- P[1:m,1:m]
  yBlock <- P[(m+1):(2*m),(m+1):(2*m)]
  crossBlock <- P[1:m,(m+1):(2*m)]
  
  result <- (sum(xBlock)+sum(yBlock))/(m^2-m) - 2*sum(crossBlock)/(m^2)
}

#Now we shuffle the matrix to get bootstrap baseline for the value then decide acceptance

uMMDDecision <- function(X,Y,R){
  X<-as.matrix(X)
  Y<-as.matrix(Y)
  #m <- dim(X)[1]
  m<-NROWS(X)
  
  Z <- rbind(X,Y) #combine the rows of X and Y data points  
  P <- matrix(0,2*m,2*m)
  
  for(i in 1:(2*m)){
    for(j in 1:(2*m)){
      P[i,j] <- rbfk(Z[i,],Z[j,])    
    }
  }
  
  P <- P - diag(P)*diag(2*m) #make the diagonal elements 0
  result <- P
  
  
  
  
  P <- kernelMatrix(X,Y,m)
  K <- 2*m
  
  S <- numeric(R)   #storage of values of statistic for different perms
  
  #SO <- uMMDP(P,m)  #statistic on original
  
  for( i in 1:R){
    k <- sample(K,2*m,replace=F)
    
    Q <- P[,k] #permute the columns
    Q <- Q[k,] #permute the rows in the same way to maintain symmetry
    
    S[i] = uMMDP(Q,m)
  }
  
  p <- mean(c(SO,S) >= SO)
  options(warn = 0)
  
  pval <- p

}



# lMMD

hk <- function(a,b,c,d){
  result <- rbfk(a,c) + rbfk(b,d) - rbfk(a,d) - rbfk(b,c)
}

lMMD <- function(X,Y){
  m <- dim(X)[1]
  m2 <- floor(m/2)
  Z <- rbind(X,Y)   #since X and Y are rows of data, we use row bind 
  stat <- 0
  for(i in (1:m2)){
    stat <- stat + hk(Z[(2*i-1),],Z[(2*(2*i-1)),],Z[2*i,],Z[4*i,]) 
  }
  
  statistic <- stat/m2
  statistic <- as.numeric(statistic)
  return(as.data.frame(statistic))
}

lMMDDecision <- function(X,Y,R){
  m <- dim(X)[1]
  
  Z <- rbind(as.matrix(X),as.matrix(Y))   #combine the samples into one vector
  S <- numeric(R)   #values of statistic for different perms
  options(warn = 1)
  SO <- lMMD(X,Y)  #statistic on original
  
  for (i in 1:R) {
    #indices sample
    indexSample <- sample(2*m,m)
    x1 <- Z[indexSample,]
    y1 <- Z[-indexSample,]
    S[i] <- lMMD(x1,y1)
  }
  
  p <- mean(c(SO,S) >= SO)
  options(warn = 0)
  
  result <- p
}

