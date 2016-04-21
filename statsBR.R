
################################################################################
#                           !!!NO Longer Used!!!!
#Function: simpleStat(x,y)
#  
#Description: Computes nearest neighbor statiistic 
#                
#Input: x - First sample (either matrix or vector)
#       y - Second sample (dimensions must match A)
#       
#
#Output: data frame, with the value of the statistics in $statistic
################################################################################
simpleStat <- function(x,y){
  #statistic<-mean((x-y)^2)
  statistic<-sqrt((mean(x)-mean(y))^2)
  return(as.data.frame(statistic))
}

################################################################################
#             Adapted from Rizzo - Statistical Computing with R
#
#Function: nNeighbor(x,y,k) 
#  
#Description: Computes nearest neighbor statiistic 
#                
#Input: x - First sample (either matrix or vector)
#       y - Second sample (dimensions must match A)
#       k - how many neighbors: 1 = closes,2 = 1st and second closest, 3= ... 
#       
#Output: data frame, with the value of the statistics in $statistic
################################################################################
nNeighbor <- function(x,y,k=3) {
  n1 <- NROW(x)
  n2 <- NROW(y)
  n <- n1 + n2
  z <- rbind(as.matrix(x),as.matrix(y))
  z<-cbind(z,0)
  NN <- get.knn(z, k)
  block1 <- NN$nn.index[1:n1, ]
  block2 <- NN$nn.index[(n1+1):n, ]
  i1 <- sum(block1 < n1 + .5)
  i2 <- sum(block2 > n1 + .5)
  statistic<-((i1 + i2) / (k * n))
  return(as.data.frame(statistic))
}

################################################################################
#              Adapted from Rizzo - Statistical Computing with R
#
#Function: edsist - adapted from Rizzo
#  
#Description: Computes the energy statistic
#                
#Input: x - First sample (either matrix or vector)
#       y - Second sample (dimensions must match A)
#       
#Output: data frame, with the value of the statistics in $statistic
################################################################################
edist <- function(x ,y) {
  
  n1 <- NROW(x)
  n2 <- NROW(y)
  z <- rbind(as.matrix(x),as.matrix(y))
  dst <- as.matrix(dist(z))
  ii <- 1:n1
  jj<-  ii+n1
  w <- n1 * n2 / (n1 + n2)
  # permutation applied to rows & cols of dist. matrix
  m11 <- sum(dst[ii,ii]) / (n1 * n1)
  m22 <- sum(dst[jj,jj]) / (n2 * n2)
  m12 <- sum(dst[ii,jj]) / (n1 * n2)
  statistic <- w * ((m12 + m12) - (m11 + m22))
  return (as.data.frame(statistic))
}

