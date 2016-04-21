################################################################################
#permTestSamp.R: permBootSamp(A,B,NUM=999,stat="simple")
#  
#Description: performs permutation test for various statistics using the 
#             sample function to perform permutations (boot is too slow). 
#                
#Input: A - First sample (either matrix or vector)
#       B - Second sample (dimensions must match A)
#       NUM - number of permutations to tes 
#       stat - statistic to use: 
#              simple - mean(colSums((x-y)^2))
#              ks - ks.test
#              nn - nearest neighbor
#              MMD - MMD
#              en - energy statistic
#              ts - ts.test
#              hotel - hotelling test
#
################################################################################


# #Check for packages.  If not installed, install it.
# check<-library("FNN",logical.return=T,quietly=T,verbose=F)
# if(check=="FALSE"){
#   install.packages("FNN")
#   library("FNN")
# }
# library("Hotelling")
# check<-library("Hotelling",logical.return=T,quietly=T,verbose=F)
# if(check=="FALSE"){
#   install.packages("Hotelling")
#   library("Hotelling")
# }
# check<-library("boot",logical.return=T,quietly=T,verbose=F)
# if(check=="FALSE"){
#   install.packages("boot")
#   library("boot")
# }

#basic statistic - could be replaced with ts.test 
simpleStat <- function(z,idx,sizes){
  z<-z[idx,1]
  x<-z[1:sizes[1]]
  y<-z[sizes[1]+1:sizes[2]]
  #return(mean(colSums((x-y)^2)))
  return(mean((x-y)^2))
}


#nearest neighbor
Tn3 <- function(z, ix, sizes) {
  n1 <- sizes[1]
  n2 <- sizes[2]
  n <- n1 + n2
  z <- z[ix, ]
  #o <- rep(0, NROW(z))
  #z <- as.data.frame(cbind(z, o))
  NN <- get.knn(z, k=3)
  block1 <- NN$nn.index[1:n1, ]
  block2 <- NN$nn.index[(n1+1):n, ]
  i1 <- sum(block1 < n1 + .5)
  i2 <- sum(block2 > n1 + .5)
  return((i1 + i2) / (3 * n))
}

#hotel stat
hotel <- function(z,idx,sizes){
  z<-z[idx,]
  x<-z[1:sizes[1],]
  y<-z[sizes[1]+1:sizes[2],]
  return(hotelling.stat(x,y, shrinkage = FALSE)$statistic)
}

#Function to create permutations using boot and call statistic functions above
permTestSamp <- function(A,B,num=999,stat = "simple"){
  sizes=c(NROW(A),NROW(B))
  big<-max(sizes)
  rowNumbers<-1:sum(sizes)
  data<-numeric(num+1)
  if(sizes[1]!=sizes[2]){stop("Dimensions do not match")}
  if(sizes[1]==1){
    z<-c(A,B)
  }else{
    z <- rbind(A, B)
  }
  
  if(stat == "ks") {
    if(NCOL(A)>1){stop("Too many Dims for ks.test")}
    data[1]<- ks.test(A,B,exact=F)$statistic
    for(i in 2:num+1){
      k<-sample(rowNumbers,big,replace=F)
      data[i]<-ks.test(z[k],z[-k],exact=F)$statistic
    }
    
  }else if (stat == "simple"){
    if(NCOL(A)>1){stop("Too many Dims for simple test")}
  
  }else if (stat == "MMD"){
    #call to MMD function goes here
  }else if(stat == "nn"){
    
  }else if (stat =="hotel"){

  }
  return(data)
}

