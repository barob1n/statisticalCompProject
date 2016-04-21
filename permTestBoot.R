################################################################################
#permTestBoot.R: permBootTest(A,B,NUM=999,stat="simple")
#  
#Description: performs permutation test for various statistics using the 
#             boot function to perform permutations. 
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


#Check for packages.  If not installed, install it.
check<-library("FNN",logical.return=T,quietly=T,verbose=F)
if(check=="FALSE"){
  install.packages("FNN")
  library("FNN")
}
library("Hotelling")
check<-library("Hotelling",logical.return=T,quietly=T,verbose=F)
if(check=="FALSE"){
  install.packages("Hotelling")
  library("Hotelling")
}
check<-library("boot",logical.return=T,quietly=T,verbose=F)
if(check=="FALSE"){
  install.packages("boot")
  library("boot")
}

#basic statistic - could be replaced with ts.test 
simpleStat <- function(X,Y){
  statistic<-mean((x-y)^2)
  return(as.data.frame(statistic))
}

#ks statistic 
kolSmirStat <- function(z,idx,sizes){
  z<-as.matrix(z[idx,])
  x<-z[1:sizes[1],]
  y<-z[sizes[1]+1:sizes[2],]
  return(ks.test(x,y,exact=F)$statistic)
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
permTestBoot <- function(A,B,NUM=999,stat = "simple"){
  N=c(NROW(A),NROW(B))
  if(NCOL(A)!=NCOL(B)){stop("Dimensions do not match")}
  if(NCOL(A)==1){
    z<-c(A,B)
  }else{
    z <- rbind(A, B)
  }
  z <- as.data.frame(z)
  
  if(stat == "ks") {
    if(NCOL(A)>1){stop("Too many Dims for ks.test")}
    boot.obj <- boot(data = z, statistic = kolSmirStat,
                 sim = "permutation", R = 1 , sizes = N, parallel = "snow",ncpus=6)
  }else if (stat == "simple"){
    if(NCOL(A)>1){stop("Too many Dims for simple test")}
    boot.obj <- boot(data = z, statistic = simpleStat,
                     sim = "permutation", R = NUM, sizes = N, parallel = "snow")
  }else if (stat == "MMD"){
    #call to MMD function goes here
  }else if(stat == "nn"){
    boot.obj <- boot(data = z, statistic = Tn3,
                     sim = "permutation", R = 1, sizes = N, parallel = "snow")
  }else if (stat =="hotel"){
    boot.obj <- boot(data = z, statistic = hotel,
                     sim = "permutation", R = NUM, sizes = N, parallel = "snow")
  }
  return(boot.obj)
}

