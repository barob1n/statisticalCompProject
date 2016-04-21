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




#ks statistic 
kolSmirStat <- function(z,idx,sizes){
  z<-as.matrix(z[idx,])
  x<-z[1:sizes[1],]
  y<-z[sizes[1]+1:sizes[2],]
  return(ks.test(x,y,exact=F)$statistic)
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
                 sim = "permutation", R = NUM , sizes = N, parallel = "multicore")
  }
  return(boot.obj)
}

