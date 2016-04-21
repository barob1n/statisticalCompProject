################################################################################
#permTestBR.R: permTesBR(A,B,NUM=999,stat="ks.test")
#  
#Description: performs permutation test for various statistics using the 
#             sample function to create permutations. 
#                
#Input: A - First sample (either matrix or vector)
#       B - Second sample (dimensions must match A)
#       NUM - number of permutations to tes - default 999 
#       stat - statistic to use
#       ... - other variables as needed for statistic used
#
#Output: An array containing the result of the statistic for each permutation 
#        S[1] - the first value in the array - is the value before permuting
################################################################################

permTestBR <- function(A,B,R=999,stat,...){
  options(warn = 1)
  A<-as.matrix(A)
  B<-as.matrix(B)
  N=c(NROW(A),NROW(B))
  if(NCOL(A)!=NCOL(B)){stop("Dimensions do not match")}
  numRows<-1:sum(N)
  maxRows<-max(N)
  S<-numeric(R+1)
  z<-rbind(A,B)
  S[1]<-as.numeric(stat(A,B,...)$statistic)
  
  for(i in 2:(R+1)){
    k<-sample(numRows,maxRows,replace=F)
    x<-z[k,]
    y<-z[-k,]
    S[i]<-as.numeric(stat(x,y,...)$statistic)
  }
  
  return(S)
}
  

