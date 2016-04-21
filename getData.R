#################################################################################################################
#Main script to run test of methods


#################################################################################################################
#                                      Parameter set up for tests
m <- 50
n <- 50
R<-999
num<-R
#perform this many iterations per delta increment.  
thismany<-20

#amount to increment for tests. Ex.: delta =.1 then test means 0,0.1,0.2,...
delta<-.5
min_mu<-0
max_mu<-.5
min_var<-1
max_var<-5

#cut off for p value
alpha<-.05
#################################################################################################################

#sequence of means from min_mu to max_mu incremented by value given by delta
steps_mu<-seq(min_mu,max_mu,delta)

#same as above, but for variance
steps_var<-is.integer((max_var-min_var)/delta)

#simple stat data for various means
mu_p_data<-numeric(thismany)

#percent that registered correct
percent_simp_mu<-numeric(length(steps_mu))

#Std normal - this distribution does not change
A<-rnorm(n,0,min_var)

# Start the clock!
ptm <- proc.time()
S<-numeric(R+1)


for(i in 1:length(steps_mu)){
  #The B distribution changes
  
  
  N=c(NROW(A),NROW(B))
  if(NCOL(A)!=NCOL(B)){stop("Dimensions do not match")}
  numRows<-1:sum(N)
  maxRows<-max(N)
  S<-numeric(num+1)
  if(NCOL(A)==1){
    z<-c(A,B)
  }else{
    z <- rbind(A, B)
  }
  
  for(j in 1:thismany){
   #data<-permTestBR(A,B,999,stat=ks.test,exact=FALSE) 
    B<-rnorm(m,steps_mu[i],min_var)
    if(NCOL(A)==1){
      z<-c(A,B)
    }else{
      z <- rbind(A, B)
    }
    hold<-ks.test(A,B,exact=FALSE)
    S[1]<-hold$statistic
    
    for(l in 2:num+1){
      k<-sample(numRows,maxRows,replace=F)
      x<-z[k]
      y<-z[-k]
      hold<-ks.test(x,y,exact=FALSE)
      S[l]<-hold$statistic
    }
    
   mu_p_data[j]<- mean(S >= S[1])
  }
  percent_simp_mu[i]<-(sum(mu_p_data<=alpha))/(thismany)*100

}
S[1]
head(S)
percent_simp_mu

# Stop the clock
proc.time() - ptm
