#################################################################################################################
#Main script to run test of methods

library(MASS)
source('permTestBoot.R')  #include the file which contains the function definitions for the statistics

#testing some univariate  data
m <- 50
n <- 50
thismany<-10

delta<-.05
min_mu<-0
max_mu<-.5
min_var<-0
max_var<-5

alpha<-.05

#steps_mu<-is.integer((max_mu-min_mu)/delta)
steps_mu<-seq(min_mu,max_mu,delta)

steps_var<-is.integer((max_var-min_var)/delta)

simp_mu_data<-numeric(thismany)
percent_simp_mu<-as.numeric(steps_mu)

R <- 999

#z <- c(A,B)
K <- 1:(m+n)
D <- numeric(R)


#Std normal
A<-rnorm(n,0,1)

# Start the clock!
ptm <- proc.time()


for(i in 1:length(steps_mu)){

  for(j in 1:thismany){
    B<-rnorm(m,steps_mu[i],1)
    z <- rbind(A,B)

    DO <- ks.test(A,B,exact=F)$statistic
    
    for (l in 1:999) {
      #indices sample
      k <- sample(K,m,replace=F)
      x1 <- z[k]
      y1 <- z[-k]
      D[l] <- ks.test(x1,y1,exact=F)$statistic
    }
    
    simp_mu_data[j] <- mean(c(DO,D) >= DO)
    
  }
  percent_simp_mu[i]<-(sum(simp_mu_data>=alpha))/length(thismany)

}

# Stop the clock
proc.time() - ptm
