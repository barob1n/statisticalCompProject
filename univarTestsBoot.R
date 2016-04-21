#################################################################################################################
#Main script to run test of methods

library(MASS)
#source('permTestBoot.R')  #include the file which contains the function definitions for the statistics

#testing some univariate  data
m <- 50
n <- 50
thismany<-10

delta<-.1
min_mu<-0
max_mu<-1
min_var<-0
max_var<-5

alpha<-.05

#steps_mu<-is.integer((max_mu-min_mu)/delta)
steps_mu<-seq(min_mu,max_mu,delta)

steps_var<-is.integer((max_var-min_var)/delta)

simp_mu_data<-numeric(thismany)
percent_simp_mu<-as.numeric(steps_mu)


#Std normal
A<-rnorm(n,0,1)

# Start the clock!
ptm <- proc.time()

for(i in 1:length(steps_mu)){
  
  for(j in 1:thismany){
  B<-rnorm(m,steps_mu[i],1) 
   data<-permTestBoot(A,B,999,stat="ks") 
   tb <-c(data$t, data$t0)
   mean(tb >= data$t0)
   simp_mu_data[j]<- mean(tb >= data$t0)
   
  }
  percent_simp_mu[i]<-(sum(simp_mu_data>=alpha))/length(thismany)

}
# Stop the clock
proc.time() - ptm



