#comparison of tests
  #1D
source('permTestBoot.R')
source('mmdStats.R')
source('permTestBR.R')

#################################################################################################################
#Description: 
#Compares two distributions, A and B, by leaving A alone and incrementing either the mean OR variance
#of B by an amount "delta" from a minimum value of min_mu to a maximum value of max_mu.  For each increment of 
#the mean (or variance), "R" permutation tests are run and a p-value calculated and stored in mu_p_data_statistic. 
#Once the mean (or variance) has been incremented through all values by amount delta, the percent that correctly
#identifed pass/fail are stroed in percent_mu_stat.
#################################################################################################################


#################################################################################################################
#                               !!!   Parameter set up for tests    !!!
#samples<-c(5,10,15,20,30,40,70,100)
samples<-50
for(p in 1:length(samples) ){
# m <- 10 
# n <- 10
  m<-samples[p]
  n<-samples[p]


#simpleStat(c(X,Y),1,c(m,n))

R<-999

#simpleStat(c(X,Y),1,c(m,n))

#perform this many iterations per delta increment. Used to compute percent pass/fail
#Example: thismany<-100, say 50 correctly identify different distributions, then 50/100 = 50 percent pass
thismany<-20

#amount to increment for tests. Ex.: delta =.1 then test means 0,0.1,0.2,...
delta<-.1
min_mu<-0
max_mu<-2
min_var<-1
max_var<-5

#cut off for p value
alpha<-.05
#################################################################################################################

#sequence of means from min_mu to max_mu incremented by value given by delta
steps_mu<-seq(min_mu,max_mu,delta)

#same as above, but for variance
steps_var<-is.integer((max_var-min_var)/delta)

#p-value each iteration for various statistics when mean was varied
mu_p_data_ks<-numeric(thismany)
mu_p_data_simp<-numeric(thismany)
mu_p_data_nn<-numeric(thismany)
mu_p_data_edist<-numeric(thismany)
mu_p_data_mmd<-numeric(thismany)

#percent that registered correct when means was varied
percent_mu_ks<-numeric(length(steps_mu))
percent_mu_simp<-numeric(length(steps_mu))
percent_mu_nn<-numeric(length(steps_mu))
percent_mu_edist<-numeric(length(steps_mu))
percent_mu_mmd<-numeric(length(steps_mu))

#Std normal - this distribution does not change, it is the base case
A<-rnorm(n,min_mu,min_var)

# Start the clock!
ptm <- proc.time()

print(length(steps_mu))
for(i in 1:length(steps_mu)){
  
   print(i,steps_mu)
  
  #The B distribution changes
  
  for(j in 1:thismany){
    B<-rnorm(m,steps_mu[i],min_var)
    
    #Get data for ks stat
    data<-permTestBR(A,B,R,stat=ks.test,exact=FALSE)
    mu_p_data_ks[j]<-mean(data>=data[1])
    
    #Get data for simple stat
    data<-permTestBR(A,B,R,stat=simpleStat)
    mu_p_data_simp[j]<-mean(data>=data[1])
    
    #Get data for near neighbor stat
    data<-permTestBR(A,B,R,stat=nNeighbor,k=3)
    mu_p_data_nn[j]<-mean(data>=data[1])
    
    #Get data for edist stat
    data<-permTestBR(A,B,R,stat=edist)
    mu_p_data_edist[j]<-mean(data>=data[1])
    
    #Get data for MMD Radial stat
    #data<-permTestBR(A,B,R,stat=kernelStat)
    #mu_p_data_mmd[j]<-mean(data>=data[1])
    
    #Get decision for uMMD
    mu_p_data_uMMD[j] <- uMMDDecision(A,B,samples,R)
    
  }
  percent_mu_ks[i]<-(sum(mu_p_data_ks<=alpha))/(thismany)*100
  percent_mu_simp[i]<-(sum(mu_p_data_simp<=alpha))/(thismany)*100
  percent_mu_nn[i]<-(sum(mu_p_data_nn<=alpha))/(thismany)*100
  percent_mu_edist[i]<-(sum(mu_p_data_edist<=alpha))/(thismany)*100
  percent_mu_mmd[i]<-(sum(mu_p_data_mmd<=alpha))/(thismany)*100
  
  
}
#percent_mu_ks

#example of what ouput will look like
df<-data.frame(steps_mu,percent_mu_ks,percent_mu_simp,percent_mu_nn, percent_mu_mmd)
 ggplot(data=df,aes(steps_mu,y=value,color=variable)) + 
   geom_line(aes(y = percent_mu_ks, col = "percent_mu_ks")) + 
   geom_line(aes(y = percent_mu_simp, col = "percent_mu_simp")) +
   geom_line(aes(y = percent_mu_nn, col = "percent_mu_nn")) +
   geom_line(aes( y = percent_mu_edist,col = "percent_mu_edist")) +
   geom_line(aes( y = percent_mu_mmd,col = "percent_mu_mmd")) +
   ggtitle("50 variables")

# Stop the clock
proc.time() - ptm

}
