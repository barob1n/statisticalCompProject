#################################################################################################################
#Main script to run test of methods
#################################################################################################################


#################################################################################################################
#                                      Parameter set up for tests
m <- 50
n <- 50
R<-999

#perform this many iterations per delta increment.  
thismany<-20

#amount to increment for tests. Ex.: delta =.1 then test means 0,0.1,0.2,...
delta<-.1
min_mu<-0
max_mu<-1.5
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

#percent that registered correct when means was varied
percent_mu_ks<-numeric(length(steps_mu))
percent_mu_simp<-numeric(length(steps_mu))
percent_mu_nn<-numeric(length(steps_mu))

#Std normal - this distribution does not change, it is the base case
A<-rnorm(n,min_mu,min_var)

# Start the clock!
ptm <- proc.time()

for(i in 1:length(steps_mu)){
  
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
  }
   percent_mu_ks[i]<-(sum(mu_p_data_ks<=alpha))/(thismany)*100
   percent_mu_simp[i]<-(sum(mu_p_data_simp<=alpha))/(thismany)*100
   percent_mu_nn[i]<-(sum(mu_p_data_nn<=alpha))/(thismany)*100

}

percent_mu_ks

#example of what ouput will look like
# plot(steps_mu,percent_mu_ks)
# points(steps_mu,percent_mu_nn)
# points(stemps_mu,percent_mu_simp)


df<-data.frame(steps_mu,percent_mu_ks,percent_mu_simp,percent_mu_nn)
ggplot(data=df,aes(steps_mu,y=value,color=variable)) + 
  geom_line(aes(y = percent_mu_ks, col = "percent_mu_ks")) + 
  geom_line(aes(y = percent_mu_simp, col = "percent_mu_simp")) +
  geom_line(aes(y = percent_mu_nn, col = "percent_mu_nn"))

# Stop the clock
proc.time() - ptm
