#comparison of tests
#1D
#source('permTestBoot.R')
#source('mmdStats.R')
#source('permTestBR.R')
#source('statsBR.R')
#source('mmdStats.R')


#################################################################################################################
#Description: 
#Compares two distributions, A and B, by leaving A alone and incrementing either the mean OR variance
#of B by an amount "delta" from a minimum value of min_mu to a maximum value of max_mu.  For each increment of 
#the mean (or variance), "R" permutation tests are run and a p-value calculated and stored in mu_p_data_statistic. 
#Once the mean (or variance) has been incremented through all values by amount delta, the percent that correctly
#identifed pass/fail are stored in percent_mu_stat.
#################################################################################################################


#################################################################################################################
#                               !!!   Parameter set up for tests    !!!
#samples<-c(30)  #if you want to vary the amount of samples to compare performance
#samples<-c(5,10) 
#samples<- 50
#samples<-c(5,10,15,25,50,100) 
samples<-c(50)  
  R<-999    #number of permutations to run
  
  #perform this many tests per delta increment. Used to compute percent pass/fail
  #Each test regenerates the samples and has you accept/reject the null hypothesis.
  #Example: thisManyTests<-100, say 50 correctly identify different distributions, then 50/100 = 50 percent pass
  thisManyTests<-20
  
  #amount to increment the mean of B for tests. Ex.: delta =.1 then test means 0,0.1,0.2,...
  delta<- 1
  min_mu<-0
  max_mu<-2
  min_var<-1
  max_var<-5
  
  #cut off for p value
  alpha<-.05
  #################################################################################################################
  
  #sequence of means from min_mu to max_mu incremented by value given by delta
  steps_mu <- seq(min_mu,max_mu,delta)
  
  #same as above, but for variance
  #steps_var <- is.integer((max_var-min_var)/delta)  #keeping variance constant here
  
  #p-value at each iteration for various statistics when mean was varied
  mu_p_data_ks   <- numeric(thisManyTests)
  mu_p_data_simp <-numeric(thisManyTests)
  mu_p_data_nn   <-numeric(thisManyTests)
  mu_p_data_edist<-numeric(thisManyTests)
  mu_p_data_mmd  <-numeric(thisManyTests)
  mu_p_data_lmmd <-numeric(thisManyTests)
  
  #percent that registered correct when means was varied
  percent_mu_ks<-numeric(length(steps_mu))
  percent_mu_simp<-numeric(length(steps_mu))
  percent_mu_nn<-numeric(length(steps_mu))
  percent_mu_edist<-numeric(length(steps_mu))
  percent_mu_mmd<-numeric(length(steps_mu))
  percent_mu_lmmd<-numeric(length(steps_mu))
  
  dataFrames<-list()
  
  
  #Std normal - this distribution does not change, it is the base case
  
  
A <- rnorm(n, mean = min_mu, sd = min_var)
  
  for(p in 1:length(samples) ){   #First a wrapping for-loop to compare sample size performance, when the length is
    # m <- 10                       #more than 1 you'll want to make different data frames for the final storage
    # n <- 10                       #for each iteration and kill the plotting
    m<-samples[p]
    n<-samples[p]
  
  print(length(steps_mu))
  for(i in 1:length(steps_mu)){
    
    timeKS=timeSimp=timeNN=timeMMD=timeEdist=0
    # Start the clock!
    
    
    print(i,steps_mu)  #will tell us far along we are in moving the mean of the second distribution
    
    #The B distribution changes
    
    for(j in 1:thisManyTests){
      
      #define the second sample
      ptm <- proc.time()
      B <- rnorm(m, mean = steps_mu[i], sd = min_var)
      
      
      
      #Get data for ks stat
      ptm <- proc.time()
      data <- permTestBR(A,B,R,stat=ks.test,exact=FALSE)
      mu_p_data_ks[j] <- mean(data>=data[1])
      timeKS=timeKS+(proc.time() - ptm)[1]
      
      #Get data for simple stat
      ptm <- proc.time()
      data<-permTestBR(A,B,R,stat=simpleStat)
      mu_p_data_simp[j]<-mean(data>=data[1])
      timeSimp=timeSimp+(proc.time() - ptm)[1]
      
      ptm <- proc.time()
      #Get data for near neighbor stat
      data<-permTestBR(A,B,R,stat=nNeighbor,k=3)
      mu_p_data_nn[j]<-mean(data>=data[1])
      timeNN=timeNN+(proc.time() - ptm)[1]
      
      #Get data for edist stat
      ptm <- proc.time()
      data<-permTestBR(A,B,R,stat=edist)
      mu_p_data_edist[j]<-mean(data>=data[1])
      timeEdist=timeEdist+(proc.time() - ptm)[1]
      
      #Permutation test on linear MMD statistic done in MMD code
      data <- permTestBR(A,B,R,stat=lMMD)
      mu_p_data_lmmd[j] <- mean( data >= data[1])
      #mu_p_data_lmmd[j] <- lMMDDecision(A,B,R)
      
      #Permutation test on quadratic MMD statistic done in MMD code
      ptm <- proc.time()
      mu_p_data_mmd[j] <- uMMDDecision(A,B,m,R)
      timeMMD=timeMMD+(proc.time() - ptm)[1]
      
    }
    
    
    #Now that we've tested the methods on different samples, check how well they distinguish different distributions
    percent_mu_ks[i]<-(sum(mu_p_data_ks<=alpha))/(thisManyTests)*100
    percent_mu_simp[i]<-(sum(mu_p_data_simp<=alpha))/(thisManyTests)*100
    percent_mu_nn[i]<-(sum(mu_p_data_nn<=alpha))/(thisManyTests)*100
    percent_mu_edist[i]<-(sum(mu_p_data_edist<=alpha))/(thisManyTests)*100
    percent_mu_mmd[i]<-(sum(mu_p_data_mmd<=alpha))/(thisManyTests)*100
    percent_mu_lmmd[i]<-(sum(mu_p_data_lmmd<=alpha))/(thisManyTests)*100
  
    # Stop the clock
    print(proc.time() - ptm)
  }
  
 
  dfTime
    
  dfTime<-data.frame(timeSimp,timeKS,timeMMD,timeNN,timeEdist)
  times<-as.numeric(c(timeSimp,timeKS,timeMMD,timeNN,timeEdist))
  times<-as.numeric(times/min(times))
  timeMatrix<-cbind(as.matrix(c(times)),as.matrix(c("Simple","KS","MMD","NN","EDist")))
  dfTimeScl<-data.frame(timeMatrix)

  dfTimeScl$X2 <- factor(dfTimeScl$X2, levels= dfTimeScl$X2)
  p1<-ggplot(dfTimeScl , aes(x=dfTimeScl$X2,y=dfTimeScl$X1,col=X2)) + geom_point()  + 
  #  scale_y_continous(limits=c(0,50)))+
 
   ggtitle("Ratio of Speed - Simple Fastest at .041 min at 50 variables")+
  labs(x="Method",y="Ratio")
  plot(p1)
  
  
  
  df<-data.frame(steps_mu,percent_mu_ks,percent_mu_simp,percent_mu_nn, percent_mu_mmd,percent_mu_edist,percent_mu_lmmd)
  dataFrames[p]<-list(df)
  
######################################## Plotting  #############################################  
  #So, say samples wtupe(as defined as samples<-c(5,10).  And you wanna plot the data for when
  #the number of samples was 5.  So you set whichSamples<-1 since 
  #samples[whichSamples] = samples[1]=5.
  
# 
#     whichSamples<-p
#     title<-paste(samples[whichSamples],"samples From Each Distribution")
#     g <- ggplot(data=as.data.frame(dataFrames[whichSamples]),aes(steps_mu,y=value,color=variable)) + 
#       geom_line(aes(y = percent_mu_ks, col = "KS")) + 
#       geom_line(aes(y = percent_mu_simp, col = "Simple")) +
#       geom_line(aes(y = percent_mu_nn, col = "Nearest Neighbor")) +
#       geom_line(aes( y = percent_mu_edist,col = "Energy")) +
#       geom_line(aes( y = percent_mu_mmd,col = "MMD")) +
#       geom_line(aes( y = percent_mu_lmmd,col = "linear MMD")) +
#       ggtitle(title) +
#       labs(x="Distance Between Means",y="Percentage of Correct Rejections")
#     
#     plot(g)
  
  # Stop the clock
  print(proc.time() - ptm)
  
}