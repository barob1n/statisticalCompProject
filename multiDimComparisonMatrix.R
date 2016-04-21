#################################################################################################################
#multiDimComparison: Compares two distributions, A and B, by leaving A alone and incrementing either the 
#                    mean OR variance of B by an amount "delta" from a minimum value of min_mu to a maximum value 
#                    of max_mu.  For each increment of the mean (or variance), "R" permutation tests are run and 
#                    a p-value calculated and stored in mu_p_data_statistic. Once the mean (or variance) has been 
#                    incremented through all values by amount delta, the percent that correctly identifed 
#                    pass/fail are stored in percent_mu_stat.
#
#input: None - requires parameters to be filled out in script
#
#ouput: plotting routine at bottom of sccript
#
#Requires: 
          source('mmdStatsMulti.R')
          source('permTestBR.R')
          source('statsBR.R')
          source('packageCheck.R')
#################################################################################################################
#                                   !!!   Parameter set up for tests    !!!
options(warn=1)
#samples<-c(5,15,50,100) 
samples<-c(50)  
steps_dim<-c(1)

#number of permutations to run
R<-999  
  
#perform this many tests per delta increment. Used to compute percent pass/fail
thisManyTests<-10
  
#Delta-  amount to  increment the mean/var of B for tests. Ex.: delta =.1 then test means 0,0.1,0.2,...
#min/max_mu -  min/max of difference in mean 
delta<- .25
min_mu<-0
max_mu<-2
min_var<-1
max_var<-5

#maxDim - max number of dimension to test-starts at 1  
#maxDim<-2
#dimDelta<-1
#steps_dim<-seq(1,maxDim,dimDelta)
  
#How much Type 1 Error?
alpha<-.05
#################################################################################################################
  
#sequence of means from min_mu to max_mu incremented by value given by delta
steps_mu <- seq(min_mu,max_mu,delta)
  
#same as above, but for variance
steps_var <- is.integer((max_var-min_var)/delta)  #keeping variance constant here

#List of statistics to use
statNames <- c("ks", "t.test","nn","edist","mmd","hotel")

#p-value at each iteration for various statistics when mean was varied
mu_p_data<-matrix(data=0,nrow=thisManyTests,ncol=length(statNames))
colnames(mu_p_data) <- c(statNames)
  
#percent that registered correct when means were varied
percent_mu<-matrix(data=0,nrow=length(steps_mu),ncol=length(statNames))
colnames(percent_mu) <- c(statNames)
  
#percent that registered correct when means were varied
timing<-matrix(data=0,nrow=thisManyTests,ncol=length(statNames))
colnames(timing) <- c(statNames)

#Creating lists to hold the data for later use
typeII.list <-list()
dimData.list <-list()
timing.list<-list()
timingDim.list<-list()


#cl <- makeCluster(mc <- getOption("cl.cores", 4))
#clusterExport(cl=cl, varlist=c("S","z","numRows","maxRows"),.export=c("get.knn","nNeighbor","edist"),envir=environment())
#clusterExport(varlist=c("S","z","numRows","maxRows","num"),envir=environment())  

for(d in 1:length(steps_dim)){
  dimension<-steps_dim[d]
  
  #A - Std normal - this distribution does not change, it is the base case
  Sigma<-diag(dimension)
  A <- mvrnorm(50,c(numeric(dimension)),Sigma) 
  
  for(p in 1:length(samples) ){ 
    m<-samples[p]
    n<-samples[p]
  
    #status update
    for(i in 1:length(steps_mu)){
      
      print(i,steps_mu)  #will tell us far along we are in moving the mean of the second distribution
    
      for(j in 1:thisManyTests){
      
        #This sample changes
        B <- mvrnorm(50,c(numeric(dimension-1),steps_mu[i]),Sigma)
    
        #Get data for ks stat
        ptm <- proc.time()
        data <- permTestBR(A,B,R,stat=ks.test,exact=FALSE)
        mu_p_data[j,"ks"] <- mean(data>=data[1])
        timing[j,"ks"]=timing[j,"ks"]+(proc.time() - ptm)[1]
      
        #Get data for simple stat
        ptm <- proc.time()
        data<-permTestBR(A,B,R,stat=t.test)
        mu_p_data[j,"t.test"]<-mean(data<=data[1])
        timing[j,"t.test"]=timing[j,"t.test"]+(proc.time() - ptm)[1]

        #Get data for hotel stat
        #ptm <- proc.time()
        #data<-permTestBR(A,B,R,stat=hotel.stat,shrinkage=FALSE)
        #mu_p_data[j,"hotel"]<-mean(data>=data[1])
        #timing[j,"hotel"]=timing[j,"hotel"]+(proc.time() - ptm)[1]
      
        #Get data for near neighbor stat
        ptm <- proc.time()
        data<-permTestBR(A,B,R,stat=nNeighbor,k=3)
        mu_p_data[j,"nn"]<-mean(data>=data[1])
        timing[j,"nn"]=timing[j,"nn"]+(proc.time() - ptm)[1]
      
        #Get data for edist stat
        ptm <- proc.time()
        data<-permTestBR(A,B,R,stat=edist)
        mu_p_data[j,"edist"]<-mean(data>=data[1])
        timing[j,"edist"]=timing[j,"edist"]+(proc.time() - ptm)[1]
      
        #Permutation test on quadratic MMD statistic done in MMD code
        ptm <- proc.time()
        #function does own permuting to speed up computation
        mu_p_data[j,"mmd"] <- uMMDDecision(A,B,R)
        timing[j,"mmd"]=timing[j,"mmd"]+(proc.time() - ptm)[1]
    }
    
      #Now that we've tested the methods on different samples, check how well they distinguish different distributions
      getPercentPass<-colSums(mu_p_data<=alpha)/(thisManyTests)*100
      percent_mu[i,]<-as.matrix(getPercentPass)
  }
  
    #list containing the type II data    
    typeII.list[[p]]<-percent_mu
    timing.list[[p]]<-colSums(timing)
    
  # Stop the clock
  print(proc.time() - ptm)
  
  }
  
  dimData.list[[d]]<-typeII.list
  timingDim.list[[d]]<-timing.list
}

######################################## Plotting  #############################################  
#So, say samples was defined as samples<-c(5,10).  And you wanna plot the data for when
#the number of samples was 5.  So you set whichSamples<-1 since 
#samples[whichSamples] = samples[1]=5.

#loops to print while drilling down through lists
for(i in 1:length(steps_dim)){
  dimData<-dimData.list[[i]]
  for(j in 1:length(samples)){
    meanData<-dimData[[j]]
    #for(k in 1:length(steps_mu)){
      
    title<-paste("Dimensions:",steps_dim[i],"Number of Samples:",samples[j])
    g <- ggplot(data=as.data.frame(cbind(steps_mu,meanData)),aes(steps_mu,y=value,color=variable) )+ 
      scale_shape_identity()+
      #geom_point(aes(y = percent_mu_ks, col = "KS")) + 
      geom_point(aes(y = ks, col = "KS",shape=2)) +
     # geom_line(aes(y = ks, col = "KS"))+
      geom_point(aes(y = t.test, col = "t.test")) +
      geom_line(aes(y = t.test, col = "t.test"))+
      geom_point(aes(y = nn, col = "Nearest Neighbor")) +
      geom_line(aes(y = nn, col = "Nearest Neighbor"))+
      geom_point(aes( y = edist,col = "Energy")) +
      geom_line(aes( y = edist,col = "Energy")) +
      geom_point(aes( y = mmd,col = "MMD")) +
      geom_line(aes( y = mmd,col = "MMD")) +
      #geom_point(aes( y = hotel,col = "Hotelling")) +
      #geom_line(aes( y = hotel,col = "Hotelling")) +
      ggtitle(title) +
      
      labs(x="Distance Between Means",y="Percentage of Correct Rejections")
    
    
    plot(g)
 
   # }
  }
}
  


