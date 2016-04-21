#Store data for each value of the mean in list for later plotting
df<-data.frame(steps_mu,percent_mu_ks,percent_mu_simp,percent_mu_nn, percent_mu_mmd)
dataFrames[p]<-list(df)

#So, say samples was defined as samples<-c(5,10).  And you wanna plot the data for when
#the number of samples was 5.  So you set whichSamples<-1 since 
#samples[whichSamples] = samples[1]=5.

#### Uncomment below to use for plotting once the script finishes ####
#
#   whichSamples<-2
#   title<-paste(samples[whichSamples],"samples From Each Distribution")
#   g <- ggplot(data=as.data.frame(dataFrames[whichSamples]),aes(steps_mu,y=value,color=variable)) + 
#     geom_line(aes(y = percent_mu_ks, col = "KS")) + 
#     geom_line(aes(y = percent_mu_simp, col = "Simple")) +
#     geom_line(aes(y = percent_mu_nn, col = "Nearest Neighbor")) +
#     geom_line(aes( y = percent_mu_edist,col = "Energy")) +
#     geom_line(aes( y = percent_mu_mmd,col = "MMD")) +
#     geom_line(aes( y = percent_mu_lmmd,col = "linear MMD")) +
#     ggtitle("50 Samples From Each Distribution") +
#     labs(x="Distance Between Means",y="Percentage of Correct Rejections")
#   
#   plot(g)