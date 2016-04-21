library(ggplot2)
#################################################################################################################
#Some initial plotting parameters for the abline
slope=1
int = 0

#################################################################################################################
#Same Mean Same variance
norm1<-rnorm(100,0,1)
norm2<-rnorm(100,0,1)
o<-'mu'
x<-cbind(norm1,"mu1=0")
y<-cbind(norm2,"mu2=0")

d1 <- as.data.frame(qqplot(norm1,norm2,plot.it=FALSE))
p1<-ggplot(d1) + geom_point(aes(x=x, y=y)) + geom_abline(slope = slope, intercept = int) + 
  geom_point(aes(x=x, y=y),colour="blue")+ggtitle("mu1=0, var1=1 - mu2=0, var2=1")


d2<-data.frame(as.numeric(rbind(x,y)[,1]),rbind(x,y)[,2])
colnames(d2) <- c("data", "mu")
p2 <- ggplot(data=d2, aes(x=data ,col=as.factor(mu))) +
      geom_density() +
      ggtitle("mu1=0, var1=1 - mu2=0, var2=1")

#################################################################################################################
#Different Mean Same Variance

norm1<-rnorm(100,0,1)
norm2<-rnorm(100,1,1)

x<-cbind(norm1,"mu1=0")
y<-cbind(norm2,"mu2=.5")

d1 <- as.data.frame(qqplot(norm1,norm2,plot.it=FALSE))
p3<-ggplot(d1) + geom_point(aes(x=x, y=y)) + geom_abline(slope = slope, intercept = int) + 
  geom_point(aes(x=x, y=y),colour="blue")+ggtitle("mu1=0, var1=1 - mu2=.5, var2=1")

d2<-data.frame(as.numeric(rbind(x,y)[,1]),rbind(x,y)[,2])
colnames(d2) <- c("data", "mu")
p4 <- ggplot(d2, aes(x=data ,color=factor(mu))) +
  geom_density() +
  ggtitle("mu1=0, var1=1 - mu2=.5, var2=1")

#################################################################################################################
#Diffent Mean and Different Variance
norm1<-rnorm(100,0,1)
norm2<-rnorm(100,0,2)

x<-cbind(norm1,"var1=1")
y<-cbind(norm2,"var2=2")

d1 <- as.data.frame(qqplot(norm1,norm2,plot.it=FALSE))
p5<-ggplot(d1) + geom_point(aes(x=x, y=y)) + geom_abline(slope = slope, intercept = int) + 
  geom_point(aes(x=x, y=y),colour="blue")+ggtitle("mu1=0, var1=1 - mu2=0, var2=2")

d2<-data.frame(as.numeric(rbind(x,y)[,1]),rbind(x,y)[,2])
colnames(d2) <- c("data", "mu")
p6 <- ggplot(d2, aes(x=data ,color=factor(mu))) +
  geom_density() +
  ggtitle("mu1=0, var1=1 - mu2=0, var2=2")

#################################################################################################################

#Different Distributions
norm1<-rnorm(100,0,1)
norm2<-rexp(100,2)

x<-cbind(norm1,1)
y<-cbind(norm2,2)
#rbind(x,y)

d1 <- as.data.frame(qqplot(norm1,norm2,plot.it=FALSE))
p7<-ggplot(d1) + geom_point(aes(x=x, y=y)) + geom_abline(slope = slope, intercept = int) + 
  geom_point(aes(x=x, y=y),colour="blue")+ggtitle("Std Normal vs Exponential alpha =2")

d2<-data.frame(as.numeric(rbind(x,y)[,1]),rbind(x,y)[,2])
colnames(d2) <- c("data", "mu")
p8 <- ggplot(d2, aes(x=data ,color=factor(mu))) +
  geom_density() +
  ggtitle("Std Normal vs Exponential alpha =2")

#################################################################################################################
#Will need to source the multiplot.R script to plot below

multiplot(p1,p3,p5,p7,p2,p4,p6,p8,cols=2)
