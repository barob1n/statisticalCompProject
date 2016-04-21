
A<-rnorm(50,0,1)
B<-rnorm(50,.5,1)
R <- 999
#z <- rbind(A,B)
z <- c(A,B)
K <- 1:(m+n)
D <- numeric(R)
options(warn = 1)

DO <- ks.test(A,B,exact=F)$statistic

for (i in 1:R) {
  #indices sample
  k <- sample(K,m,replace=F)
  x1 <- z[k]
  y1 <- z[-k]
  D[i] <- ks.test(x1,y1,exact=F)$statistic
}

p <- mean(c(DO,D) >= DO)
options(warn = 0)

print(p)

if(p < .05){
  print("Rejected")
}else {
  
  print("accepted")
}