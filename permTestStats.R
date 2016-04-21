#simple statistic

simpleStat <- function(x,y){
   sqrt(sum(mean(x-y))^2)
}

#Kolmogorov-Smirnov built in statistic
kolSmirStat <- function(x,y){
  ks.test(x,y,exact=F)$statistic
}