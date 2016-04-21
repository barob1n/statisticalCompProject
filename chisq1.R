#two-sample chi sq test

A <- rnorm(50, 3,.5)
B <- rnorm(50, 2,.5)



binSize <- 1
bins <- 10

binA <- numeric(bins)
binB <- numeric(bins)
#print(binA)

for (i in 1:bins) {
    binA[i] =  sum((A < (i-1)*binSize) & (A > (i-2)*binSize))
    binB[i] =  sum((B < (i-1)*binSize) & (B > (i-2)*binSize))
}

#for (i in 1:100) {
 # binA[i] =  sum((A < (i-1)/10) & (A > (i-2)/10))
  #binB[i] =  sum((B < (i-1)/10) & (B > (i-2)/10))
#}

print(binA)
print(binB)

p <- chisq.test(binA,binB,simulate.p.value = T)$p.value

print(p)