################################################################################
#NearNeighbor: performs two sample test via nearest-neighbor method
#
#Note: Will need the following packages installed and loaded:
#      install.packages("FNN")
#      library(FNN)
#      library(boot)
################################################################################

#generate a small data set
attach(chickwts)
x <- as.vector(weight[feed == "sunflower"])
y <- as.vector(weight[feed == "linseed"])
detach(chickwts)
z <- c(x, y)
o <- rep(0, length(z))
z <- as.data.frame(cbind(z, o))


#Defining the nearest neighnor function for k = 3
Tn3 <- function(z, ix, sizes) {
  n1 <- sizes[1]
  n2 <- sizes[2]
  n <- n1 + n2
  z <- z[ix, ]
  o <- rep(0, NROW(z))
  z <- as.data.frame(cbind(z, o))
  NN <- get.knn(z, k=3)
  block1 <- NN$nn.index[1:n1, ]
  block2 <- NN$nn.index[(n1+1):n, ]
  i1 <- sum(block1 < n1 + .5)
  i2 <- sum(block2 > n1 + .5)
  return((i1 + i2) / (3 * n))
}

N <- c(12, 12)
boot.obj <- boot(data = z, statistic = Tn3,
                 sim = "permutation", R = 999, sizes = N)




