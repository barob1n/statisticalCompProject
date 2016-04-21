# Group Project for StatComp
R code for the two-sample test procedures

# Layout
##Three main scripts:
  OneDimComparison.R = Compare 1 dim. data - Kolmogorov-Smirnov, Chi Sq. Test, MMD_u, MMD_l, PlotTest
  
  multiDimComparison.R = Compare multi dim data - for 2, 3, and 1000 dim data
  
  dataComparison.R = Compare approaches on nonstandard data
  
  getData.R = runs some of the 1dim stats, to be rolled into OneDimComparison
  permTest.R = ''
  input.R = ''
  
##Scripts for generating each type of data.
  Built in mvrnorm (using MASS), rnorm, rexp
  
##Test scripts
  Script for computing the statistic of a test.
  
  Script for automating the bootstrap/permutation method where appicable.
    TODO: separate this out from stuff below to keep the statistic definitions separate
    
    
  permTestBoot.R  = simpleStat , ks stat, nearest neighbor, hotel, and permutation functionality through boot strap
    nearNeighborBoot.R is a solo version for the nearest neighbor stat
  chiSq1.R = simple binning for some 1 dim normal comparison
  
  permTestStats.R contains ks stat, simpleStat, and the kernel stat
  
  compare_normals_via_plot.R, multiplot.R = plot comparison method
  
##Plot scripts
  Plot acceptance rate.
  Plot type 1 and type 2 errors.
  Plot run times.
  
  
