boundary
========

norm-noah boundary
-----------------------

Example: 1
-----------------

Data obtained data from 
  http://archive.ics.uci.edu/ml/datasets/Abalone
  
```{r }
## I added a header to start of .data file (no #)
# Sex,Length,Diam,Ht,WWt,SWt,VWt,Shell,Rings
ab <- read.csv("abalone.data",header=T)
library(ggplot2)
# add numeric code for male
ab$Male <- as.integer(ab$Sex == "M")

# for convenience, extracted some columns of interest
dt <- ab[,c(2,5,9,10)]

# used nearest-neighbor method to estimate probability of male at each
# combination of length and whole weight
source("/home/nm/R/kNN.R")
kout <- knn(ab[,c(2,5)],ab[,10],50,regtype="cls")

# added the estimated probabilities to dt
dt$regvals <- kout$regvals

# loaded the GGPlot2 library, a highly popular graphics package for R
library(ggplot2)

# ran scatter plot of weight vs. length, color-coded by estimated
# probability of male
ggplot(dt, aes(dt$Length,dt$WWt,dt$regvals,colour=dt$regvals)) + geom_point(size=1.5)
```

The visualization for the abalone data will look like this

![Abalone](https://raw.github.com/noahgift/boundary/master/images/abalone.png)
