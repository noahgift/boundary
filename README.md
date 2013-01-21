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
# add numeric code for male
ab$Male <- as.integer(ab$Sex == "M")

# for convenience, extracted some columns of interest
dt <- ab[,c(2,5,9,10)]

# used nearest-neighbor method to estimate probability of male at each
# combination of length and whole weight
source("kNN.R") #This assumes a relative path to the checkout
kout <- knn(ab[,c(2,5)],ab[,10],50,regtype="cls")

# added the estimated probabilities to dt
dt$regvals <- kout$regvals

# loaded the GGPlot2 library, a highly popular graphics package for R
library(ggplot2)

# ran scatter plot of weight vs. length, color-coded by estimated
# probability of male
ggplot(dt, aes(dt$Length,dt$WWt,dt$regvals,colour=dt$regvals)) + geom_point(size=1.5)
```

The visualization for the abalone data will look like this:

![Abalone](https://raw.github.com/noahgift/boundary/master/images/abalone.png)

The boundary code though, can visualize the boundary when the probability of male is greater than .5.
To generate this, you would do the following:

```{r }
boundary(ab$Male,ab[,c(2,5)],k=3)
```
The new visualization for the abalone data probability boundary will look like this:

![Abalone](https://raw.github.com/noahgift/boundary/master/images/boundary.png)
