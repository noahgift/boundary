                                                                     
                                                                     
                                                                     
                                             

# avoids the "black screen problem" by plotting the kernel-estimated
# density of (X1,X2), color-coding the density heights 

# uses uniform kernel, Manhattan (L1) distance; default bandwidth is
# 0.01 * range(X1)

library(ggplot2)
library(Rlof)

# x:  (X1,X2) data 
# h:  kernel bandwidth
# cls:  snow cluster, if present

twodimdens <- function(x,h=NULL,cls=NULL) {
   if (is.null(h)) h <- 0.01 * (max(x[,1]) - min(x[,1]))
   if (is.null(cls)) {
      #  estdens <- apply(x,1,unikerpt,x,h)
      estdens <-  serestdens(x,h)
   } else estdens <-  parestdens(cls,x,h)
   dfx <- data.frame(x,estdens)
   # for now; change later, e.g. via evalq()
   names(dfx) <- c("X1","X2","estdens")  
   ggplot(dfx,aes(X1,X2,estdens,colour=estdens)) + geom_point(size=1.8)
}

serestdens <- function(x,h) {
   n <- nrow(x)
   returnval <- vector(length=n)
   dsts <- distmc(x,method="manhattan")
   dsts <- as.matrix(dsts)
   twoh <- 2 * h
   area <- 4 * h^2
   for (i in 1:n) {
      returnval[i] <- (sum(dsts[i,] < twoh) / n) / area
   }
   returnval
}

# uniform kernel estimate at pt, based on data x, s
unikerpt <- function(pt,x,h) {
   manhat <- function(xrow,pt) sum(abs(xrow-pt))
   dists <- apply(x,1,manhat,pt)
   (sum(dists < 2*h) / nrow(x)) / h^2
}

sim <- function() {
   # x <- matrix(rnorm(8000),ncol=2)
   n <- 10000
   n2 <- 2 * n
   xa <- matrix(rnorm(n2),ncol=2)   
   xb <- matrix(rnorm(n2,mean=0.5,sd=1.5),ncol=2)   
   pop1 <- sample(1:n,n,replace=F)
   pop2 <- setdiff(1:n,pop1)
   x <- rbind(xa[pop1,],xb[pop2])
   xx <- x[abs(x[,1]) < 1 & abs(x[,2]) < 1,]
   twodimdens(xx)
}
