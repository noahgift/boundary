# avoids the "black screen problem" by plotting the kernel-estimated
# density of (X1,X2), color-coding the density heights 

# uses uniform kernel, Manhattan (L1) distance; default bandwidth is
# 0.01 * range(X1)

# x:  (X1,X2) data 
# h:  kernel bandwidth

twodimdens <- function(x,h=NULL) {
   if (is.null(h)) h <- 0.01 * (max(x[,1]) - min(x[,1]))
   estdens <- apply(x,1,unikerpt,x,h)
   dfx <- data.frame(x,estdens)
   # for now; change later, e.g. via evalq()
   names(dfx) <- c("X1","X2","estdens")  
   ggplot(dfx,aes(X1,X2,estdens,colour=estdens)) + geom_point(size=1.8)
}

# uniform kernel estimate at pt, based on data x, s
unikerpt <- function(pt,x,h) {
   manhat <- function(xrow,pt) sum(abs(xrow-pt))
   dists <- apply(x,1,manhat,pt)
   (sum(dists < 2*h) / nrow(x)) / h^2
}

# x <- matrix(runif(20),ncol=2)
# library(ggplot2)
# source("NonBlackScreen.R")
# twodimhist(x)

sim <- function() {
   x <- matrix(rnorm(20000),ncol=2)
   twodimhist(x)
}
