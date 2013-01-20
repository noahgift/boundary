                                                                     
                                                                     
                                                                     
                                             

# plots the boundary for mean Y = bval against (X1,X2); motivated by
# classification but can be used for continuous Y and regression

library(ggplot2)

# calls knn() from kNN.R

# y:  vector of responses
# x:  data for 2 predictors
# bval:  value from which the bounday is defined; default = mean Y
# bandhw:  all points having estimated response mean within bval
#    +/- bandhw*bval will be used to find the band containing the boundary
# k:  number of nearest neighbors
# nxs:  number of strips for partitioning the X1 space
# oldplot:  if not null, this is the saved previous plot, to which we
#    will now add

boundary <- function(y,x,bval=NULL,bandhw=0.1,k=25,nxs=50,oldplot=NULL) {
   if (is.null(bval)) bval <- mean(y)
   # find estimated mean Y at all data points
   eyhat <- knn(x,y,k)$regvals
   dfx <- data.frame(x,eyhat)
   tol <- bandhw * bval
   # find indices of the points in the band around the boundary
   bandpts <- which(abs(eyhat - bval) < tol)
   if (length(bandpts) == 0) stop("empty band")
   # find centroids
   cts <- matrix(nrow=nxs,ncol=2)
   xleft <- min(x[,1])
   xright <- max(x[,1])
   xbot <- min(x[,2])
   xtop <- max(x[,2])
   intw <- (xright - xleft) / nxs
   x1 <- x[,1]
   for (i in 1:nxs) {
      lo <- xleft + (i-1) * intw
      hi <- xleft + i * intw
      spts <- 
         bandpts[which(x1[bandpts] > lo & x1[bandpts] <= hi)]
      if (length(spts) > 0) {
         cts[i,] <- centroid(x,spts)
      } else cts[i,] <- NA
   }
   cts <- data.frame(cts)
   if (is.null(oldplot)) {
      ggplot(cts) + geom_line(aes(x=X1,y=X2)) 
                 # + xlim(c(xleft,xright)) + ylim(c(xbot,xtop))
   } else oldplot + geom_line(data=cts,aes(x=X1,y=X2))
}

centroid <- function(x,spts) {
   xs <- x[spts,]
   apply(xs,2,mean)
}

# x <- matrix(runif(20),ncol=2)
# y <- sample(0:1,10,replace=T)
# library(ggplot2)
# source("/home/nm/R/kNN.R")
# source("Boundary.R")
# boundary(y,x,k=3,nxs=2,bandhw=0.5)
# setBreakpoint("Boundary.R",31)

logit <- function(t) 1 / (1 + exp(-t))

geny <- function(xrow) {
   x1 <- xrow[1]
   x2 <- xrow[2]
   t <- x1 + x2 - 1
   # t <- x1 + x2 + 2*x1*x2 + 1
   y <- as.integer(runif(1) < logit(t))
}

sim <- function() {
   x <- matrix(runif(10000),ncol=2)
   y <- apply(x,1,geny)
   boundary(y,x,nxs=10,k=80,bandhw=0.1)
}
