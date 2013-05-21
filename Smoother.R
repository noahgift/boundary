# applies a user-specified smoothing function to the given data set

# in the density estimation case, the density is evaluated on the same
# points as it is estimated from

# in the regression estimation case, estimation and evaluation will be
# on the same points if zpred is NULL; if zpred is not NULL, then the
# estimation will be done using z, and the evaluation on zpred

# arguments:
#    cls:  Snow cluster 
#    nchunks:  number of cluster nodes by default, but can be set to 
#       a larger number to take advantage of superlinearity; for load 
#       balance, should be a multiple of length(cls)
#    z:  data matrix/data frame, one observation per row; 
#       in regression case, last column is Y
#    sf:  smoothing function, knnreg() or knndens() 
#    zpred:  in regression case, points at which regression function 
#       is to be computed; if left as NULL, then set to z (except for
#       the Y column), i.e. estimation of the regression function and
#       its evaluation at data points are both done on z
# return value:
#    values of the smoothing function for each observation in z (or
#    zpred, as explained above)

# knnreg() and knndens() use k-nearest neighbor estimates, in order to
# take advantage of the fast (and already implemented) FNN package

library(parallel)

smoothz <- function(cls,nchunks,z,sf,k,zpred=NULL) {
   if (is.vector(z)) z <- matrix(z,ncol=1)
   if (is.data.frame(z)) z <- as.matrix(z)
   if (any(is.na(z))) stop("can't use FNN with NAs")
   if (!is.null(zpred)) {
      if (is.vector(zpred)) zpred <- matrix(zpred,ncol=1)
      if (is.data.frame(zpred)) zpred <- as.matrix(zpred)
      if (any(is.na(zpred))) stop("can't use FNN with NAs")
   }
   regr <- identical(sf,knnreg) || identical(sf,knnregx) 
   if (regr) {
      zp <- if (is.null(zpred)) z else zpred
      n <- nrow(zp)
   } else n <- nrow(z)
   # determine which observations each node will process
   idxchunks <- splitIndices(n,nchunks)
   if (regr) {
      zpchunks <- Map(function(ichunk) zp[ichunk,],idxchunks)
      tmp <- if (is.null(zpred) ) clusterApply(cls,zpchunks,knnreg,k) else
                                  clusterApply(cls,zpchunks,knnregx,k,z) 
   }  else {
      zchunks <- Map(function(ichunk) z[ichunk,],idxchunks)
      tmp <- clusterApply(cls,zchunks,sf,k)
   }
   Reduce(c,tmp)
}

# kNN regression; predict the points in data from those points
knnreg <- function(data,k) {
   require(FNN)
   ycol <- ncol(data)
   x <- data[,-ycol,drop=F]
   y <- data[,ycol]
   idx <- get.knn(data=x,k=k)$nn.index
   # i-th row of idx contains the indices of the k nearest neighbors to
   # that row of x (not including that row)
   apply(idx,1,function(idxrow) mean(y[idxrow]))
}

# same as knnreg() above, but evaluate the estimated regression function
# computed from data at the points in datapred; datapred and data must
# be matrices, not vectors
knnregx <- function(datapred,k,data) {
   require(FNN)
   ycol <- ncol(data)
   x <- data[,-ycol,drop=F]
   y <- data[,ycol]
   idx <- get.knnx(data=x,query=datapred,k=k)$nn.index
   # i-th row of idx contains the indices of the k nearest neighbors to
   # that row of x (not including that row)
   apply(idx,1,function(idxrow) mean(y[idxrow]))
}

# kNN density estimation
knndens <- function(data,k) {
   # finds kNN-based density estimates at the rows of data
   if (ncol(data) < 2) stop("must have at least 2-column data")
   library(FNN)
   dsts <- get.knn(data,k=k)$nn.dist
   hvec <- dsts[,k]
   (k/nrow(data)) / (pi * hvec^2)
}

gendata <- function(n,p) {
   x <<- matrix(rnorm(n*p),ncol=p)
   y <<- x%*%c(rep(0.5,p)) + rnorm(n)
}

# same as smoothz(), but only for regression, and with the regression
# values computed on a prediction set zpred
smoothzregpred <- function(cls,nchunks,z,sf,k,zpred) {
   if (is.vector(z)) z <- matrix(z,ncol=1)
   if (is.data.frame(z)) z <- as.matrix(z)
   if (any(is.na(z))) stop("can't use FNN with NAs")
   n <- nrow(z)
   if (is.null(nchunks)) nchunks <- length(cls)
   # determine which observations each node will process
   idxchunks <- splitIndices(n,nchunks)
   zchunks <- Map(function(ichunk) z[ichunk,],idxchunks)
   tmp <- clusterApply(cls,zchunks,sf,k)
   Reduce(c,tmp)
}

# kNN regression; same as knnreg(), but with datapred arg (see
# smoothzregpred() comments above)
knnreg <- function(data,k,datapred) {
   require(FNN)
   ycol <- ncol(data)
   x <- data[,-ycol,drop=F]
   y <- data[,ycol]
   idx <- get.knn(x,k=k)$nn.index
   # i-th row of idx contains the indices of the k nearest neighbors to
   # that row of x (not including that row)
   apply(idx,1,function(idxrow) mean(y[idxrow]))
}

