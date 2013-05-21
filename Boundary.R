
# compute the contour for conditional mean Y = bval against 2 specified
# predictors X1,X2 in X; motivated by classification--"on this side of
# the boundary curve you guess yes, on the other side you guess no"--but
# can be used for continuous Y and regression, in which case one can
# plot contours of (X1,X2) pairs corresponding to given values of the
# regression function

# returns the ggplot2 object for plotting the boundary against the
# specified predictors; one of the inputs can be an old such plot, to
# which a new boundary will be added

# prints a few random data points, with estimated conditional mean Y
# values, to help identify which side of the boundary is in the
# "positive" (increasing conditional mean Y) direction

# method:  the estimated conditional mean Y values are computed at each
# point, using a nearest-neighbors method; a band is formed around the
# boundary; the boundary band is smoothed into a curve

# parallel computation is used, via the R Snow package

library(ggplot2)

source("Smoother.R")

# y:  Y vector
# x:  (X1,X2) matrix or data frame
# bval:  value from which the boundary is defined; default is estimated
#    overall (unconditional) mean Y
# bandhw:  determines width of band around boundary--all points 
#    having estimated conditional Y mean within bval +/- bandhw*bval; 
#    default is 0.2
# k:  number of nearest neighbors; default is square root of the number
#    of observations
# oldplot:  if not null, this is the saved previous plot, to which we
#    will now add
# xlb:  optional label for the horizontal axis
# ylb:  optional label for the vertical axis
# cls:  Snow cluster
# nchunks:  number of chunks; see Smoother.R

boundary <- 
   function(cls,nchunks,y,x,bval=NULL,bandhw=0.2,k=NULL,
      oldplot=NULL,xlb=NULL,ylb=NULL,clr="#FF0000")
{
   if (is.null(bval)) bval <- mean(y)
   if (is.null(k)) k <- ceiling(sqrt(nrow(x)))
   # find estimated mean Y at all data points for X1X2 
   eyhat <- smoothz(cls,nchunks,cbind(x,y),knnreg,k)
   tol <- bandhw * bval
   # find indices of the points in the band around the boundary
   bandpts <- which(abs(eyhat - bval) < tol)
   if (length(bandpts) == 0) stop("empty band")
   # prepare to plot boundary
   x1 <- x[,1]  # "horizontal" variable
   x1band <- x1[bandpts]
   x2 <- x[,2]  # "vertical" variable
   x2band <- x2[bandpts]
   x12 <- data.frame(x1b=x1band,x2b=x2band)
   newplot <- 
      if (is.null(oldplot)) {
         ggplot(x12,aes(x1b,x2b)) + geom_smooth(stat="smooth",colour=clr)
      } else oldplot + geom_smooth(data=x12,stat="smooth",colour=clr)
   if (!is.null(xlb)) newplot <- newplot + xlab(xlb)
   if (!is.null(ylb)) newplot <- newplot + ylab(ylb)
   print(prcomp(x[bandpts,]))
   print("some random points:")
   rx <- sample(1:nrow(x),5,replace=F)
   tmpx <- x[rx,]
   tmpy <- eyhat[rx]
   print(cbind(tmpx,tmpy))
   newplot
}

pc2 <- function(x) {
   pc12 <- prcomp(x)$rotation[,1:2]
   # x might be a data frame
   as.matrix(x) %*% pc12

}