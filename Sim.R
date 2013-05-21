#Simulation Runner for Boundary

library(parallel)
source("Boundary.R")

bstrapsnow <-
   function(csize) 
{
      shmcls <- function(k) makeCluster(type = "SOCK", rep("localhost", k))
      c2 <- shmcls(csize)
}

sim <- 
	function(n,prodcoeff) 
{
   	x <- matrix(runif(2*n),ncol=2)
   	geny <- function(xrow) {
      x1 <- xrow[1]
      x2 <- xrow[2]
      tmp <- runif(1) > (x1 + x2 + prodcoeff*x1*x2)/3
      as.integer(tmp)
   	}
   	y <- apply(x,1,geny)
   	cbind(x,y)
 }

runsim <-
   function(n,d, cs) 
{
      if (missing(n)) n <- 10000
      if (missing(d)) d <- 0.6
      if (missing(cs)) cs <- 4
      resx <- sim(n,d)
      resy <- sim(n,d)
      css <- bstrapsnow(cs)
      boundary(css,4, resx, resy)
}
