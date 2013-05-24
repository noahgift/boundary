#Simulation Runner for Boundary
library(parallel)
source("Boundary.R")

bstrapsnow <-
   function(csize=4) 
{
      shmcls <- function(k) makeCluster(type = "SOCK", rep("localhost", k))
      c2 <- shmcls(csize)
}

sim <- 
	function(n=10000,prodcoeff=0.6) 
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

addplot <-
   function(simout1, simout2, prevplot=NULL, bandhw=0.2, k=4)
{
   css <- bstrapsnow()
   boundary(css,4,simout1[,"y"], simout2, bandhw=bandhw, k=k, oldplot=prevplot)
}

#fiveruns <-
#   function
#

#This is the entry point to run a simulation
#o <- runsim(10000,0.6,4, bandhw=0.4,k=10)
runsim <-
   function(n,d, cs,bandhw=0.2,k=4, oldplot=NULL) 
{
      if (missing(n)) n <- 10000
      if (missing(d)) d <- 0.6
      if (missing(cs)) cs <- 4
      resx <- sim(n,d)
      resy <- sim(n,d)
      css <- bstrapsnow(cs)
      boundary(css,4, resy[,"y"], resx, bandhw=bandhw,k=k, oldplot=oldplot)
}

#p1 + annotate("text", x=.5, y=.51, label="Bandhw=0.2")


#TO DO:
#1.  Create several k permutations:  1, 5, 10, 20, 40, 80, 160, 320, 640
#2.  Keep N at 10,000  
#3.  Create several bandhw permutations: 0.2, 0.4, 0.6, 0.8, 1, 1.5, 2, 4, 6
#
#



