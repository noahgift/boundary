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
function(n,prodcoeff) 
{
   if (missing(n)) n <- 10000
   if (missing(prodcoeff)) prodcoeff <- 0.6
x <- matrix(runif(2*n),ncol=2)
geny <- function(xrow) {
   x1 <- xrow[1]
   x2 <- xrow[2]
   tmp <- runif(1) < (x1 + x2 + prodcoeff*x1*x2)/3
   as.integer(tmp)
}
y <- apply(x,1,geny)
cbind(x,y)
}

addplot <-
   function(sim1, sim2, css, bandhw, k, oldplot)
{
   if (missing(css)) css <- bstrapsnow()
   boundary(css,4,sim1[,"y"], sim2, bandhw=bandhw, k=k, oldplot=oldplot)
}

#This is the entry point to run a simulation
#o <- runsim(10000,0.6,4, bandhw=0.4,k=10)
runsim <-
   function(sim1, sim2, bandhw=0.4,k=4, oldplot=NULL) 
{
      if (missing(sim1)) sim1 <-sim(n,d)
      if (missing(sim2)) sim2 <-sim(n,d)
      addplot(sim1,sim2, bandhw=bandhw, k=k, oldplot=oldplot)
}

#Example Run
sim1 <- sim()
sim2 <- sim()
p1 <- runsim(sim1, sim2, bandhw=0.5)
p1a <- p1 + annotate("text", x=.1, y=0.482, label="bandhw=0.5")
p2 <- runsim(sim1, sim2, bandhw=4, oldplot=p1a)
p2a <- p2 + annotate("text", x=.1, y=0.488, label="bandhw=1.5")
p3 <- runsim(sim1, sim2, bandhw=1, oldplot=p2a)
p3a <- p3 + annotate("text", x=.1, y=0.502, label="bandhw=3")
p3a + annotate("text", x=.5, y=0.47, label="N=10000, prodcoeff=0.6")
