#Simulation Runner for Boundary
library(parallel)
source("Boundary.R")

bstrapsnow <-
   function(csize=4) 
{
      shmcls <- function(k) makeCluster(type = "SOCK", rep("localhost", k))
      shmcls(csize)
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
   function(sim, css, bandhw, k, oldplot)
{
   if (missing(css)) css <- bstrapsnow()
   boundary(css,4,sim[,3], sim[,1:2], bandhw=bandhw, k=k, oldplot=oldplot)
}

#This is the entry point to run a simulation
runsim <-
   function(sim, bandhw=0.4,k=4, oldplot=NULL) 
{
      if (missing(sim)) sim <-sim(n,d)
      addplot(sim, bandhw=bandhw, k=k, oldplot=oldplot)
}

##K Values######
#Example Run For N=10000
#For deriving K graph
bet <- 0.6
dta <- sim(10000, bet)
p1 <- runsim(dta, bandhw=0.4)
p1a <- p1 + annotate("text", x=0.01, y=.637, label="k=4")
p2 <- runsim(dta, k=40, oldplot=p1a)
p2a <- p2 + annotate("text", x=0.01, y=0.850, label="k=40")
p3 <- runsim(dta, k=400, oldplot=p2a)
p3a <- p3 + annotate("text", x=0.01, y=0.77, label="k=400")
pf <- p3a + annotate("text", x=.5, y=0.2, label="N=10000, prodcoeff=0.6")
#True Curve
b <- mean(dta[,3])
pff <- pf + stat_function(fun=function(x) (3*b-x)/(1+bet*x))
pffa <- pff + annotate("text", x=.035, y=1.2, label="True Curve")


##K Values######
#Example Run For N=100000
#For deriving K graph
#Still Crashes on 64 bit R on OS X
bet <- 0.6
dta <- sim(100000, bet)
p1 <- runsim(dta, bandhw=0.4)
p1a <- p1 + annotate("text", x=0.01, y=.637, label="k=4")
p2 <- runsim(dta, k=40, oldplot=p1a)
p2a <- p2 + annotate("text", x=0.01, y=0.850, label="k=40")
p3 <- runsim(dta, k=400, oldplot=p2a)
p3a <- p3 + annotate("text", x=0.01, y=0.77, label="k=400")
pf <- p3a + annotate("text", x=.5, y=0.2, label="N=100000, prodcoeff=0.6")
#True Curve
b <- mean(dta[,3])
pff <- pf + stat_function(fun=function(x) (3*b-x)/(1+bet*x))
pffa <- pff + annotate("text", x=.035, y=1.2, label="True Curve")


##BANDHW#####
#Example Run For N=10000
#For deriving bandhw graph
# bet <- 0.6
# dta <- sim(10000, bet)
# p1 <- runsim(dta, bandhw=0.5, k=40)
# p1a <- p1 + annotate("text", x=.1, y=0.76, label="bandhw=0.5")
# p2 <- runsim(dta, bandhw=.75, k=40, oldplot=p1a)
# p2a <- p2 + annotate("text", x=.1, y=0.625, label="bandhw=.75")
# p3 <- runsim(dta, bandhw=1, k=40, oldplot=p2a)
# p3a <- p3 + annotate("text", x=.1, y=0.53, label="bandhw=1")
# pf <- p3a + annotate("text", x=.5, y=0.2, label="N=10000, prodcoeff=0.6")
# #True Curve
# b <- mean(dta[,3])
# pff <- pf + stat_function(fun=function(x) (3*b-x)/(1+bet*x))
# pffa <- pff + annotate("text", x=.035, y=1.2, label="True Curve")

