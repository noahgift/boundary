# the function knn() does k-nearest neighbor regression; the user has a
# choice of either just fitting to the x,y dataset or using that data to
# predict new observations newobs for which only the predictors are
# known

# arguments:

# x:  matrix or data frame of the predictor variable data, one row per
#     observation
#
# y:  vector of the response variables corresponding to x; in the
#     classification case, these are assumed to be 1s and 0s
#
# k:  the number of nearest neighbors to use for estimating the regression
#     or predicting the new data
#
# newobs:  a matrix of values of the predictors, one row per observation,
#          on which to predict the responses; default value is NULL
#
# regtype:  "reg" for prediction of continuous variables, "cls" for
#           classification problems; default value "reg"
#

# return value: an R list with the following components
#
#    regvals:  estimated values of the regression function at x
#
#    predvals:  if newobs is not NULL, predicted values for y from newobs
#               otherwise NULL
#
#    predsuccess:  if newobs is NULL, then R^2 in the "reg" case, proportion 
#                  of correctly classified observations in the "cls" case; 
#                  otherwise NULL

library(RANN)  # fast nearest-neighbor finder on CRAN

knn <- function(x,y,k,newobs=NULL,regtype="reg") {
   # make sure x is a matrix or data frame for use with RANN
   if (is.vector(x)) x <- matrix(x,ncol=1)
   retval <- list()
   # just trying out on current data set?
   if (is.null(newobs)) {
      nearones <- nn2(data=x,k=k,query=x)$nn.idx
   } else {
      nearones <- nn2(data=x,k=k,query=newobs)$nn.idx
   }
   # row i of nearones now consists of the indices in x of the k closest
   # observations in x to row i of x or row i of newobs
   #
   # now find the estimated regression function at each row
   regvals <- apply(nearones,1,pred1y,y)
   if (is.null(newobs)) {
      if (regtype=="reg") {
         tmp <- cor(regvals,y)
         predsuccess <- tmp^2
      } else {
         predvals <- as.integer(regvals > 0.5)
         predsuccess <- mean(predvals == y)
      }
      predvals <- NULL
   } else {
      predsuccess <- NULL 
      newregvals <- apply(nearones,1,pred1y,y)
      if (regtype == "reg") predvals <- newregvals else {
         predvals <- as.integer(regvals > 0.5)  
      }
   }
   retval$regvals <- regvals
   retval$predvals <- predvals
   retval$predsuccess <- predsuccess
   retval
}

# for a single observation, calculate the value of the regression
# function there, knowing the indices xidxs of the values in the
# original data x that are closest to the given observation
pred1y <- function(xidxs,y) predval <- mean(y[xidxs])

