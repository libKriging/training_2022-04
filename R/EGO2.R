## *****************************************************************************
## AUTHOR: Yves Deville
##
## GOAL: dolka example for 2022-04-07 training session.
##
## A few iterations in a basic EGO.
##
## Hints: once the algorithm works, experiment on changing the kriging object
## class, on changing the optimizer for the EI, ...
## 
## *****************************************************************************

library(DiceKriging)
library(dolka)
library(rlibkriging)

f <- DiceKriging::branin
d <- 2
lower <- rep(0.0, d)
upper <- rep(1.0, d)

set.seed(123)
X0 <- as.matrix(expand.grid(x1 = seq(0.0, 1.0, len = 3),
                            x2 = seq(0.0, 1.0, len = 3)))
n0 <- nrow(X0)
## only for the plot...
colnames(X0) <- c("x1", "x2")
y <- apply(X0, MARGIN = 1, f)

myKM <- KM(design = X0, response = y)

nIter <- 5

## =============================================================================
## We will simply use (the updated versions of ) 'myKM' to store the iterates.
## We can retrieve these as myKM@X
## =============================================================================

for (iter in 1:nIter) {
    ## 1 find a 'new' input :
    resOpt <- dolka::max_EI_cmaes(model = myKM, lower = lower,
                                  upper = upper)
    
    newX <- resOpt$par
    
    ## 2 evaluate f at the new input : XXX fill below
    newy <- f(newX)

    ## 3 update the model object : XXX fill below
    myKM <- update(myKM, newX = newX, newy = newy)
}

