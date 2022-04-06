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

myKM <- km(design = X0, response = y)

nIter <- 5

## =============================================================================
## We will simply use (the updated versions of ) 'myKM' to store the iterates.
## We can retrieve these as myKM@X
## =============================================================================

DO <- FALSE
for (iter in 1:nIter) {
    ## 1 find a 'new' input :
    if (DO){
        resOpt <- DiceOptim::max_EI(model = myKM, lower = lower, upper = upper)
    } else {
        resOpt <- dolka::max_EI_cmaes(model = myKM, lower = lower,
                                      upper = upper)
    }
    newX <- resOpt$par
    
    ## 2 evaluate f at the new input : XXX fill below
    newy <- f(newX)

    ## 3 update the model object : XXX fill below
    myKM <- update(myKM, newX = newX, newy = newy, newnoise.var = 1e-8)
}

## =============================================================================
## contours of the kriging sd of the current object
## =============================================================================
g0 <- contours(object = myKM, which = "sd") + 
    ggtitle("Kriging sd at the last iteration  (empty bullets = initial)")
g0 

## =============================================================================
## contours of the EI for the current object.
## =============================================================================
g1 <- contours(object = myKM, which = NULL, grad = TRUE, other = "EI",
               otherGrad = "EI.grad") +
    geom_point(data = data.frame(X0),
               mapping = aes(x = x1, y = x2), shape = 23,
               fill = "white", size = 2) +
    ggtitle("EI criterion at the last iteration (empty bullets = initial)")

if (myKM@n > n0) {
    g1 <-  g1 + geom_label(data = data.frame(myKM@X[-(1:n0), ],
                                             lab = (n0 + 1):myKM@n - n0),
                           mapping = aes(x = x1, y = x2, label = lab)) 
}

g1

## =============================================================================
## Contours of the branin function. A square-root scale helps at evaluating
## the scale.
## =============================================================================

lbranin <- function(x) sqrt(branin(x))

g2 <- contours(object = myKM, which = NULL, other = "lbranin") +
    geom_point(data = data.frame(X0),
               mapping = aes(x = x1, y = x2), shape = 23,
               fill = "white", size = 2) +
    ggtitle("log(branin) and iterates (empty bullets = initial)")

if (myKM@n > n0) {
    g2 <- g2 +  geom_label(data = data.frame(myKM@X[-(1:n0), ],
                                             lab = (n0 + 1):myKM@n - n0),
                           mapping = aes(x = x1, y = x2, label = lab)) 
}
g2

## The value of 'f' at the three minima
## see ?branin
Xmin <- rbind(c(0.9616520, 0.15), c(0.1238946, 0.8166644), c(0.5427730, 0.15))
apply(Xmin, 1, FUN = branin)
