set.seed(3071)

N.1 <- 1000
N.2 <- 1000

mean.1 <- 0
mean.2 <- 0

sd.1 <- 1
sd.2 <- 1


#Set up latent variable
eta.1 <- rnorm(N.1, mean.1, sd.1)
eta.2 <- rnorm(N.2, mean.2, sd.2)


#LOADINGS
#Invariant loadings
load.inv <- .6
error.inv <-sqrt(1 - load.inv^2) #This ensures variance will be 1 (provided SD given above is 1)

#Noninvariant loadings
load.noninv.g2 <- .9
error.noninv.g2 <- sqrt(1 - load.noninv.g2^2) #This ensures variance will be 1 (provided SD given above is 1)

#INTERCEPT
#Invariant intercept
int.inv <- .3

#Noninvariant intercept
int.noninv.g2 <- -.5

#Group 1
y1.1 <- load.inv*eta.1 + int.inv + error.inv*rnorm(N.1, 0, 1)
y2.1 <- load.inv*eta.1 + int.inv + error.inv*rnorm(N.1, 0, 1)
y3.1 <- load.inv*eta.1 + int.inv + error.inv*rnorm(N.1, 0, 1)
y4.1 <- load.inv*eta.1 + int.inv + error.inv*rnorm(N.1, 0, 1)
y5.1 <- load.inv*eta.1 + int.inv + error.inv*rnorm(N.1, 0, 1)
y6.1 <- load.inv*eta.1 + int.inv + error.inv*rnorm(N.1, 0, 1) #The noninvariant item

#Group 2
y1.2 <- load.inv*eta.2 + int.inv + error.inv*rnorm(N.1, 0, 1)
y2.2 <- load.inv*eta.2 + int.inv + error.inv*rnorm(N.1, 0, 1)
y3.2 <- load.inv*eta.2 + int.inv + error.inv*rnorm(N.1, 0, 1)
y4.2 <- load.inv*eta.2 + int.inv + error.inv*rnorm(N.1, 0, 1)
y5.2 <- load.inv*eta.2 + int.inv + error.inv*rnorm(N.1, 0, 1)
y6.2 <- load.noninv.g2*eta.2 + int.inv + error.noninv.g2*rnorm(N.1, 0, 1) #The noninvariant item




