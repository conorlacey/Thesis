rm(list = ls())
library(dmacs)
library(tidyverse)
source("functions2.R")

# Built dMACS Function ------------------------------------------------------
get.dMACS <- function(outcome.R, outcome.F, eta.R = eta.R, eta.F = eta.F) {
  lm.R <- lm(outcome.R ~ eta.R)$coefficients
  lm.F <- lm(outcome.F ~ eta.F)$coefficients
  load.R <- lm.R[2]
  int.R <- lm.R[1]
  load.F <- lm.F[2]
  int.F <- lm.F[1]
  integrand <- function(x){
    product<-(load.R*x + int.R) - (load.F*x + int.F)
    return((product^2)*dnorm(x,mean(eta.F), sd(eta.F)))
  }
  inside.sqrt<- integrate(integrand, -Inf, Inf)$value
  sd.pooled.num <- ((length(outcome.R)-1)*sd(outcome.R)) + ((length(outcome.F)-1)*sd(outcome.F))
  sd.pooled.denom <- (length(outcome.R)-1) + (length(outcome.F)-1)
  sd.pooled <- sd.pooled.num/sd.pooled.denom
  sqrt(inside.sqrt)/sd.pooled
}

# Fake Data ---------------------------------------------------------------
set.seed(1572)

N.R <- 250
N.F <- 250  
load.R <- .5
load.F <- .3
int.R <- 1
int.F <- .8
priorPH0 <- .5

sd.R <- 1
sd.F <- 1

eta.R <- rnorm(N.R, 0, sd.R)
eta.F <- rnorm(N.F, 1, sd.F)

error.R <- sqrt(1-load.R^2)
error.F <- sqrt(1-load.F^2)

y.R <- load.R*eta.R + int.R + error.R*rnorm(N.R,0,sd.R) #Observed Scores
y.F <- load.F*eta.F + int.F + error.F*rnorm(N.F,1,sd.F) #Observed Scores

datR <- data.frame(eta = eta.R, y = y.R, group = "R")
datF <- data.frame(eta = eta.F, y = y.F, group = "F")
dat  <- bind_rows(datR,datF)

dat %>% ggplot(aes(x = eta, y = y, color = group)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(x = "Eta",
       y = "Estimated Response") +
  geom_line(data = datF,
            aes(x = eta, y = dnorm(eta, mean(eta), sd(eta)), color = NULL)) 

sd.pooled.test.num <- ((length(y.R)-1)*sd(y.R)) + ((length(y.F)-1)*sd(y.F))
sd.pooled.test.denom <- (length(y.R) - 1) + (length(y.F) - 1)
sd.pooled.test <- sd.pooled.test.num/sd.pooled.test.denom

get.dMACS(y.R, y.F, eta.R, eta.F)

# dMACS OG ----------------------------------------------------------------
lm.R <- lm(y.R ~ eta.R)$coefficients
lm.F <- lm(y.F ~ eta.F)$coefficients
load.R <- lm.R[2]
int.R <- lm.R[1]
load.F <- lm.F[2]
int.F <- lm.F[1]

item_dmacs(load.R, #loading, R
                    load.F, #loading, F
                    int.R, #intercept, R
                    int.F, #intercept, F
                    mean(eta.F), #mean (both groups)
                    var(eta.F), #variance (both groups, but it would be the focal group if they didn't have the same variance)
                    sd.pooled.test #pooled standard deviation (again, this is basically arbitrary)
)

# dMACS_Shrunk ------------------------------------------------------------
sigmaSlab <- 1

ybarExpl <- get.dMACS(y.R, y.F, eta.R, eta.F)
ybarExpl

nExpl <- length(y.R)

upMAExpl <- updatePar(priorPH0, sigmaSlab, nExpl, ybarExpl[1L])
ciMAExpl <- postStat(upMAExpl)

tbExplicit <- data.frame(t(c(upMAExpl, ciMAExpl)))
names(tbExplicit) <- c("ph0", "mu1", "sd1", "Lower", "Upper", "modelAveraged")

dMACS_Shrunk <- tbExplicit[2]*(1-tbExplicit[1])
dMACS_Shrunk

