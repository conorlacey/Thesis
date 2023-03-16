set.seed(55555)

library(dmacs)

eta <- seq(-1000,1000, by = .1) #Generate true scores
load.R <- .6 #loading for the reference group
load.F <- 1.2 #loading for focal group
int.R <- 0 #intercept for reference group
int.F <- 1 #intercept for focal group

y.R <- load.R*eta + int.R #model-implied scores for reference group
y.F <- load.F*eta + int.F #model-implied scores for focal group

sd.pooled.test.num <- ((length(y.R)-1)*sd(y.R)) + ((length(y.F)-1)*sd(y.F)) 
sd.pooled.test.denom <- (length(y.R) - 1) + (length(y.F) - 1)
sd.pooled.test <- sd.pooled.test.num/sd.pooled.test.denom

#Let's just calculate dMACS using the store-bought version, just to make sure our values line up
item_dmacs(.6, #loading, R 
      1.2, #loading, F
      0, #intercept, R
      1, #intercept, F
      mean(eta), #mean (both groups)
      var(eta), #variance (both groups, but it would be the focal group if they didn't have the same variance)
      sd.pooled.test #pooled standard deviation (again, this is basically arbitrary)
      )

#get a value of .9251793


#################################
### Our version of the dMACS ####
#################################


#The logic here is to set this up analogously to Bergh et al -- write a function for the dMACS, then update everything using the other functions
#So here's our homegrown frequentist dMACS.
#let's call "outcome.F" and "outcome.R" the predicted values of y in the reference and focal groups, respectively
#So in other words, this version of the function assumes you've already calculated these using the item parameters--
#its arguments aren't the loadings and intercepts but the model-implied trace line data points themselves
#as well as the latent variable (eta)


# dMACS.freq <- function(outcome.R, outcome.F, eta) {
#   sq.diff <- (outcome.R - outcome.F)^2 #Difference between trace lines, squared
#   f.eta <- dnorm(eta, mean(eta), sd(eta)) #Normal density of eta
#   f.eta <- f.eta/sum(f.eta) #Currently WRONG -- trying in vain to normalize the density of eta
#   inside.sqrt <- sum(sq.diff*f.eta) #Also probably wrong everything inside the square root sign -- i.e., the difference between trace lines integrated over the density of eta
#   sd.pooled.num <- ((length(outcome.R)-1)*sd(outcome.R)) + ((length(outcome.F)-1)*sd(outcome.F))
#   sd.pooled.denom <- (length(outcome.R)-1) + (length(outcome.F)-1)
#   sd.pooled <- sd.pooled.num/sd.pooled.denom
#   sqrt(inside.sqrt)/sd.pooled
# }

dMACS.freq <- function(outcome.R, outcome.F, eta) {
  sq.diff <- (outcome.R - outcome.F)^2 #Difference between trace lines, squared
  f.eta <- dnorm(eta, mean(eta), sd(eta)) #Normal density of eta
  f.eta <- f.eta/sum(f.eta) #Currently WRONG -- trying in vain to normalize the density of eta
  inside.sqrt <- sum(sq.diff*f.eta) #Also probably wrong everything inside the square root sign -- i.e., the difference between trace lines integrated over the density of eta
  sd.pooled.num <- ((length(outcome.R)-1)*sd(outcome.R)) + ((length(outcome.F)-1)*sd(outcome.F))
  sd.pooled.denom <- (length(outcome.R)-1) + (length(outcome.F)-1)
  sd.pooled <- sd.pooled.num/sd.pooled.denom
  sqrt(inside.sqrt)/sd.pooled
}

dMACS.freq(y.R,y.F, eta)

# My Edits ----------------------------------------------------------------

dMACS.freq <- function(outcome.R, outcome.F, eta) {
  lm.R <- lm(outcome.R ~ eta)$coefficients
  lm.F <- lm(outcome.F ~ eta)$coefficients
  load.R <- lm.R[2]
  int.R <- lm.R[1]
  load.F <- lm.F[2]
  int.F <- lm.F[2]
  integrand <- function(x){
    product<-(load.R*x + int.R) - (load.F*x + int.F)
    return((product^2)*dnorm(x,mean(eta), sd(eta)))
  }
  inside.sqrt<- integrate(integrand, (min(eta)-10000), (max(eta)+10000))$value
  sd.pooled.num <- ((length(outcome.R)-1)*sd(outcome.R)) + ((length(outcome.F)-1)*sd(outcome.F))
  sd.pooled.denom <- (length(outcome.R)-1) + (length(outcome.F)-1)
  sd.pooled <- sd.pooled.num/sd.pooled.denom
  sqrt(inside.sqrt)/sd.pooled
}
dMACS.freq(y.R,y.F, eta)



