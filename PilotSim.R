# Libraries ---------------------------------------------------------------
suppressWarnings(library(tidyverse))
library(dmacs)

# Functions ---------------------------------------------------------------
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

erfcinv <- function(x) qnorm(x / 2, lower.tail = FALSE) / sqrt(2)

pss <- function(x, par) {
  return((x > 0) * par[1] + (1 - par[1]) * pnorm(x, par[2], par[3]))
}

inverseCdf <- function(p, par) {
  
  thresh0 <- pss(0, par)
  thresh1 <- pss(0 + 1e-100, par)
  out <- numeric(length(p))
  
  idx1 <- p < thresh0 | p > thresh1 # where F(p) != 0
  idx0 <- p[idx1] >= thresh1 # where F(p) < 0
  
  out[idx1] <- par[2] - sqrt(2) * par[3] * erfcinv(2 * (par[1]*idx0 - p[idx1]) / (par[1] - 1))
  return(out)
}

postStat <- function(par) {
  # lower <- myQ(.025, par=par)
  # error(lower, 0.025, par) > error(0.1, 0.025, par)
  # diff <- 0.025 - pss(lower, par)
  # upper <- myQ(.975 - diff, par)
  # browser()
  # curve(pss(x, par), from = -2, to = 2, n = 2^12)
  lower <- inverseCdf(0.025, par)
  diff <- 0.025 - pss(lower, par)
  upper <- inverseCdf(0.975 - diff, par)
  if (upper == 0) upper <- upper + 1e-100
  diff <- 0.975 - pss(upper, par)
  lower <- inverseCdf(0.025 - diff, par)
  pss(upper, par) - pss(lower, par)
  me <- par[2] * (1 - par[1])
  return(c("lower" = lower, "upper" = upper, "model averaged" = me))
}

bf <- function(d, n, v0) {
  a <- 1 / sqrt(n * (v0 + 1))
  b <- n^2 * d^2
  c <- 2 * (n + 1 / v0)
  return(a * exp(b / c))
}

updatePar <- function(rho0, v0, N, ybar) {
  prec <- (N + 1 / v0)
  v1 <- 1 / prec
  c <- N * ybar
  mu1 <- v1 * c
  w <- bf(ybar, N, v0)
  rho1 <- rho0 / (rho0 + (1 - rho0) * w)
  return(c("ph0" = rho1, "mu" = mu1, "sd" = sqrt(v1)))
}


# Simulations -------------------------------------------------------------
# measures <- matrix(nrow = 1000, ncol = 2) %>% data.frame() %>% 
#   setNames(c("dMACS", "dMACS_Shrunk"))

set.seed(7029)

N.R <- 1000
N.F <- 1000

mean.R <- 0
mean.F <- 1

sd.R <- 1
sd.F <- 1

#Set up latent variable
eta.R <- rnorm(N.R, mean.R, sd.R)
eta.F <- rnorm(N.F, mean.F, sd.F)

#eta <- seq(-1000,1000, by = 10) #Generate true scores
load.R <- .6 #loading for the reference group
load.F <- 1.2 #loading for focal group
int.R <- 0 #intercept for reference group
int.F <- 1 #intercept for focal group

y.R <- load.R*eta.R + int.R #model-implied scores for reference group
y.F <- load.F*eta.F+ int.F #model-implied scores for focal group

datR <- data.frame(eta = eta.R, y = y.R, group = "R")
datF <- data.frame(eta = eta.F, y = y.F, group = "F")
dat  <- bind_rows(datR,datF) 

dat %>% ggplot(aes(x = eta, y = y, color = group)) +
  geom_line(linewidth = 1) + 
  labs(x = "Eta",
       y = "Estimated Response") + 
  geom_line(data = datF, 
            aes(x = eta, y = dnorm(eta, mean(eta), sd(eta)), color = NULL),
            linetype = "dashed") + 
  xlim(-3,3) +
  ylim(0,1.2)



sd.pooled.test.num <- ((length(y.R)-1)*sd(y.R)) + ((length(y.F)-1)*sd(y.F)) 
sd.pooled.test.denom <- (length(y.R) - 1) + (length(y.F) - 1)
sd.pooled.test <- sd.pooled.test.num/sd.pooled.test.denom

#dMACS
dMACS <- item_dmacs(load.R, #loading, R 
           load.F, #loading, F
           int.R, #intercept, R
           int.F, #intercept, F
           mean(eta.F), #mean (both groups)
           var(eta.F), #variance (both groups, but it would be the focal group if they didn't have the same variance)
           sd.pooled.test #pooled standard deviation (again, this is basically arbitrary)
)

dMACS

#dMACS_Shrunk
priorPH0  <- 0.5
sigmaSlab <- 1

ybarExpl <- get.dMACS(y.R, y.F, eta.R, eta.F)
ybarExpl

nExpl <- length(y.R)

upMAExpl <- updatePar(priorPH0, sigmaSlab, nExpl, ybarExpl[1L])
ciMAExpl <- postStat(upMAExpl)

tbExplicit <- data.frame(t(c(upMAExpl, ciMAExpl)))
names(tbExplicit) <- c("ph0", "mu1", "sd1", "Lower", "Upper", "modelAveraged")

dMACS_Shrunk <- (1-tbExplicit[1])*tbExplicit[2]

dMACS_Shrunk

# measures$dMACS_Shrunk <- measures$dMACS_Shrunk %>% as.numeric()
# cor(measures$dMACS,measures$dMACS_Shrunk)



# dMACS Conditions --------------------------------------------------------
{
## dMACS = .10 ------------------------------
{
### Means Equal  --------------
set.seed(7029)

N.R <- 1000
N.F <- 1000

mean.R <- 0
mean.F <- 0

sd.R <- 1
sd.F <- 1

#Set up latent variable
eta.R <- rnorm(N.R, mean.R, sd.R)
eta.F <- rnorm(N.F, mean.F, sd.F)

#eta <- seq(-1000,1000, by = 10) #Generate true scores
load.R <- .6 #loading for the reference group
load.F <- .634 #loading for focal group
int.R <- 0 #intercept for reference group
int.F <- 0.05 #intercept for focal group

y.R <- load.R*eta.R + int.R #model-implied scores for reference group
y.F <- load.F*eta.F+ int.F #model-implied scores for focal group

datR <- data.frame(eta = eta.R, y = y.R, group = "R")
datF <- data.frame(eta = eta.F, y = y.F, group = "F")
dat  <- bind_rows(datR,datF) 

dat %>% ggplot(aes(x = eta, y = y, color = group)) +
  geom_line(linewidth = 1) + 
  labs(x = "Eta",
       y = "Estimated Response") + 
  geom_line(data = datF, 
            aes(x = eta, y = dnorm(eta, mean(eta), sd(eta)), color = NULL),
            linetype = "dashed") + 
  xlim(-3,3) +
  ylim(0,1.2)



sd.pooled.test.num <- ((length(y.R)-1)*sd(y.R)) + ((length(y.F)-1)*sd(y.F)) 
sd.pooled.test.denom <- (length(y.R) - 1) + (length(y.F) - 1)
sd.pooled.test <- sd.pooled.test.num/sd.pooled.test.denom

#dMACS
dMACS <- item_dmacs(load.R, #loading, R 
                    load.F, #loading, F
                    int.R, #intercept, R
                    int.F, #intercept, F
                    mean(eta.F), #mean (both groups)
                    var(eta.F), #variance (both groups, but it would be the focal group if they didn't have the same variance)
                    sd.pooled.test #pooled standard deviation (again, this is basically arbitrary)
)

dMACS

## Means Unequal  --------------
set.seed(7029)

N.R <- 1000
N.F <- 1000

mean.R <- 0
mean.F <- 1

sd.R <- 1
sd.F <- 1

#Set up latent variable
eta.R <- rnorm(N.R, mean.R, sd.R)
eta.F <- rnorm(N.F, mean.F, sd.F)

#eta <- seq(-1000,1000, by = 10) #Generate true scores
load.R <- .6 #loading for the reference group
load.F <- .61 #loading for focal group
int.R <- 0 #intercept for reference group
int.F <- 0.05 #intercept for focal group

y.R <- load.R*eta.R + int.R #model-implied scores for reference group
y.F <- load.F*eta.F+ int.F #model-implied scores for focal group

datR <- data.frame(eta = eta.R, y = y.R, group = "R")
datF <- data.frame(eta = eta.F, y = y.F, group = "F")
dat  <- bind_rows(datR,datF) 

dat %>% ggplot(aes(x = eta, y = y, color = group)) +
  geom_line(linewidth = 1) + 
  labs(x = "Eta",
       y = "Estimated Response") + 
  geom_line(data = datF, 
            aes(x = eta, y = dnorm(eta, mean(eta), sd(eta)), color = NULL),
            linetype = "dashed") + 
  xlim(-3,3) +
  ylim(0,1.2)



sd.pooled.test.num <- ((length(y.R)-1)*sd(y.R)) + ((length(y.F)-1)*sd(y.F)) 
sd.pooled.test.denom <- (length(y.R) - 1) + (length(y.F) - 1)
sd.pooled.test <- sd.pooled.test.num/sd.pooled.test.denom

#dMACS
dMACS <- item_dmacs(load.R, #loading, R 
                    load.F, #loading, F
                    int.R, #intercept, R
                    int.F, #intercept, F
                    mean(eta.F), #mean (both groups)
                    var(eta.F), #variance (both groups, but it would be the focal group if they didn't have the same variance)
                    sd.pooled.test #pooled standard deviation (again, this is basically arbitrary)
)

dMACS

}
# dMACS = .50 ------------------------------
{
## Means Equal  --------------
set.seed(7029)

N.R <- 1000
N.F <- 1000

mean.R <- 0
mean.F <- 0

sd.R <- 1
sd.F <- 1

#Set up latent variable
eta.R <- rnorm(N.R, mean.R, sd.R)
eta.F <- rnorm(N.F, mean.F, sd.F)

#eta <- seq(-1000,1000, by = 10) #Generate true scores
load.R <- .6 #loading for the reference group
load.F <- .61 #loading for focal group
int.R <- 0 #intercept for reference group
int.F <- 0.3 #intercept for focal group

y.R <- load.R*eta.R + int.R #model-implied scores for reference group
y.F <- load.F*eta.F+ int.F #model-implied scores for focal group

datR <- data.frame(eta = eta.R, y = y.R, group = "R")
datF <- data.frame(eta = eta.F, y = y.F, group = "F")
dat  <- bind_rows(datR,datF) 

dat %>% ggplot(aes(x = eta, y = y, color = group)) +
  geom_line(linewidth = 1) + 
  labs(x = "Eta",
       y = "Estimated Response") + 
  geom_line(data = datF, 
            aes(x = eta, y = dnorm(eta, mean(eta), sd(eta)), color = NULL),
            linetype = "dashed") + 
  xlim(-3,3) +
  ylim(0,1.2)



sd.pooled.test.num <- ((length(y.R)-1)*sd(y.R)) + ((length(y.F)-1)*sd(y.F)) 
sd.pooled.test.denom <- (length(y.R) - 1) + (length(y.F) - 1)
sd.pooled.test <- sd.pooled.test.num/sd.pooled.test.denom

#dMACS
dMACS <- item_dmacs(load.R, #loading, R 
                    load.F, #loading, F
                    int.R, #intercept, R
                    int.F, #intercept, F
                    mean(eta.F), #mean (both groups)
                    var(eta.F), #variance (both groups, but it would be the focal group if they didn't have the same variance)
                    sd.pooled.test #pooled standard deviation (again, this is basically arbitrary)
)

dMACS

# Means Unequal  --------------
set.seed(7029)

N.R <- 1000
N.F <- 1000

mean.R <- 0
mean.F <- 1

sd.R <- 1
sd.F <- 1

#Set up latent variable
eta.R <- rnorm(N.R, mean.R, sd.R)
eta.F <- rnorm(N.F, mean.F, sd.F)

#eta <- seq(-1000,1000, by = 10) #Generate true scores
load.R <- .6 #loading for the reference group
load.F <- .61 #loading for focal group
int.R <- 0 #intercept for reference group
int.F <- 0.29 #intercept for focal group

y.R <- load.R*eta.R + int.R #model-implied scores for reference group
y.F <- load.F*eta.F+ int.F #model-implied scores for focal group

datR <- data.frame(eta = eta.R, y = y.R, group = "R")
datF <- data.frame(eta = eta.F, y = y.F, group = "F")
dat  <- bind_rows(datR,datF) 

dat %>% ggplot(aes(x = eta, y = y, color = group)) +
  geom_line(linewidth = 1) + 
  labs(x = "Eta",
       y = "Estimated Response") + 
  geom_line(data = datF, 
            aes(x = eta, y = dnorm(eta, mean(eta), sd(eta)), color = NULL),
            linetype = "dashed") + 
  xlim(-3,3) +
  ylim(0,1.2)



sd.pooled.test.num <- ((length(y.R)-1)*sd(y.R)) + ((length(y.F)-1)*sd(y.F)) 
sd.pooled.test.denom <- (length(y.R) - 1) + (length(y.F) - 1)
sd.pooled.test <- sd.pooled.test.num/sd.pooled.test.denom

#dMACS
dMACS <- item_dmacs(load.R, #loading, R 
                    load.F, #loading, F
                    int.R, #intercept, R
                    int.F, #intercept, F
                    mean(eta.F), #mean (both groups)
                    var(eta.F), #variance (both groups, but it would be the focal group if they didn't have the same variance)
                    sd.pooled.test #pooled standard deviation (again, this is basically arbitrary)
)

dMACS

}
# dMACS = .90 ------------------------------
{
  ## Means Equal  --------------
  set.seed(7029)
  
  N.R <- 1000
  N.F <- 1000
  
  mean.R <- 0
  mean.F <- 0
  
  sd.R <- 1
  sd.F <- 1
  
  #Set up latent variable
  eta.R <- rnorm(N.R, mean.R, sd.R)
  eta.F <- rnorm(N.F, mean.F, sd.F)
  
  #eta <- seq(-1000,1000, by = 10) #Generate true scores
  load.R <- .6 #loading for the reference group
  load.F <- .606#loading for focal group
  int.R <- 0 #intercept for reference group
  int.F <- 0.55 #intercept for focal group
  
  y.R <- load.R*eta.R + int.R #model-implied scores for reference group
  y.F <- load.F*eta.F+ int.F #model-implied scores for focal group
  
  datR <- data.frame(eta = eta.R, y = y.R, group = "R")
  datF <- data.frame(eta = eta.F, y = y.F, group = "F")
  dat  <- bind_rows(datR,datF) 
  
  dat %>% ggplot(aes(x = eta, y = y, color = group)) +
    geom_line(linewidth = 1) + 
    labs(x = "Eta",
         y = "Estimated Response") + 
    geom_line(data = datF, 
              aes(x = eta, y = dnorm(eta, mean(eta), sd(eta)), color = NULL),
              linetype = "dashed") + 
    xlim(-3,3) +
    ylim(0,1.2)
  
  
  
  sd.pooled.test.num <- ((length(y.R)-1)*sd(y.R)) + ((length(y.F)-1)*sd(y.F)) 
  sd.pooled.test.denom <- (length(y.R) - 1) + (length(y.F) - 1)
  sd.pooled.test <- sd.pooled.test.num/sd.pooled.test.denom
  
  #dMACS
  dMACS <- item_dmacs(load.R, #loading, R 
                      load.F, #loading, F
                      int.R, #intercept, R
                      int.F, #intercept, F
                      mean(eta.F), #mean (both groups)
                      var(eta.F), #variance (both groups, but it would be the focal group if they didn't have the same variance)
                      sd.pooled.test #pooled standard deviation (again, this is basically arbitrary)
  )
  
  dMACS
  
  # Means Unequal  --------------
  set.seed(7029)

  N.R <- 1000
  N.F <- 1000

  mean.R <- 0
  mean.F <- 1

  sd.R <- 1
  sd.F <- 1

  #Set up latent variable
  eta.R <- rnorm(N.R, mean.R, sd.R)
  eta.F <- rnorm(N.F, mean.F, sd.F)

  #eta <- seq(-1000,1000, by = 10) #Generate true scores
  load.R <- .6 #loading for the reference group
  load.F <- .595 #loading for focal group
  int.R <- 0 #intercept for reference group
  int.F <- 0.55 #intercept for focal group

  y.R <- load.R*eta.R + int.R #model-implied scores for reference group
  y.F <- load.F*eta.F+ int.F #model-implied scores for focal group

  datR <- data.frame(eta = eta.R, y = y.R, group = "R")
  datF <- data.frame(eta = eta.F, y = y.F, group = "F")
  dat  <- bind_rows(datR,datF)

  dat %>% ggplot(aes(x = eta, y = y, color = group)) +
    geom_line(linewidth = 1) +
    labs(x = "Eta",
         y = "Estimated Response") +
    geom_line(data = datF,
              aes(x = eta, y = dnorm(eta, mean(eta), sd(eta)), color = NULL),
              linetype = "dashed") +
    xlim(-3,3) +
    ylim(0,1.2)



  sd.pooled.test.num <- ((length(y.R)-1)*sd(y.R)) + ((length(y.F)-1)*sd(y.F))
  sd.pooled.test.denom <- (length(y.R) - 1) + (length(y.F) - 1)
  sd.pooled.test <- sd.pooled.test.num/sd.pooled.test.denom

  #dMACS
  dMACS <- item_dmacs(load.R, #loading, R
                      load.F, #loading, F
                      int.R, #intercept, R
                      int.F, #intercept, F
                      mean(eta.F), #mean (both groups)
                      var(eta.F), #variance (both groups, but it would be the focal group if they didn't have the same variance)
                      sd.pooled.test #pooled standard deviation (again, this is basically arbitrary)
  )

  dMACS
  
}

  
}


# dMACS Conditions 2 ------------------------------------------------------
{
## dMACS = .20 ------------------------------------------------------------
{
### Mean Equal -----------------------------
  {
  set.seed(7029)
  
  N.R <- 1000
  N.F <- 1000
  
  mean.R <- 0
  mean.F <- 0
  
  sd.R <- 1
  sd.F <- 1
  
  #Set up latent variable
  eta.R <- rnorm(N.R, mean.R, sd.R)
  eta.F <- rnorm(N.F, mean.F, sd.F)
  
  #eta <- seq(-1000,1000, by = 10) #Generate true scores
  load.R <- .6 #loading for the reference group
  load.F <- .66 #loading for focal group
  int.R <- 0 #intercept for reference group
  int.F <- 0.11 #intercept for focal group
  
  y.R <- load.R*eta.R + int.R #model-implied scores for reference group
  y.F <- load.F*eta.F+ int.F #model-implied scores for focal group
  
  datR <- data.frame(eta = eta.R, y = y.R, group = "R")
  datF <- data.frame(eta = eta.F, y = y.F, group = "F")
  dat  <- bind_rows(datR,datF) 
  
  dat %>% ggplot(aes(x = eta, y = y, color = group)) +
    geom_line(linewidth = 1) + 
    labs(x = "Eta",
         y = "Estimated Response") + 
    geom_line(data = datF, 
              aes(x = eta, y = dnorm(eta, mean(eta), sd(eta)), color = NULL),
              linetype = "dashed") + 
    xlim(-3,3) +
    ylim(0,1.2)
  
  
  
  sd.pooled.test.num <- ((length(y.R)-1)*sd(y.R)) + ((length(y.F)-1)*sd(y.F)) 
  sd.pooled.test.denom <- (length(y.R) - 1) + (length(y.F) - 1)
  sd.pooled.test <- sd.pooled.test.num/sd.pooled.test.denom
  
  #dMACS
  dMACS <- item_dmacs(load.R, #loading, R 
                      load.F, #loading, F
                      int.R, #intercept, R
                      int.F, #intercept, F
                      mean(eta.F), #mean (both groups)
                      var(eta.F), #variance (both groups, but it would be the focal group if they didn't have the same variance)
                      sd.pooled.test #pooled standard deviation (again, this is basically arbitrary)
  )
  dMACS
  }
  
  ## Mean Unequal -----------------------------
  {
    set.seed(7029)
    
    N.R <- 1000
    N.F <- 1000
    
    mean.R <- 0
    mean.F <- 1
    
    sd.R <- 1
    sd.F <- 1
    
    #Set up latent variable
    eta.R <- rnorm(N.R, mean.R, sd.R)
    eta.F <- rnorm(N.F, mean.F, sd.F)
    
    #eta <- seq(-1000,1000, by = 10) #Generate true scores
    load.R <- .6 #loading for the reference group
    load.F <- .612 #loading for focal group
    int.R <- 0 #intercept for reference group
    int.F <- 0.11 #intercept for focal group
    
    y.R <- load.R*eta.R + int.R #model-implied scores for reference group
    y.F <- load.F*eta.F+ int.F #model-implied scores for focal group
    
    datR <- data.frame(eta = eta.R, y = y.R, group = "R")
    datF <- data.frame(eta = eta.F, y = y.F, group = "F")
    dat  <- bind_rows(datR,datF) 
    
    dat %>% ggplot(aes(x = eta, y = y, color = group)) +
      geom_line(linewidth = 1) + 
      labs(x = "Eta",
           y = "Estimated Response") + 
      geom_line(data = datF, 
                aes(x = eta, y = dnorm(eta, mean(eta), sd(eta)), color = NULL),
                linetype = "dashed") + 
      xlim(-3,3) +
      ylim(0,1.2)
    
    
    
    sd.pooled.test.num <- ((length(y.R)-1)*sd(y.R)) + ((length(y.F)-1)*sd(y.F)) 
    sd.pooled.test.denom <- (length(y.R) - 1) + (length(y.F) - 1)
    sd.pooled.test <- sd.pooled.test.num/sd.pooled.test.denom
    
    #dMACS
    dMACS <- item_dmacs(load.R, #loading, R 
                        load.F, #loading, F
                        int.R, #intercept, R
                        int.F, #intercept, F
                        mean(eta.F), #mean (both groups)
                        var(eta.F), #variance (both groups, but it would be the focal group if they didn't have the same variance)
                        sd.pooled.test #pooled standard deviation (again, this is basically arbitrary)
    )
    dMACS
  }
}
# dMACS = .30 ------------------------------------------------------------
{
  ### Mean Equal -----------------------------
  {
    set.seed(7029)
    
    N.R <- 1000
    N.F <- 1000
    
    mean.R <- 0
    mean.F <- 0
    
    sd.R <- 1
    sd.F <- 1
    
    #Set up latent variable
    eta.R <- rnorm(N.R, mean.R, sd.R)
    eta.F <- rnorm(N.F, mean.F, sd.F)
    
    #eta <- seq(-1000,1000, by = 10) #Generate true scores
    load.R <- .6 #loading for the reference group
    load.F <- .69 #loading for focal group
    int.R <- 0 #intercept for reference group
    int.F <- 0.17 #intercept for focal group
    
    y.R <- load.R*eta.R + int.R #model-implied scores for reference group
    y.F <- load.F*eta.F+ int.F #model-implied scores for focal group
    
    datR <- data.frame(eta = eta.R, y = y.R, group = "R")
    datF <- data.frame(eta = eta.F, y = y.F, group = "F")
    dat  <- bind_rows(datR,datF) 
    
    dat %>% ggplot(aes(x = eta, y = y, color = group)) +
      geom_line(linewidth = 1) + 
      labs(x = "Eta",
           y = "Estimated Response") + 
      geom_line(data = datF, 
                aes(x = eta, y = dnorm(eta, mean(eta), sd(eta)), color = NULL),
                linetype = "dashed") + 
      xlim(-3,3) +
      ylim(0,1.2)
    
    
    
    sd.pooled.test.num <- ((length(y.R)-1)*sd(y.R)) + ((length(y.F)-1)*sd(y.F)) 
    sd.pooled.test.denom <- (length(y.R) - 1) + (length(y.F) - 1)
    sd.pooled.test <- sd.pooled.test.num/sd.pooled.test.denom
    
    #dMACS
    dMACS <- item_dmacs(load.R, #loading, R 
                        load.F, #loading, F
                        int.R, #intercept, R
                        int.F, #intercept, F
                        mean(eta.F), #mean (both groups)
                        var(eta.F), #variance (both groups, but it would be the focal group if they didn't have the same variance)
                        sd.pooled.test #pooled standard deviation (again, this is basically arbitrary)
    )
    dMACS
  }
  
  ## Mean Unequal -----------------------------
  {
    set.seed(7029)
    
    N.R <- 1000
    N.F <- 1000
    
    mean.R <- 0
    mean.F <- 1
    
    sd.R <- 1
    sd.F <- 1
    
    #Set up latent variable
    eta.R <- rnorm(N.R, mean.R, sd.R)
    eta.F <- rnorm(N.F, mean.F, sd.F)
    
    #eta <- seq(-1000,1000, by = 10) #Generate true scores
    load.R <- .6 #loading for the reference group
    load.F <- .614#loading for focal group
    int.R <- 0 #intercept for reference group
    int.F <- 0.17 #intercept for focal group
    
    y.R <- load.R*eta.R + int.R #model-implied scores for reference group
    y.F <- load.F*eta.F+ int.F #model-implied scores for focal group
    
    datR <- data.frame(eta = eta.R, y = y.R, group = "R")
    datF <- data.frame(eta = eta.F, y = y.F, group = "F")
    dat  <- bind_rows(datR,datF) 
    
    dat %>% ggplot(aes(x = eta, y = y, color = group)) +
      geom_line(linewidth = 1) + 
      labs(x = "Eta",
           y = "Estimated Response") + 
      geom_line(data = datF, 
                aes(x = eta, y = dnorm(eta, mean(eta), sd(eta)), color = NULL),
                linetype = "dashed") + 
      xlim(-3,3) +
      ylim(0,1.2)
    
    
    
    sd.pooled.test.num <- ((length(y.R)-1)*sd(y.R)) + ((length(y.F)-1)*sd(y.F)) 
    sd.pooled.test.denom <- (length(y.R) - 1) + (length(y.F) - 1)
    sd.pooled.test <- sd.pooled.test.num/sd.pooled.test.denom
    
    #dMACS
    dMACS <- item_dmacs(load.R, #loading, R 
                        load.F, #loading, F
                        int.R, #intercept, R
                        int.F, #intercept, F
                        mean(eta.F), #mean (both groups)
                        var(eta.F), #variance (both groups, but it would be the focal group if they didn't have the same variance)
                        sd.pooled.test #pooled standard deviation (again, this is basically arbitrary)
    )
    dMACS
  }
}
# dMACS = .40 ------------------------------------------------------------
  {
    ## Mean Equal -----------------------------
    {
      set.seed(7029)
      
      N.R <- 1000
      N.F <- 1000
      
      mean.R <- 0
      mean.F <- 0
      
      sd.R <- 1
      sd.F <- 1
      
      #Set up latent variable
      eta.R <- rnorm(N.R, mean.R, sd.R)
      eta.F <- rnorm(N.F, mean.F, sd.F)
      
      #eta <- seq(-1000,1000, by = 10) #Generate true scores
      load.R <- .6 #loading for the reference group
      load.F <- .585 #loading for focal group
      int.R <- 0 #intercept for reference group
      int.F <- 0.24 #intercept for focal group
      
      y.R <- load.R*eta.R + int.R #model-implied scores for reference group
      y.F <- load.F*eta.F+ int.F #model-implied scores for focal group
      
      datR <- data.frame(eta = eta.R, y = y.R, group = "R")
      datF <- data.frame(eta = eta.F, y = y.F, group = "F")
      dat  <- bind_rows(datR,datF) 
      
      dat %>% ggplot(aes(x = eta, y = y, color = group)) +
        geom_line(linewidth = 1) + 
        labs(x = "Eta",
             y = "Estimated Response") + 
        geom_line(data = datF, 
                  aes(x = eta, y = dnorm(eta, mean(eta), sd(eta)), color = NULL),
                  linetype = "dashed") + 
        xlim(-3,3) +
        ylim(0,1.2)
      
      
      
      sd.pooled.test.num <- ((length(y.R)-1)*sd(y.R)) + ((length(y.F)-1)*sd(y.F)) 
      sd.pooled.test.denom <- (length(y.R) - 1) + (length(y.F) - 1)
      sd.pooled.test <- sd.pooled.test.num/sd.pooled.test.denom
      
      #dMACS
      dMACS <- item_dmacs(load.R, #loading, R 
                          load.F, #loading, F
                          int.R, #intercept, R
                          int.F, #intercept, F
                          mean(eta.F), #mean (both groups)
                          var(eta.F), #variance (both groups, but it would be the focal group if they didn't have the same variance)
                          sd.pooled.test #pooled standard deviation (again, this is basically arbitrary)
      )
      dMACS
    }
    
    # Mean Unequal -----------------------------
    {
      set.seed(7029)
      
      N.R <- 1000
      N.F <- 1000
      
      mean.R <- 0
      mean.F <- 1
      
      sd.R <- 1
      sd.F <- 1
      
      #Set up latent variable
      eta.R <- rnorm(N.R, mean.R, sd.R)
      eta.F <- rnorm(N.F, mean.F, sd.F)
      
      #eta <- seq(-1000,1000, by = 10) #Generate true scores
      load.R <- .6 #loading for the reference group
      load.F <- .604#loading for focal group
      int.R <- 0 #intercept for reference group
      int.F <- 0.24 #intercept for focal group
      
      y.R <- load.R*eta.R + int.R #model-implied scores for reference group
      y.F <- load.F*eta.F+ int.F #model-implied scores for focal group
      
      datR <- data.frame(eta = eta.R, y = y.R, group = "R")
      datF <- data.frame(eta = eta.F, y = y.F, group = "F")
      dat  <- bind_rows(datR,datF) 
      
      dat %>% ggplot(aes(x = eta, y = y, color = group)) +
        geom_line(linewidth = 1) + 
        labs(x = "Eta",
             y = "Estimated Response") + 
        geom_line(data = datF, 
                  aes(x = eta, y = dnorm(eta, mean(eta), sd(eta)), color = NULL),
                  linetype = "dashed") + 
        xlim(-3,3) +
        ylim(0,1.2)
      
      
      
      sd.pooled.test.num <- ((length(y.R)-1)*sd(y.R)) + ((length(y.F)-1)*sd(y.F)) 
      sd.pooled.test.denom <- (length(y.R) - 1) + (length(y.F) - 1)
      sd.pooled.test <- sd.pooled.test.num/sd.pooled.test.denom
      
      #dMACS
      dMACS <- item_dmacs(load.R, #loading, R 
                          load.F, #loading, F
                          int.R, #intercept, R
                          int.F, #intercept, F
                          mean(eta.F), #mean (both groups)
                          var(eta.F), #variance (both groups, but it would be the focal group if they didn't have the same variance)
                          sd.pooled.test #pooled standard deviation (again, this is basically arbitrary)
      )
      dMACS
    }
  }
}