#Functions

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
