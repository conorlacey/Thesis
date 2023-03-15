rm(list = ls())

packages <- c("tidyverse", "faux", "lsr")
lapply(packages, library, character.only=TRUE)

measures.dat <- matrix(nrow = 1000, ncol = 2) %>% data.frame() %>% 
  setNames(c("dMACS", "CohensD"))

# Means -------------------------------------------------------------------
set.seed(3602)

n <- seq(1,2, by = 0.01)
cor.dat <- matrix(nrow = length(n), ncol = 2) %>% data.frame() %>%
  setNames(c("mean", "correlation"))

for (j in 1:length(n)){
for (i in 1:nrow(measures.dat)){
fake.dat.1<-rnorm_multi(n = 100, 
                        mu = c(1,n[j]),
                        sd = c(1,1),
                        r = .1,
                        varnames = c("x","y"))

fake.dat.2<-rnorm_multi(n = 100, 
                        mu = c(1,1),
                        sd = c(1,1),
                        r = .1,
                        varnames = c("x","y"))

linear.1<-lm(fake.dat.1$y~fake.dat.1$x)
linear.2<-lm(fake.dat.2$y~fake.dat.2$x)

integrand <- function(x){
  product <-(linear.1$coefficients[1] + linear.1$coefficients[2]*x) - (linear.2$coefficients[1] + linear.2$coefficients[2]*x)
  return((product^2)*dnorm(x,1,1))
}

a <- (nrow(fake.dat.1)-1)*sd(fake.dat.1$y)+(nrow(fake.dat.2)-1)*sd(fake.dat.2$y)
b <- (nrow(fake.dat.1)-1) + (nrow(fake.dat.2)-1) 

den <- a/b

a<-sqrt(integrate(integrand, -Inf, Inf)$value)/den

b<-cohensD(fake.dat.1$y, fake.dat.2$y)

measures.dat$dMACS[i] <- a

measures.dat$CohensD[i]<- b
}
 cor.dat$average.dMAC[j] <- mean(measures.dat$dMACS)
 cor.dat$average.cohensD[j] <- mean(measures.dat$CohensD)  
 cor.dat$mean[j] <- n[j]
 cor.dat$correlation[j] <- cor(measures.dat$dMACS, measures.dat$CohensD)
}

# Mean and Correlation Plot 
cor.dat %>% ggplot(aes(x = mean, y = correlation)) + 
  geom_line()

# Mean and Average Estimates Plot
cor.dat %>% ggplot(aes(x = mean)) + 
  geom_line(aes(y = average.dMAC, color = "dMACS"),
            linewidth = 1, alpha = 0.5) + 
  geom_line(aes(y = average.cohensD, color = "Cohen's D"),
            linewidth = 1, alpha = 0.5) + 
  ylab("Average Estimate") + 
  scale_color_manual(name = "",
                     breaks = c("dMACS", "Cohen's D"),
                     values = c("dMACS" = "blue", "Cohen's D" = "red"))


# R value -----------------------------------------------------------------
set.seed(3602)
m <- seq(.1,.99, by = 0.01)

r.dat <- matrix(nrow = length(m), ncol = 2) %>% data.frame() %>%
  setNames(c("RValue", "correlation"))

for (j in 1:length(m)){
  for (i in 1:nrow(measures.dat)){
    fake.dat.1<-rnorm_multi(n = 100, 
                            mu = c(1,1),
                            sd = c(1,1),
                            r = m[j],
                            varnames = c("x","y"))
    
    fake.dat.2<-rnorm_multi(n = 100, 
                            mu = c(1,1),
                            sd = c(1,1),
                            r = .1,
                            varnames = c("x","y"))
    
    linear.1<-lm(fake.dat.1$y~fake.dat.1$x)
    linear.2<-lm(fake.dat.2$y~fake.dat.2$x)
    
    integrand <- function(x){
      product <-(linear.1$coefficients[1] + linear.1$coefficients[2]*x) - (linear.2$coefficients[1] + linear.2$coefficients[2]*x)
      return((product^2)*dnorm(x,1,1))
    }
    
    a <- (nrow(fake.dat.1)-1)*sd(fake.dat.1$y)+(nrow(fake.dat.2)-1)*sd(fake.dat.2$y)
    b <- (nrow(fake.dat.1)-1) + (nrow(fake.dat.2)-1) 
    
    den <- a/b
    
    a<-sqrt(integrate(integrand, -Inf, Inf)$value)/den
    
    b<-cohensD(fake.dat.1$y, fake.dat.2$y)
    
    measures.dat$dMACS[i] <- a
    
    measures.dat$CohensD[i]<- b
  }
  r.dat$average.dMAC[j] <- mean(measures.dat$dMACS)
  r.dat$average.cohensD[j] <- mean(measures.dat$CohensD)
  r.dat$RValue[j] <- m[j]
  r.dat$correlation[j] <- cor(measures.dat$dMACS, measures.dat$CohensD)
}


#R value and correlation plot
r.dat %>% ggplot(aes(x = RValue, y = correlation)) + 
  geom_line()


#R value and Average Estimates Plot
r.dat %>% ggplot(aes(x = RValue)) + 
  geom_line(aes(y = average.dMAC, color = "dMACS"),
            linewidth = 1, alpha = 0.5) + 
  geom_line(aes(y = average.cohensD, color = "Cohen's D"),
            linewidth = 1, alpha = 0.5) + 
  ylab("Average Estimate") + 
  scale_color_manual(name = "",
                     breaks = c("dMACS", "Cohen's D"),
                     values = c("dMACS" = "blue", "Cohen's D" = "red"))






