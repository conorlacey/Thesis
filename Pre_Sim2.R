rm(list=ls())

packages <- c("tidyverse", "faux", "lsr")
lapply(packages, library, character.only=TRUE)

measures.dat <- matrix(nrow = 1000, ncol = 2) %>% data.frame() %>% 
  setNames(c("dMACS", "CohensD"))

n <- seq(1,10, by = 0.1)

# Intercepts --------------------------------------------------------------
set.seed(3602)
int.dat <- matrix(nrow = length(n), ncol = 2) %>% data.frame() %>% 
  setNames(c("intercept", "correlation"))

for (j in 1:length(n)){
for (i in 1:nrow(measures.dat)){
x1=rnorm(100,0,1)
x2=rnorm(100,0,1)
error=rnorm(100,0,5)

y1<- n[j]+(1*x1)+error
y2<- 1+(1*x2)+error

fake.dat.1 <- data.frame(x1 = x1, x2 = x2,
                         y1 = y1, y2 = y2)

m1 <- lm(y1~x1)
m2 <- lm(y2~x2)


integrand <- function(x){
  product <-(m1$coefficients[1] + m1$coefficients[2]*x) - (m2$coefficients[1] + m2$coefficients[2]*x)
  return(product*dnorm(x,0,1))
}

a <- (nrow(fake.dat.1)-1)*sd(fake.dat.1$y1)+(nrow(fake.dat.1)-1)*sd(fake.dat.1$y2)
b <- (nrow(fake.dat.1)-1) + (nrow(fake.dat.1)-1) 

den <- a/b

c<-integrate(integrand, -Inf, Inf)$value/den

d<-cohensD(y1,y2)


measures.dat$dMACS[i]<-c
measures.dat$CohensD[i]<-d
}

int.dat$intercept[j] <- n[j]
int.dat$correlation[j] <- cor(measures.dat$dMACS, measures.dat$CohensD)
int.dat$average.dMAC[j] <- mean(measures.dat$dMACS)
int.dat$average.cohensD[j] <- mean(measures.dat$CohensD)
}

# Intercept and Correlation 
int.dat %>% ggplot(aes(x = intercept, y = correlation)) + 
  geom_line()

# Intercept and Average Estimates
int.dat %>% ggplot(aes(x = intercept)) + 
            geom_line(aes(y = average.dMAC, color = "dMACS"),
                          linewidth = 1, alpha = 0.5) + 
            geom_line(aes(y = average.cohensD, color = "Cohen's D"),
                               linewidth = 1, alpha = 0.5) + 
            ylab("Average Estimate") + 
            scale_color_manual(name = "",
                               breaks = c("dMACS", "Cohen's D"),
                               values = c("dMACS" = "blue", "Cohen's D" = "red"))


cof.dat %>% ggplot(aes(x = regression)) + 
            geom_line(aes(y = average.dMAC, color = "dMACS"),
                          linewidth = 1, alpha = 0.5) + 
            geom_line(aes(y = average.cohensD, color = "Cohen's D"),
                          linewidth = 1, alpha = 0.5) + 
            ylab("Average Estimate") + 
            scale_color_manual(name = "",
                              breaks = c("dMACS", "Cohen's D"),
                              values = c("dMACS" = "blue", "Cohen's D" = "red"))

# Regression Coefficient --------------------------------------------------
set.seed(3602)

cof.dat <- matrix(nrow = length(n), ncol = 2) %>% data.frame() %>% 
  setNames(c("regression", "correlation"))

for (j in 1:length(n)){
  for (i in 1:nrow(measures.dat)){
    x1=rnorm(100,0,1)
    x2=rnorm(100,0,1)
    error=rnorm(100,0,5)
    
    y1<- 1+(n[j]*x1)+error
    y2<- 1+(1*x2)+error
    
    fake.dat.1 <- data.frame(x1 = x1, x2 = x2,
                             y1 = y1, y2 = y2)
    
    m1 <- lm(y1~x1)
    m2 <- lm(y2~x2)
    
    
    integrand <- function(x){
      product <-(m1$coefficients[1] + m1$coefficients[2]*x) - (m2$coefficients[1] + m2$coefficients[2]*x)
      return(product*dnorm(x,0,1))
    }
    
    a <- (nrow(fake.dat.1)-1)*sd(fake.dat.1$y1)+(nrow(fake.dat.1)-1)*sd(fake.dat.1$y2)
    b <- (nrow(fake.dat.1)-1) + (nrow(fake.dat.1)-1) 
    
    den <- a/b
    
    c<-integrate(integrand, -Inf, Inf)$value/den
    
    d<-cohensD(y1,y2)
    
    
    measures.dat$dMACS[i]<-c
    measures.dat$CohensD[i]<-d
  }

  cof.dat$regression[j] <- n[j]
  cof.dat$correlation[j] <- cor(measures.dat$dMACS, measures.dat$CohensD)
  cof.dat$average.dMAC[j] <- mean(measures.dat$dMACS)
  cof.dat$average.cohensD[j] <- mean(measures.dat$CohensD)  

}

# Regression Coefficient and Correlation 
cof.dat %>% ggplot(aes(x = regression, y = correlation)) + 
  xlab("Regression Coefficient") +
  geom_line()

# Regression Coefficient and Average Estimates
cof.dat %>% ggplot(aes(x = regression)) + 
  geom_line(aes(y = average.dMAC, color = "dMACS"),
            linewidth = 1, alpha = 0.5) + 
  geom_line(aes(y = average.cohensD, color = "Cohen's D"),
            linewidth = 1, alpha = 0.5) + 
  ylab("Average Estimate") + 
  xlab("Regression Coefficient") +
  scale_color_manual(name = "",
                     breaks = c("dMACS", "Cohen's D"),
                     values = c("dMACS" = "blue", "Cohen's D" = "red"))
