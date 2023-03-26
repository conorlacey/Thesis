library(tidyverse)
library(dmacs)
source("functions2.R")
set.seed(9420)

names <- c(1:54) %>% as.character()

dat_dMACS <- matrix(nrow = 500, ncol = 54) %>% data.frame() %>% 
  setNames(c(names))
dat_dMACS_S <- matrix(nrow = 500, ncol = 54) %>% data.frame() %>% 
  setNames(c(names))

df <- matrix(nrow = 54, ncol = 8) %>% data.frame() %>% 
  setNames(c("N.R","N.F", "load.R", "load.F", "int.R", "int.F","priorPH0",
             "mean.F"))

for (i in 1:500){
  for (cond in 1:54){
    if (cond %in% c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,52)){
      N.R <- 250
      N.F <- 250
    }
    if (cond %in% c(2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53)){
      N.R <- 500
      N.F <- 500
    }
    if (cond %in% c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54)){
      N.R <- 1000
      N.F <- 1000
    }
    if (cond %in% c(1,2,3,10,11,12,19,20,21)){ # dMACS 0.1 mean equal
      load.R <- .6 #loading for the reference group
      load.F <- .634 #loading for focal group
      int.R <- 0 #intercept for reference group
      int.F <- 0.05 #intercept for focal group
    }
    if (cond %in% c(28,29,30,37,38,39,46,47,48)){ #dMACS 0.1 mean unequal
      load.R <- .6 #loading for the reference group
      load.F <- .61 #loading for focal group
      int.R <- 0 #intercept for reference group
      int.F <- 0.05 #intercept for focal group
    }
    if (cond %in% c(4,5,6,13,14,15,22,23,24)){ #dMACS 0.5 mean equal
      load.R <- .6 #loading for the reference group
      load.F <- .61 #loading for focal group
      int.R <- 0 #intercept for reference group
      int.F <- 0.3 #intercept for focal group
    }
    if (cond %in% c(31,32,33,40,41,42,49,50,51)){ #dMACS 0.5 mean unequal
      load.R <- .6 #loading for the reference group
      load.F <- .61 #loading for focal group
      int.R <- 0 #intercept for reference group
      int.F <- 0.29 #intercept for focal group
    }
    if (cond %in% c(7,8,9,16,17,18,25,26,27)){ #dMACS 0.9 mean equal
      load.R <- .6 #loading for the reference group
      load.F <- .606#loading for focal group
      int.R <- 0 #intercept for reference group
      int.F <- 0.55 #intercept for focal group
    }
    if (cond %in% c(34,35,36,43,44,45,52,53,54)){ #dMACS 0.9 mean unequal
      load.R <- .6 #loading for the reference group
      load.F <- .595 #loading for focal group
      int.R <- 0 #intercept for reference group
      int.F <- 0.55 #intercept for focal group
    }
    if (cond %in% c(1:9,28:36)){
      priorPH0  <- 0.25
    }
    if (cond %in% c(10:18,37:45)){
      priorPH0 <- 0.5
    }
    if (cond %in% c(19:27,46:54)){
      priorPH0 <- 0.75
    }
    if (cond %in% c(1:27)){ #mean equal
      mean.F <- 0
    }
    if (cond %in% c(28:54)){ #mean unequal
      mean.F <- 1
    }
    
    #Index
    df$N.R[cond] <- N.R
    df$N.F[cond] <- N.F
    df$load.R[cond] <- load.R
    df$load.F[cond] <- load.F
    df$int.R[cond] <- int.R
    df$int.F[cond] <- int.F
    df$priorPH0[cond] <- priorPH0
    df$mean.F[cond] <- mean.F
    
    mean.R <- 0
    
    
    sd.R <- 1
    sd.F <- 1
    
    #Set up latent variable
    eta.R <- rnorm(N.R, mean.R, sd.R)
    eta.F <- rnorm(N.F, mean.F, sd.F)
    
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
    
    dat_dMACS[i,cond] <- dMACS
    
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
    
    dat_dMACS_S[i,cond]<- dMACS_Shrunk
  }
}

write.csv(dat_dMACS, file = "dat_dMACS.csv")
write.csv(dat_dMACS_S, file = "dat_dMACS_S.csv")

