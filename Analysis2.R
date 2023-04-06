library(tidyverse)

# Read in Data ------------------------------------------------------------
dMACS <- read_rds("data/dat_dMACS.2.RDS")
dMACS_low <- read_rds("data/dat_dMACS_low.2.RDS")
dMACS_high <- read_rds("data/dat_dMACS_high.2.RDS")

dMACS_post <- read_rds("data/dat_dMACS_post.2.RDS")
dMACS_low_CRI <- read_rds("data/dat_dMACS_low_CRI.2.RDS")
dMACS_high_CRI <- read_rds("data/dat_dMACS_high_CRI.2.RDS")

dMACS_S <- read_rds("data/dat_dMACS_S.2.RDS")
dMACS_s_low <- read_rds("data/dat_dMACS_S_low.2.RDS")
dMACS_s_high <- read_rds("data/dat_dMACS_S_high.2.RDS")

# Means of Columns --------------------------------------------------------
dMACS <- dMACS %>% colMeans() %>% data.frame()
dMACS_low <- dMACS_low %>% colMeans() %>% data.frame()
dMACS_high <- dMACS_high %>% colMeans() %>% data.frame()
dMACS <- bind_cols(dMACS, dMACS_low, dMACS_high) %>% 
  setNames(c("dMACS", "Lower", "Upper"))
head(dMACS)

dMACS_post <- dMACS_post %>% colMeans() %>% data.frame()
dMACS_low_CRI <- dMACS_low_CRI %>% colMeans() %>% data.frame()
dMACS_high_CRI <- dMACS_high_CRI %>% colMeans() %>% data.frame()
dMACS_post <- bind_cols(dMACS_post, dMACS_low_CRI, dMACS_high_CRI) %>% 
  setNames(c("dMACS_post", "Lower", "Upper"))
head(dMACS_post)

dMACS_S <- dMACS_S %>% colMeans() %>% data.frame()
dMACS_s_low <- dMACS_s_low %>% colMeans() %>% data.frame()
dMACS_s_high <- dMACS_s_high %>% colMeans() %>% data.frame()
dMACS_S <- bind_cols(dMACS_S, dMACS_s_low, dMACS_s_high) %>% 
  setNames(c("dMACS_S", "Lower", "Upper"))
head(dMACS_S)

# Proportion with No Effect -----------------------------------------------
props <- matrix(nrow = 54, ncol = 1) %>% data.frame() %>% 
  setNames(c("Proportion"))
for (i in 1:54){
  props[i,1] <- sum((dMACS_low[i] <= 0 & dMACS_high[i] >= 0)/500)*100
}

props

props_post <- matrix(nrow = 54, ncol = 1) %>% data.frame() %>% 
  setNames(c("Proportion"))
for (i in 1:54){
  props_post[i,1] <- sum((dMACS_low_CRI[i] <= 0 & dMACS_high_CRI[i] >= 0)/500)*100
}

props_post

props_shrunk <- matrix(nrow = 54, ncol = 1) %>% data.frame() %>% 
  setNames(c("Proportion"))
for (i in 1:54){
  props_shrunk[i,1] <- sum((dMACS_s_low[i] <= 0 & dMACS_s_high[i] >= 0)/500)*100
}

props_shrunk
