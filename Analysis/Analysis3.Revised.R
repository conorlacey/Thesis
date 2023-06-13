library(tidyverse)
rm(list = ls())

# Read in Data ------------------------------------------------------------
dMACS <- read_rds("data/dat_dMACS.Rev3.RDS")
dMACS_low <- read_rds("data/dat_dMACS_low.Rev3.RDS")
dMACS_high <- read_rds("data/dat_dMACS_high.Rev3.RDS")

dMACS_post <- read_rds("data/dat_dMACS_post.Rev3.RDS")
dMACS_low_CRI <- read_rds("data/dat_dMACS_low_CRI.Rev3.RDS")
dMACS_high_CRI <- read_rds("data/dat_dMACS_high_CRI.Rev3.RDS")

dMACS_S <- read_rds("data/dat_dMACS_S.Rev3.RDS")
dMACS_s_low <- read_rds("data/dat_dMACS_S_low.Rev3.RDS")
dMACS_s_high <- read_rds("data/dat_dMACS_S_high.Rev3.RDS")

# Means of Columns --------------------------------------------------------
dMACS_a <- dMACS %>% colMeans() %>% data.frame()
dMACS_low_a <- dMACS_low %>% colMeans() %>% data.frame()
dMACS_high_a <- dMACS_high %>% colMeans() %>% data.frame()
dMACS_a <- bind_cols(dMACS_a, dMACS_low_a, dMACS_high_a) %>% 
  setNames(c("dMACS", "Lower", "Upper"))
head(dMACS_a)

dMACS_post_a <- dMACS_post %>% colMeans() %>% data.frame()
dMACS_low_CRI_a <- dMACS_low_CRI %>% colMeans() %>% data.frame()
dMACS_high_CRI_a <- dMACS_high_CRI %>% colMeans() %>% data.frame()
dMACS_post_a <- bind_cols(dMACS_post_a, dMACS_low_CRI_a, dMACS_high_CRI_a) %>% 
  setNames(c("dMACS_post", "Lower", "Upper"))
head(dMACS_post_a)

dMACS_S_a <- dMACS_S %>% colMeans() %>% data.frame()
dMACS_s_low_a <- dMACS_s_low %>% colMeans() %>% data.frame()
dMACS_s_high_a <- dMACS_s_high %>% colMeans() %>% data.frame()
dMACS_S_a <- bind_cols(dMACS_S_a, dMACS_s_low_a, dMACS_s_high_a) %>% 
  setNames(c("dMACS_S", "Lower", "Upper"))
head(dMACS_S_a)

cor(dMACS_a$Lower, dMACS_post_a$Lower)

cor(dMACS_a$Upper, dMACS_post_a$Upper)

# Proportion with No Effect -----------------------------------------------
props <- matrix(nrow = 54, ncol = 1) %>% data.frame() %>% 
  setNames(c("Percent"))
for (i in 1:54){
  props[i,1] <- sum((dMACS_low[i] <= 0 & dMACS_high[i] >= 0)/500)*100
}

props_post <- matrix(nrow = 54, ncol = 1) %>% data.frame() %>% 
  setNames(c("Percent"))
for (i in 1:54){
  props_post[i,1] <- sum((dMACS_low_CRI[i] <= 0 & dMACS_high_CRI[i] >= 0)/500)*100
}

props_shrunk <- matrix(nrow = 54, ncol = 1) %>% data.frame() %>% 
  setNames(c("Percent"))
for (i in 1:54){
  props_shrunk[i,1] <- sum((dMACS_s_low[i] <= 0 & dMACS_s_high[i] >= 0)/500)*100
}


