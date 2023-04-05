library(tidyverse)


# Read in Data ------------------------------------------------------------
dMACS <- read_rds("data/dat_dMACS.RDS")
dMACS_low <- read_rds("data/dat_dMACS_low.RDS")
dMACS_high <- read_rds("data/dat_dMACS_high.RDS")

dMACS_post <- read_rds("data/dat_dMACS_post.RDS")
dMACS_low_CRI <- read_rds("data/dat_dMACS_low_CRI.RDS")
dMACS_high_CRI <- read_rds("data/dat_dMACS_high_CRI.RDS")

dMACS_S <- read_rds("data/dat_dMACS_S.RDS")
dMACS_s_low <- read_rds("data/dat_dMACS_S_low.RDS")
dMACS_s_high <- read_rds("data/dat_dMACS_S_high.RDS")


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


