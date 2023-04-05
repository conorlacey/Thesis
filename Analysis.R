library(tidyverse)


dMACS <- read_rds("data/dat_dMACS.RDS")
dMACS_low <- read_rds("data/dat_dMACS_low.RDS")
dMACS_high <- read_rds("data/dat_dMACS_high.RDS")

dMACS_post <- read_rds("data/dat_dMACS_post.RDS")
dMACS_low_CRI <- read_rds("data/dat_dMACS_low_CRI.RDS")
dMACS_high_CRI <- read_rds("data/dat_dMACS_high_CRI.RDS")

dMACS_S <- read_rds("data/dat_dMACS_S.RDS")
dMACS_s_low <- read_rds("data/dat_dMACS_S_low.RDS")
dMACS_s_high <- read_rds("data/dat_dMACS_S_high.RDS")

dMACS_avg <- dMACS %>% colMeans() %>% as.data.frame() %>% setNames("Mean")
dMACS_S_avg <- dMACS_S %>% colMeans() %>% as.data.frame() %>% setNames("Mean")



