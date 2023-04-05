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

View(dMACS)
View(dMACS_high) # This needs to be fixed #Done
View(dMACS_low)

View(dMACS_post) # This needs to be fixed
View(dMACS_high_CRI)
View(dMACS_low_CRI)

View(dMACS_S)
View(dMACS_s_high)
View(dMACS_s_low)

