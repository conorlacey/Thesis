library(tidyverse)

dMACS_S <- read_csv("dat_dMACS_S.csv") %>% as.data.frame() %>% select(-...1)
dMACS <- read_csv("dat_dMACS.csv") %>% as.data.frame() %>% select(-...1)

dMACS_avg <- dMACS %>% colMeans() %>% as.data.frame() %>% setNames("Mean")
dMACS_S_avg <- dMACS_S %>% colMeans() %>% as.data.frame() %>% setNames("Mean")
