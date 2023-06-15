library(tidyverse)
library(effectsize)

#Read and reorder dMACS data
dMACS<- readRDS(file = "data/dat_dMACS.Rev3.RDS")

conds1 <- c(1,28,10,37,19,46) # ES=SM | SS=SM | PP= 0.25,0.5,0.75 | LV=Equal & Unequal
conds2 <- c(2,29,11,38,20,47) # ES=SM | SS=M | PP= 0.25,0.5,0.75 | LV=Equal & Unequal
conds3 <- c(3,30,12,39,21,48) # ES=SM | SS=L | PP= 0.25,0.5,0.75 | LV=Equal & Unequal

conds4 <- c(4,31,13,40,22,49) # ES=M | SS=SM | PP= 0.25,0.5,0.75 | LV=Equal & Unequal
conds5 <- c(5,32,14,41,23,50) # ES=M | SS=M | PP= 0.25,0.5,0.75 | LV=Equal & Unequal
conds6 <- c(6,33,15,42,24,51) # ES=M | SS=L | PP= 0.25,0.5,0.75 | LV=Equal & Unequal

conds7 <- c(7,34,16,43,25,52) # ES=L | SS=SM | PP= 0.25,0.5,0.75 | LV=Equal & Unequal
conds8 <- c(8,35,17,44,26,53) # ES=L | SS=M | PP= 0.25,0.5,0.75 | LV=Equal & Unequal
conds9 <- c(9,36,18,45,27,54) # ES=L | SS=L | PP= 0.25,0.5,0.75 | LV=Equal & Unequal

conds <- c(conds1,conds2,conds3,
           conds4,conds5,conds6,
           conds7,conds8,conds9)

dMACS_obtained <- dMACS[,conds[1]]
for (i in 2:54){
  x <- dMACS[,conds[i]]
  dMACS_obtained <- c(dMACS_obtained, x)
}

#Read and reorder dMACS_shrunk data
dMACS_shrunk <- readRDS(file = "data/dat_dMACS_S.Rev3.RDS")

conds1 <- c(1,28,10,37,19,46) # ES=SM | SS=SM | PP= 0.25,0.5,0.75 | LV=Equal & Unequal
conds2 <- c(2,29,11,38,20,47) # ES=SM | SS=M | PP= 0.25,0.5,0.75 | LV=Equal & Unequal
conds3 <- c(3,30,12,39,21,48) # ES=SM | SS=L | PP= 0.25,0.5,0.75 | LV=Equal & Unequal

conds4 <- c(4,31,13,40,22,49) # ES=M | SS=SM | PP= 0.25,0.5,0.75 | LV=Equal & Unequal
conds5 <- c(5,32,14,41,23,50) # ES=M | SS=M | PP= 0.25,0.5,0.75 | LV=Equal & Unequal
conds6 <- c(6,33,15,42,24,51) # ES=M | SS=L | PP= 0.25,0.5,0.75 | LV=Equal & Unequal

conds7 <- c(7,34,16,43,25,52) # ES=L | SS=SM | PP= 0.25,0.5,0.75 | LV=Equal & Unequal
conds8 <- c(8,35,17,44,26,53) # ES=L | SS=M | PP= 0.25,0.5,0.75 | LV=Equal & Unequal
conds9 <- c(9,36,18,45,27,54) # ES=L | SS=L | PP= 0.25,0.5,0.75 | LV=Equal & Unequal

conds <- c(conds1,conds2,conds3,
           conds4,conds5,conds6,
           conds7,conds8,conds9)

dMACS_shrunk_obtained <- dMACS_shrunk[,conds[1]]
for (i in 2:54){ # <- CHECK THIS OUT
  x <- dMACS_shrunk[,conds[i]]
  dMACS_shrunk_obtained <- c(dMACS_shrunk_obtained,x)
}

#Create dMACS_compare
dMACS_compare <- c(rep(.1, 9000), rep(.5, 9000), rep(.9, 9000))

# OK, now we will simulate the value of shrinkage for each of the different types of dMACS
shrinkage_dMACS <- (dMACS_compare - dMACS_obtained) / dMACS_compare
shrinkage_dMACS_shrunk <- (dMACS_compare - dMACS_shrunk_obtained) / dMACS_compare

# Now we will simulate the conditions
# Simulate replication ID.
RepID <- rep(1:500, 54)
# Simulate effect size
ES <- c(rep(1, 9000), rep(2, 9000), rep(3, 9000))
ES <- factor(ES, labels = c("SM", "M", "L"))
# Simulate sample size
SS <- rep(c(rep(1, 3000), rep(2, 3000), rep(3, 3000)), 3)
SS <- factor(SS, labels = c("SM", "M", "L"))
# Simulate prior probability
PP <- rep(c(rep(1, 1000), rep(2, 1000), rep(3, 1000)), 9)
PP <- factor(PP, labels = c(".25", ".5", ".75"))
# Simulate latent variable means
LV <- rep(c(rep(1, 500), rep(2, 500)), 27)
LV <- factor(LV, labels = c("Equal", "Unequal"))

# Now let's put it all together
the.data.2 <- data.frame(RepID, dMACS_compare, dMACS_obtained, dMACS_shrunk_obtained,
                       shrinkage_dMACS, shrinkage_dMACS_shrunk,
                       ES, SS, PP, LV)

##################################################################################
######################## Running the meta-models #################################
##################################################################################

# OK, so here's what a meta-model would look like
# Here's the model for the degree of shrinkage for dMACS_shrunk
the.model <- lm(shrinkage_dMACS_shrunk ~ ES*SS*PP*LV, data = the.data.2)
anova(the.model)
eta_squared(the.model, partial = TRUE)

the.data %>% group_by(ES, SS, PP, LV) %>% 
  summarize(mean = mean(dMACS_obtained) %>% 
  round(digits = 2)) %>% 
  View()

the.data.2 %>% group_by(ES, SS, PP, LV) %>% 
  summarize(mean = mean(dMACS_obtained) %>% 
  round(digits = 2)) %>% 
  View()
