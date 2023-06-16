library(tidyverse)
library(effectsize)

dMACS<- readRDS(file = "data/dat_dMACS.Rev3.RDS")

dMACS_obtained <- unlist(dMACS, use.names = FALSE)

dMACS_shrunk <- readRDS(file = "data/dat_dMACS_S.Rev3.RDS")

dMACS_shrunk_obtained <- unlist(dMACS_shrunk, use.names = FALSE)

#Create dMACS_compare
dMACS_compare <- rep(c(rep(.1, 1500), rep(.5, 1500), rep(.9, 1500)),6)

# OK, now we will simulate the value of shrinkage for each of the different types of dMACS
shrinkage_dMACS <- (dMACS_compare - dMACS_obtained) / dMACS_compare
shrinkage_dMACS_shrunk <- (dMACS_compare - dMACS_shrunk_obtained) / dMACS_compare
shrinkage <- (dMACS_obtained - dMACS_shrunk_obtained) / dMACS_obtained

# Now we will simulate the conditions
# Simulate replication ID.
RepID <- rep(1:500, 54)
# Simulate effect size
ES <- rep(c(rep(1, 1500), rep(2, 1500), rep(3, 1500)),6)
ES <- factor(ES, labels = c("SM", "M", "L"))
# Simulate sample size
SS <- rep(c(rep(1, 500), rep(2, 500), rep(3, 500)), 18)
SS <- factor(SS, labels = c("SM", "M", "L"))
# Simulate prior probability
PP <- rep(c(rep(1, 4500), rep(2, 4500), rep(3, 4500)), 2)
PP <- factor(PP, labels = c(".25", ".5", ".75"))
# Simulate latent variable means
LV <- c(rep(1, 13500), rep(2, 13500))
LV <- factor(LV, labels = c("Equal", "Unequal"))

# Now let's put it all together
the.data <- data.frame(RepID, dMACS_compare, dMACS_obtained, dMACS_shrunk_obtained,
                       shrinkage_dMACS, shrinkage_dMACS_shrunk, shrinkage,
                       ES, SS, PP, LV)

##################################################################################
######################## Running the meta-models #################################
##################################################################################

# OK, so here's what a meta-model would look like
# Here's the model for the degree of shrinkage for dMACS_shrunk
the.model <- lm(shrinkage_dMACS ~ ES*SS*PP*LV, data = the.data)
anova(the.model) 
eta_squared(the.model, partial = TRUE)

the.model.c <- lm(shrinkage ~ ES*SS*PP*LV, data = the.data)
anova(the.model.c) 
eta_squared(the.model.c, partial = TRUE)

the.data.f <- the.data %>% filter(ES == "SM")

the.model.f <- lm(shrinkage_dMACS_shrunk ~ SS*PP*LV, data = the.data.f)
anova(the.model.f) 
eta_squared(the.model.f, partial = TRUE)

the.model.c.f <- lm(shrinkage ~ SS*PP*LV, data = the.data.f)
anova(the.model.c.f) 
eta_squared(the.model.c.f, partial = TRUE)


