library(effectsize)

##################################################################################
##### Simulating data to have the format it needs to be in for meta-models #######
##################################################################################

# Assume we have 54 conditions. 
# Effect size has 3 levels: S, M, L. We code this as ES.
# Sample size has 3 levels: S, M, L. We code this as SS. 
# Prior probability has 3 levels: .25, .5, .75. We code this as PP.
# Latent variable mean has 2 levels: E (for equal) or U (for unequal). We code this as LV.

# We will simulate 500 replications for each of these conditions.

# First thing we do is simulate some dMACS scores -- 
# this would be the value of dMACS (not dMACS_shrunk) we obtained in each replication
dMACS_obtained <- c(rnorm(9000, .1, .1), rnorm(9000, .5, .1), rnorm(9000, .9, .1))
# Now let's simulate the value of dMACS_shrunk we obtained in each replication
dMACS_shrunk_obtained <- c(rnorm(9000, .05, .1), rnorm(9000, .5, .1), rnorm(9000, .9, .1))
# Now here is the value of dMACS we're comparing to for each replication
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
the.data <- data.frame(RepID, dMACS_compare, dMACS_obtained, dMACS_shrunk_obtained,
                       shrinkage_dMACS, shrinkage_dMACS_shrunk,
                       ES, SS, PP, LV)



##################################################################################
######################## Running the meta-models #################################
##################################################################################

# OK, so here's what a meta-model would look like
# Here's the model for the degree of shrinkage for dMACS_shrunk
the.model <- lm(shrinkage_dMACS_shrunk ~ ES*SS*PP*LV)
anova(the.model)
eta_squared(the.model, partial = TRUE)