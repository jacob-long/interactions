

library(interactions)
library(sandwich)
library(lmtest)
library(jtools)


model <- lm(mpg ~ hp * wt, data = mtcars)
# Assuming 'cyl' is the clustering variable

clustered_se <- vcovCL(model, cluster = ~cyl)

#sim_slopes(model, pred = "hp", modx = "wt", vcov = clustered_se)


sim_slopes(model, pred = "hp", modx = "wt", jnplot = TRUE,vcov = clustered_se)

clustered_se <- vcovCL(model, cluster = ~cyl)
sim_slopes(model, pred = "hp", modx = "wt", jnplot = TRUE, vcov = clustered_se)

getAnywhere(sim_slopes)
conflicted::conflict_scout()



getwd()
setwd("/home/onyxia/interactions")
getwd()
