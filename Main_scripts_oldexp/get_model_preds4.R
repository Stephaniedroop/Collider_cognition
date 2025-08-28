#################################################### 
###### Collider - get model predictions  #####
####################################################
# Script to set up probability vectors of each variable, then run a series of 3 source files to implement the cesm
# and save the model predictions for each run

rm(list=ls())
setwd("../Main_scripts")
library(tidyverse)

# NOW WITHOUT SENSITIVITY PARAMETER AND WITH S SET AT 0.7. See v2 for those.

# Other values set outside for now 
N_cf <- 100000L # How many counterfactual samples to draw
modelruns <- 10
s <- 0.7

causes1 <- c('A', 'Au', 'B', 'Bu')

load('../model_data/params.rdata', verbose = T) # defined in script `set_params.r`


# Load functions: world_combos, get_cfs 
source('functionsN1.R')

set.seed(12)

# -------------- Full cesm - what we started with ----------

# Empty df to put everything in
all <- data.frame(matrix(ncol=27, nrow = 0)) # needs to be 10 longer than df

# For each setting of possible probability parameters we want to: 
# 1) generate worlds, 2) get conditional probabilities and 3) get model predictions
for (i in 1:length(poss_params)) { 
  # 1) Get possible world combos of two observed variables in both dis and conj structures
  dfd <- world_combos3(params = poss_params[[i]], structure = 'disjunctive')
  dfd$pgroup <- i
  dfc <- world_combos3(params = poss_params[[i]], structure = 'conjunctive')
  dfc$pgroup <- i
  # 2) Get conditional probabilities of these and the two unobserved variables too
  # newdfd <- get_cond_probs2(dfd)
  # newdfc <- get_cond_probs2(dfc)
  
  mp1 <- data.frame(matrix(ncol = 27, nrow = 0)) # was 27
  # 3) Get predictions of the counterfactual effect size model for these worlds 
      # We also want to calculate like 10 versions to get the variance of model predictions IT SHOULD BE RUNNING ON NEWDFD ETC
  for (m in 1:modelruns) {
    mpd <- get_cfs(params = poss_params[[i]], structure = 'disjunctive', df = dfd) # 16 obs of 6
    mpd$run <- m
    mpc <- get_cfs(params = poss_params[[i]], structure = 'conjunctive', df = dfc)
    mpc$run <- m
    mp1 <- rbind(mp1, mpd, mpc)
  }
  #mp1$pgroup <- i
  all <- rbind(all, mp1)  # 1440 of 26
} 
# It takes a minute or two but not terrible.
# saves intermediate set of world setup and model predictions
#write.csv(all, "../model_data/allnew1.csv") # 1920 of 29
write.csv(all, "../model_data/allnew2.csv") # 1440 of 26
