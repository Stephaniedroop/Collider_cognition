##############################################################
########### Master script for collider for cogsci25 ##########
##############################################################

# setwd() # wherever you saved the files

library(tidyverse)
library(ggnewscale) # Download these if you don't have them
rm(list=ls())

# Please note, each script takes as output a csv or df produced in the previous one. 
# These have not been saved separately in the repo.
# If you need access to a later one only, you currently still need to run the previous scripts.

#------- 1. Create parameters, run cesm, get model predictions and save them ------------
source('set_params.R')

source('get_model_preds4.R') # 
# Takes the probability vectors of settings of the variables from `set_params.R`. 
# Also loads source file `functionsN1.R` for 2 static functions which 1) generate world settings then 
# model predictions for those and normalise/condition for unobserved variables

# Process model predictions to be more user friendly: 
# Takes average of 10 model runs
# Wrangles and renames variables, splits out node values 0 and 1
source('modpred_processing2.R')  #  

#--------------- 2. Get ppt data  -------------------
source('mainbatch_preprocessing.R') # saves data
# Setwd back again - it might still be in the previous script's one 
# (which it needed there to use a nifty one line to get the data docs out and together)
setwd("../Main_scripts")

# -------------3. Results: fit model, compare predictions, plot etc

source('modelCombLesions.Rmd') # puts the processed model predictions together with lesions to get a df called 'modelAndDataUnfit.csv'
# That is then sent to a few different scripts, following structure of paper:

source('itemLevelChisq.Rmd') # Check ppt n against a uniform distribution
source('abnormalInflation.Rmd') # Is the phenomenon found in our results?

source('optimise_noK.Rmd') # Get predictions and optimise: Get nll and tau for each model
source('reportingFigsnoK.Rmd') # 'K' is a parameter from later analysis; not needed here 



