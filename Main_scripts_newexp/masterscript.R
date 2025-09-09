################################################################################
########### Master script for collider for new experiment - June 2025 ##########
#################################################################################

# setwd() # wherever you saved the files eg Collider_cognition
setwd("/Users/stephaniedroop/Documents/GitHub/Collider_cognition/Data")

library(tidyverse)
library(ggnewscale) # Download these if you don't have them
rm(list=ls())

# Please note, each script takes as output a csv or df produced in the previous one. 
# These have not been saved separately in the repo.
# If you need access to a later one only, you currently still need to run the previous scripts.

#--------------- 1. Get ppt data from behavioural experiment  -------------------
# (JavaScript for the experiment itself is in the folder Experiment. 
# For each participant a csv was saved on the server and then transferred out of there into Data)
# Demographics are in the `preprocessing` file

source('preprocessing.Rmd') # Takes individual csvs, collates them, performs reconciliation with prolific report
source('cover_story_test.Rmd') # Freestanding analysis to check if cover story affects answers (it doesn't - except in 2/36 conditions, which can be ascribed to noise)

# Setwd back again - it might still be in the previous script's one 
# (which it needed there to use a nifty one line to get the data docs out and together)
setwd("../Main_scripts")

#------- 1. Create parameters, run cesm, get model predictions and save them ------------
source('set_params.R')

source('get_model_preds4.R') # 
# Takes the probability vectors of settings of the variables from `set_params.R`. 
# Also loads source file `functionsN2.R` for 2 static functions which 1) generate world settings then CESM predictions

# Process model predictions to be more user friendly: take average of 10 model runs, wrangles and renames variables, splits out node values 0 and 1
source('modpred_processing2.R')  #  


# -------------3. Results: fit model, compare predictions, plot etc

source('modelCombLesions.Rmd') # puts the processed model predictions together with lesions to get a df called 'modelAndDataUnfit.csv'
# That is then sent to a few different scripts, following structure of paper:

source('optimise_withKandEps.Rmd')
source('reportingFigs16.Rmd')

# Some earlier checks 
source('itemLevelChisq.Rmd') # Check ppt n against a uniform distribution
source('abnormalInflation.Rmd') # Is the phenomenon found in our results?
source('samplePredictions.Rmd')






