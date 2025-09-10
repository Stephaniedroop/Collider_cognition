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
# Demographics are in the `preprocessing` # file , quiet=TRUE
source(knitr::purl('preprocessing.Rmd')) # Collates individual csvs, reconciles with prolific report, saves `Data.Rdata` and also `ppts.csv`
source(knitr::purl('cover_story_test.Rmd', quiet=TRUE)) # Freestanding analysis to check if cover story affects answers (it doesn't - except in 2/36 conditions, which can be ascribed to noise)

#------- 1. Create parameters, run cesm, get model predictions and save them ------------
source('set_params.R')
source('get_model_preds4.R') # 
# Takes the probability vectors of settings of the variables from `set_params.R`. 
# Also loads source file `functionsN2.R` for 2 static functions which 1) generate world settings then CESM predictions

# Process model predictions to be more user friendly: take average of 10 model runs, wrangles and renames variables, splits out node values 0 and 1
source('modpred_processing2.R')  #  


# -------------3. Results: fit model, compare predictions, plot etc

source(knitr::purl('modelCombLesions.Rmd')) # puts the processed model predictions together with lesions to get a df called 'modelAndDataUnfit.csv'
# That is then sent to a few different scripts, following structure of paper:

source(knitr::purl('optimise_withKandEps.Rmd'))
source(knitr::purl('processForPlot.Rmd'))
source(knitr::purl('samplePredictions.Rmd')) # Sample explanations from model
source(knitr::purl('reportingFigs16.Rmd')) # Reporting plots on proportion model prediction, ie not the sampled explanations
source(knitr::purl('plotMatchedSamples.Rmd')) # In process - so far gives the same layout as the simpler proportional ones
source(knitr::purl('by_ppt_fitting.Rmd')) # Likelihood and model fit by participant

# ------------- 4. Some earlier checks and standalone analyses
source(knitr::purl('itemLevelChisq.Rmd')) # Check ppt n against a uniform distribution. Uses modelAndDataUnfit because that's a useful place for ppts grouped by trial_id, but doesn't use the model
source(knitr::purl('abnormalInflation.Rmd')) # Is the phenomenon found in our results? Only uses particpant Data.Rdata and doesn't need the model







