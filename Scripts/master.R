################################################################################
########### Master script for collider for new experiment - June 2025 ##########
#################################################################################

library(tidyverse)
#library(rjson)
library(ggnewscale) # Load these if you don't have them
library(here)
library(RColorBrewer)

# ------------- 0. Source utils -------------
source(here::here('Scripts', 'cesmUtils.R')) # Functions for running the cesm model
source(here('optimUtils.R')) # Functions for optimisation and likelihood calculation

#--------------- 1. Get ppt data from behavioural experiment  -------------------
# (JavaScript for the experiment itself is in the folder Experiment. 
# For each participant a csv was saved on the server and then transferred out of there into Data)
# Demographics are in the `preprocessing` # file , quiet=TRUE
source(here::here('Scripts', '01preprocess.R')) #source(knitr::purl('preprocessing.Rmd')) # Collates individual csvs, reconciles with prolific report, saves `Data.Rdata` and also `ppts.csv`
source(here('Scripts', '02coverTest')) #source(knitr::purl('cover_story_test.Rmd', quiet=TRUE)) # Freestanding analysis to check if cover story affects answers (it doesn't - except in 2/36 conditions, which can be ascribed to noise)

#------- 1. Create parameters, run cesm, get model predictions and save them ------------
source(here('Scripts'. '03setParams.R')) 
source(here('Scripts', '04getPreds.R')) # gets only cesm model predictions because those are the most complicated. All the other parts are compiled later in the lesions script
# Takes the probability vectors of settings of the variables from `set_params.R`. 


# Process model predictions to be more user friendly: take average of 10 model runs, wrangles and renames variables, splits out node values 0 and 1
source(here('Scripts', '05processPreds.R'))  #  


# -------------3. Results: fit model, compare predictions, plot etc

source(here('Scripts', '06getLesions.R')) #
#source(knitr::purl('modelCombLesions.Rmd')) # puts the processed model predictions together with lesions to get a df called 'modelAndDataUnfit.csv'
# That is then sent to a few different scripts, following structure of paper:
source(here('Scripts', '07optimise.R'))
# Uses 08optUtils.R for the functions to get likelihood and predictions
#source(knitr::purl('optimise_withKandEps.Rmd'))
source(here('Scripts', '08processForPlot.R')) 

source(here('Scripts', '09samplePredictions.R')) # Sample explanations from model
source(here('Scripts', '10reportingFigs16.R')) # Reporting plots on proportion model prediction, ie not the sampled explanations
source(here('Scripts', '11plotMatchedSamples.R')) # In process - so far gives the same layout as the simpler proportional ones
source(here('Scripts', '12by_ppt_fitting.R')) # Likelihood and model fit by participant

source(here('Scripts', '13itemLevelChisq.R')) # Check ppt n against a uniform distribution. Uses modelAndDataUnfit because that's a useful place for ppts grouped by trial_id, but doesn't use the model
source(here('Scripts', '14abnormalInflation.R')) # Is the phenomenon found in our results? Only uses particpant Data.Rdata and doesn't need the model

# source(knitr::purl('samplePredictions.Rmd')) 
# source(knitr::purl('reportingFigs16.Rmd')) 
# source(knitr::purl('plotMatchedSamples.Rmd')) 
# source(knitr::purl('by_ppt_fitting.Rmd')) 

# ------------- 4. Some earlier checks and standalone analyses
# source(knitr::purl('itemLevelChisq.Rmd')) 
# source(knitr::purl('abnormalInflation.Rmd')) # Is the phenomenon found in our results? Only uses particpant Data.Rdata and doesn't need the model







