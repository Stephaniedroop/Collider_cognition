################################################################################
########### Master script for collider for new experiment - June 2025 ##########
#################################################################################

library(tidyverse)
#library(rjson)
library(ggnewscale) # Load these if you don't have them
library(here)
library(RColorBrewer)
library(ggplot2)
library(lme4)
library(lmerTest)

set.seed(12)

# ------------- 0. Source utils -------------
source(here('Scripts', 'cesmUtils.R')) # Functions for running the cesm model
source(here('Scripts', 'optimUtils.R')) # Functions for optimisation and likelihood calculation
source(here('Scripts', 'plotUtils.R')) # Functions for plotting to compare model and ppts, using ggplot

#--------------- 1. Get ppt data from behavioural experiment  -------------------
# (JavaScript for the experiment itself is in the folder Experiment. 
# For each participant a csv was saved on the server and then transferred out of there into Data)
# Demographics are in the `preprocessing` # file , quiet=TRUE
source(here('Scripts', '01preprocess.R')) #source(knitr::purl('preprocessing.Rmd')) # Collates individual csvs, reconciles with prolific report, saves `Data.Rdata` and also `ppts.csv`

#-------------- 2. Create parameters, run cesm, get model predictions and save them ------------
source(here('Scripts', '02setParams.R')) 
source(here('Scripts', '03getPreds.R')) # gets only cesm model predictions because those are the most complicated. All the other parts are compiled later in the lesions script
# Takes the probability vectors of settings of the variables from `set_params.R`. 

# Process model predictions to be more user friendly: take average of 10 model runs, wrangles and renames variables, splits out node values 0 and 1
source(here('Scripts', '04processPreds.R'))  #  

# -------------3. Results: fit model, compare predictions, plot etc

source(here('Scripts', '05getLesions.R')) # 
source(here('Scripts', '06optimise.R'))
source(here('Scripts', '07processForPlot.R')) 

# restructuring done to here

source(here('Scripts', '08samplePreds.R')) # Sample explanations from model. Input `fitforplot16m.rda` for model, `Data.Rdata` for ppt data, output `matchedByppt.rda`
source(here('Scripts', '09testSamp.R')) # Test the matched sampled explanations against participants for our theory metrics
# Note - the following reporting figs use the probdist style model predictions, not the matched sampled explanations, which are still for full model only
source(here('Scripts', '10reportFigs.R')) # Reporting plots on proportion model prediction, ie not the sampled explanations
source(here('Scripts', '11reportFigs2.R')) # Other calls not using functions; aggregate plots on unobserved v observed
source(here('Scripts', '12fitByppt.R')) # Best model fit by participant
# Plots to show difference in Known and Observed status between matched sampled explanations and participants
source(here('Scripts', '13plotMatchSamp.R')) # (maybe not in the best order: this takes output of 08 and 09)


# ------------- 4. Standalone analyses ------------
source(here::here("Scripts", "Standalone", "demogs.R"))

# These use a lot of text so are better as .Rmd. Statistics and numbers for reporting
 
# source(knitr::purl('itemLevelChisq.Rmd')) 
# source(knitr::purl('abnormalInflation.Rmd')) # Is the phenomenon found in our results? Only uses particpant Data.Rdata and doesn't need the model

# Check if cover story affects answers (it doesn't - except in 2/36 conditions, which can be ascribed to noise)
rmarkdown::render(
  input   = here::here("Scripts", "Standalone", "coverTest.Rmd"),
  output_file = here::here("Other", "Reports", "coverTest.html")
)





