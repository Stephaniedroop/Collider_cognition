################################################################################
########### Master script for Collider project started June 2025 ##########
#################################################################################

library(tidyverse)
#library(rjson)
library(ggnewscale)
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
source(here('Scripts', '01preprocess.R')) # Collates individual csvs, reconciles with prolific report, saves `Data.Rdata` and also `ppts.csv`

#-------------- 2. Create parameters, run cesm, get model predictions and save them ------------
source(here('Scripts', '02setParams.R')) # The baserates of the causal model
source(here('Scripts', '03getPreds.R')) # Gets cesm model predictions using `cesmUtils.R`.
# All the other model modules are set later in the lesions script

# Process model predictions to be more user friendly: take average of 10 model runs, wrangles and renames variables, splits out node values 0 and 1
source(here('Scripts', '04processPreds.R')) #  Also defines important values like 'Known', 'Actual' etc.

# -------------3. Results: fit model, compare predictions, plot etc

source(here('Scripts', '05getLesions.R')) # All other model modules apart from raw cesm scores
source(here('Scripts', '06optimise.R')) # Uses `optimUtils.R` to fit models.
# Has changed as of 2 Nov 2025 to fit all models in one go (ie both k series at the same time).
# It used to be you had to manually change whether you wanted k to be ig or known, and then run the whole series with what you chose, to compre the 8 with your chosen k, adn the 8 without
# PLots etc also generated from that. Now rethinking how to rerun with the all at once.

source(here('Scripts', '07processForPlot.R')) # Make model predictions use friendly for plotting

source(here('Scripts', '08samplePreds.R')) # Sample explanations from model. Input `fitforplot16m.rda` for model, `Data.Rdata` for ppt data, output `matchedByppt.rda`
source(here('Scripts', '09testSamp.R')) # Test the matched sampled explanations against participants for our theory metrics: Actual, Observed, Known

# The following reporting figs use the probdist style model predictions, not the matched sampled explanations, but they produce the same answers
source(here('Scripts', '10reportFigs.R')) # Main plots on every trial at once. Uses `plotUtils` functions to compare models
source(here('Scripts', '11reportFigs2.R')) # Other plots not using functions; aggregate and split plots
source(here('Scripts', '12fitByppt.R')) # Best model fit by participant. Input: data.rda and fitforplot from processForplot
source(here('Scripts', '13fitByppt2.R')) # Best actual model fit by participant. Input: data.rda and modelunfitig from 05getLesions

# ------------- 4. Standalone analyses ------------
source(here("Scripts", "Standalone", "demogs.R"))
source(here("Scripts", "Standalone", "chisq.R")) # Uses the `unfitmodelpredictions` data but only to get grouped ppt numbers. Doesn't vary with different models
source(here("Scripts", "Standalone", "abnormalInflation.R")) # Tests for overall presence of the effect seen in the plot of the 111 conditions by pgroup and structure - doesn't find any because it's only seen in pgroup 1

# Check if cover story affects answers (it doesn't - except in 2/36 conditions due to noise)
rmarkdown::render(
  input = here::here("Scripts", "Standalone", "coverTest.Rmd"),
  output_file = here::here("Other", "Reports", "coverTest.html")
)
