# ==========================================================
#   Alternative model fits on just the E=1 cases 
# ==========================================================

# LIke optimise script, but first filter for only E=1 cases

library(tidyverse)
library(here)
library(xtable)
source(here('Scripts', 'optimUtils.R')) # functions to optimise

load(here('Data', 'modelData', 'modelAndDataUnfitig.rda')) # df, 288 of 23

# Then filter for trialtype %in% c('c5', 'd3', 'd5', d7')
df <- df |> 
  filter(trialtype %in% c('c5', 'd3', 'd5', 'd7')) # 96 obs


# -------------- Run the functions ------------------

# Currently in two versions, one for 3 pars and one for 2. Later will make the functions general for any number

# Initial values for testing:
pars <- c(1, 1, 1)
mod_name <- 'full'
i <- 1

# Usage:
model_names <- c('full', 
                 'noAct', 
                 'noInf', 
                 'noSelect', 
                 'noActnoInf', 
                 'noActnoSelect', 
                 'noInfnoSelect', 
                 'noActnoInfnoSelect')  

results1 <- optimize_models(model_names, df)

print(results1)

# Format results1$model_fits in latex code in a table for publication in a paper
# This way doesn't print the model names but we can get them above
results1$model_fits <- sapply(results1$model_fits, trim_zeros)
xtable(results1$model_fits, digits = 3)

# -------------- The two par version --------------

# Initial values for testing:
pars <- c(1, 1)
mod_name <- 'noKind'
i <- 1

# Usage:
model_names2 <- c('noKind', 
                  'noActnoKind', 
                  'noInfnoKind', 
                  'noKindnoSelect', 
                  'noActnoInfnoKind', 
                  'noActnoKindnoSelect', 
                  'noInfnoKindnoSelect', 
                  'noActnoInfnoKindnoSelect',
                  'baseline')  

# Replace the predictions with the model_names2 (this is fine because the presence or absence of the other modules remains same)
df <- df |> 
  relocate(baseline, .before = Actual)

colnames(df)[8:16] <- model_names2

results2 <- optimize_models2(model_names2, df)

print(results2)



# Format results1$model_fits in latex code in a table for publication in a paper
# This way doesn't print the model names but we can get them above
results2$model_fits <- sapply(results2$model_fits, trim_zeros)
xtable(results2$model_fits, digits = 3)


allpredictions <- rbind(results1$predictions, results2$predictions) # 4608: 36tt x 8 nodevals x 16 models 

## ---------------------------------------------------------------------------------------------------------------
df_wide <- allpredictions |>
  pivot_wider(
    id_cols = c(trial_id, node3),
    names_from = model,
    values_from = predicted_prob
  )

## ---------------------------------------------------------------------------------------------------------------
justppt <- df |> 
  select(trial_id, node3, n, prop, pgroup, Actual, A, B, E, Include, Observed, Known)

fitforplot <- merge(df_wide, justppt, by = c('trial_id', 'node3'))
# Set the NAs in Actual to False
fitforplot[, 23][is.na(fitforplot[, 23])] <- FALSE

## ---------------------------------------------------------------------------------------------------------------

save(fitforplot, file = here('Data', 'modelData', 'fit111.rda'))
