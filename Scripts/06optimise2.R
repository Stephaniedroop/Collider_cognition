##################################################################
######## Optimise params and NLL ################
##################################################################

library(tidyverse)
library(here)
library(xtable)
source(here('Scripts', 'optimUtilsNNN.R')) # functions to optimise

set.seed(12)

load(here('Data', 'modelData', 'modelAndDataUnfitig.rda')) # df, 288 of 23

# ------------- An extra section for reporting noisy answers -------------
# nons <- sum(df$n[df$Include == FALSE]) #132 / 2580.   389???!!
#
# ppl <- df |>
#   group_by(trial_id) |>
#   summarise(n=sum(n))
#
# sum(ppl$n) # 2580

# For old experiment it is 50/3408 = .0147
# For new experiment it is 132/2580 = .0511 now it is 15.1 %???!!

# -------------- Run the functions ------------------

# Usage: FIRST FOR THE KAPPA SERIES, on the first series of model_names (listed in optimUtils)

# Run the series of optimisation functions from the utils file
results1 <- get_optimisation(model_names, df, operative = WITH_KAPPA)


# SECOND FOR THE NO-KAPPA SERIES

# It's the same functions but just need to rename the unfit df columns first
colnames(df)[8:16] <- model_names2

# Run the series of optimisation functions from the utils file, on the second series of model_names
results2 <- get_optimisation(model_names2, df, operative = NO_KAPPA)
# BIC did not need optimised - the params dont mean anything - replace manually in the table if need be

# ---------- Format and save the results, series run in several blocks ----------

#---------- Model 1: -------------

results1$model_fits[, -1] <- lapply(results1$model_fits[, -1], trim_zeros)
xtable(results1$model_fits, digits = 3)

# Move the kappa column to before logl
results1$model_fits <- results1$model_fits |>
  relocate(kappa, .before = logl)

filename1 <- "3par_series_ig" # title for html

xtab1 <- xtable(results1$model_fits, digits = 3) # caption = c('Full title', 'Short title') if you wwant to include long tags

# Then print to file in both tex and html
print(xtab1, file = here('Data', 'fitTables', paste0(filename1, '.tex'))) # LaTeX output
print(
  xtab1,
  type = 'html',
  file = here('Data', 'fitTables', paste0(filename1, '.html'))
)

saveRDS(xtab1, file = here('Data', 'fitTables', paste0(filename1, '.rds')))

# ---- 2-par series - results2 ------
results2$model_fits[, -1] <- lapply(results2$model_fits[, -1], trim_zeros)
xtable(results2$model_fits, digits = 3)

filename2 <- "2par_series_ig" # title for html

xtab2 <- xtable(results2$model_fits, digits = 3) # caption = c('Full title', 'Short title') if you wwant to include long tags

# Then print to file in both tex and html
print(xtab2, file = here('Data', 'fitTables', paste0(filename2, '.tex'))) # LaTeX output
print(
  xtab2,
  type = 'html',
  file = here('Data', 'fitTables', paste0(filename2, '.html'))
)

saveRDS(xtab2, file = here('Data', 'fitTables', paste0(filename2, '.rds')))

# ------------- Combine and process --------------------

allpredictions <- rbind(results1$predictions, results2$predictions) # 4896: 36tt x 8 nodevals x 17 models

## ---------------------------------------------------------------------------------------------------------------
df_wide <- allpredictions |>
  pivot_wider(
    id_cols = c(trial_id, node3),
    names_from = model,
    values_from = predicted_prob
  )

## ---------------------------------------------------------------------------------------------------------------
justppt <- df |>
  select(
    trial_id,
    node3,
    n,
    prop,
    pgroup,
    Actual,
    A,
    B,
    E,
    Include,
    Observed,
    Known
  )

fitforplot <- merge(df_wide, justppt, by = c('trial_id', 'node3')) # 288 of 37

## ---------------------------------------------------------------------------------------------------------------

save(fitforplot, file = here('Data', 'modelData', 'fit16ig.rda')) #
