##################################################################
######## Optimise params and NLL ################
##################################################################

library(tidyverse)
library(here)
library(xtable)
source(here('Scripts', 'modelNames.R'))
source(here('Scripts', 'optimUtils4par.R')) # functions to optimise CURRENTLY NO SCRIPT 5

set.seed(12)

load(here('Data', 'modelData', 'goOptim.rda')) # loads mp and df

# -------------- Run the functions ------------------

results <- get_optimisation(
  model_names = model_names, # e.g. names(models)
  mp = mp,
  df = df
)

print(results)

# ---------- Format and save the results

results$model_fits[, -1] <- lapply(results$model_fits[, -1], trim_zeros)
xtable(results$model_fits, digits = 3)

# Move the kappa column to before logl
results$model_fits <- results$model_fits |>
  relocate(kappa, .before = logl)

filename <- "4par" # title for html

xtab1 <- xtable(results$model_fits, digits = 3) # caption = c('Full title', 'Short title') if you wwant to include long tags

# Then print to file in both tex and html
print(xtab1, file = here('Data', 'fitTables', paste0(filename, '.tex'))) # LaTeX output
print(
  xtab1,
  type = 'html',
  file = here('Data', 'fitTables', paste0(filename, '.html'))
)

saveRDS(xtab1, file = here('Data', 'fitTables', paste0(filename, '.rds')))


# ------------- Combine and process --------------------

df_wide <- results$predictions |>
  pivot_wider(
    id_cols = c(trial_id, node3),
    names_from = model,
    values_from = predicted_prob
  )


fitforplot <- merge(df_wide, df, by = c('trial_id', 'node3')) # 288 of 37

## ---------------------------------------------------------------------------------------------------------------

save(fitforplot, file = here('Data', 'modelData', 'fit4par.rda')) #
