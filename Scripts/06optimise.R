##################################################################
######## Optimise params and NLL ################
##################################################################


library(tidyverse)
library(here)
library(xtable)
source(here('Scripts', 'optimUtils.R')) # functions to optimise

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

# Currently in two versions, one for 3 pars and one for 2. Later will make the functions general for any number

# Initial values for testing:
#pars <- c(1, 1, 1)
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

results1 <- optimize_models(model_names, df, operatives_list)

print(results1)

# ---------- Format and save the results, series run in several blocks ----------

#---------- Model 1: -------------

# Useful names for tables
title1 <- "kappa acting on combinations of: ig, Known, Actual with ig restricted to 0 for Known=F" # CHANGE THIS TO CORRESPOND. Becomes caption in latex
filename1 <- "3par_series_igr" # title for html

# Format results1$model_fits in latex code in a table for publication in a paper
results1$model_fits[, -c(1, 2)] <- apply(results1$model_fits[, -c(1, 2)], 2, trim_zeros) # This in combination with the uncommented line in the function 
rownames(results1$model_fits) <- NULL

xtab <- xtable(results1$model_fits, digits = 3, caption = title1) # caption = c('Full title', 'Short title')

# Then print to file in both tex and html
print(xtab, file = here('Data', 'fitTables', paste0(filename1, '.tex')))     # LaTeX output
print(xtab, type = 'html', file = here('Data', 'fitTables', paste0(filename1, '.html')))

# ONLY USE THIS IF WE WANT TO PUT TABLES SIDE BY SIDE LATER
# combine_operatives_fits <- function(operatives_fits_list, suffix_position = "suffix") {
#   # operatives_fits_list: named list of data frames, e.g. list(op1 = df1, op2 = df2)
#   # suffix_position: either "suffix" or "prefix" for how to rename param columns
#   
#   # Helper to rename columns except 'model'
#   rename_params <- function(df, op_name) {
#     cols <- colnames(df)
#     # exclude 'model'
#     params <- setdiff(cols, "model")
#     
#     if(suffix_position == "suffix") {
#       new_names <- paste0(params, "_", op_name)
#     } else if (suffix_position == "prefix") {
#       new_names <- paste0(op_name, "_", params)
#     } else {
#       stop("suffix_position must be 'suffix' or 'prefix'")
#     }
#     
#     colnames(df)[colnames(df) %in% params] <- new_names
#     df
#   }
#   
#   # Rename columns for each operative
#   renamed_list <- mapply(rename_params, operatives_fits_list, names(operatives_fits_list), SIMPLIFY = FALSE)
#   
#   # Now reduce by joining all data frames on 'model'
#   combined <- Reduce(function(x, y) full_join(x, y, by = "model"), renamed_list)
#   
#   # Optional: reorder rows by model name or factor
#   combined <- combined %>% arrange(model)
#   
#   combined
# }
# 








# And also .rds with long descriptive name and short name for easy reference later
xtablabel <- list(
  data = xtab,
  info = "Full name: Table of GLM fits for anxiety outcome, tags: age-adjusted, generated 2025-10-14, N=2200"
)
saveRDS(xtablabel, file = here('Data', 'fitTables', paste0(filename1, '.rds')))

# ---------- Model 2 and onwards.... ------------



# Make a data structure like a list of lists for all the different model specs I want to try. 
# This will make it easier to loop through them all and insert as the operative part of the likelihood function
# Tried this in the utils file but too complicated



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

# Basically, currently, there is no need for kappa to scale the distance or information gain: 
# we get the benefit just by adding it to the model prediction


# Format results1$model_fits in latex code in a table for publication in a paper
results2$model_fits[, -1] <- apply(results2$model_fits[, -1], 2, trim_zeros) # This in combination with the uncommented line in the function 
xtable(results2$model_fits[, -1], digits = 3)

# ---------------- Test ai functions ----------------

# Was asking for efficient function that can do it differently depending on the number of parameters and what the specific prediction model is, but can't get it to write for me





# ------------- Once we have all predictions for a single model fits run, combine and process --------------------

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

save(fitforplot, file = here('Data', 'modelData', 'fit16ig.rda')) # Full is 8635. noActnoSelect fits best, at 8523
