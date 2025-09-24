##################################################################
######## Optimise params and NLL ################
##################################################################

load(here('Data', 'modelData', 'modelAndDataUnfit.rda')) # functions to run the model

# ------------- An extra section for reporting noisy answers -------------
nons <- sum(df$n[df$Include == FALSE]) #132 / 2580.   389???!!

ppl <- df |> 
  group_by(trial_id) |> 
  summarise(n=sum(n)) 

sum(ppl$n) # 2580

# For old experiment it is 50/3408 = .0147
# For new experiment it is 132/2580 = .0511 now it is 15.1 %???!!


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

save(fitforplot, file = here('Data', 'modelData', 'fit16mpn.rda'))
