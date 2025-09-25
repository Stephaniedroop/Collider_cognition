#####################################################
########### Model fit by participant ################
#####################################################

library(tidyverse)
library(here)


load(here('Data', 'Data.Rdata')) # This is one big df, 'df2', 2580, of 215 ppts

# Now select just the columns you need
byppt <- df2 |> 
  select(subject_id, trial_id, node3, include)

df <- read.csv('../model_data/fitforplot16mpn.csv') 


ppts <- merge(byppt, df, by = c('trial_id', 'node3'))

# Want the model preds fitted to the aggregate group

# So, split df2 into ppts list
# Some ppts did see some conditions more than once - due to a counterbalancing error in the experiment design.

data_list <- split(ppts, ppts$subject_id) # 215 elements each of 12x14

test <- data_list[[2]]

exclude_cols <- test |> 
  select (trial_id:X, n:include.y)

model_names <- c('full', 
                 'noAct', 
                 'noInf', 
                 'noSelect', 
                 'noActnoInf', 
                 'noActnoSelect', 
                 'noInfnoSelect', 
                 'noActnoInfnoSelect',
                 'noKind', 
                 'noActnoKind', 
                 'noInfnoKind', 
                 'noKindnoSelect', 
                 'noActnoInfnoKind', 
                 'noActnoKindnoSelect', 
                 'noInfnoKindnoSelect', 
                 'noActnoInfnoKindnoSelect',
                 'baseline')  

# Compute all NLLs once per subject, storing both full NLLs and min info together
results <- lapply(data_list, function(dat) {
  nlls <- sapply(model_names, function(mod_col) {
    mpred <- dat[[mod_col]]
    -sum(log(mpred))
  })
  list(
    nlls = nlls,
    min_nll = min(nlls),
    min_model = names(nlls)[which.min(nlls)]
  )
})

# Extract min_model vector for later
best_models <- sapply(results, function(x) x$min_model)
best_models_df <- data.frame(model = best_models, stringsAsFactors = FALSE)

# Construct a dataframe of all NLLs per subject
nll_matrix <- t(sapply(results, function(x) x$nlls))
nll_df <- as.data.frame(nll_matrix)
rownames(nll_df) <- names(results)

# Count how many participants have each model as best fit
summary_table <- best_models_df |>
  group_by(model) |>
  summarise(participant_count = n()) |>
  arrange(desc(participant_count))


all_models <- data.frame(model = model_names)

summary_table_full <- all_models |>
  left_join(summary_table, by = "model") |>
  mutate(participant_count = ifelse(is.na(participant_count), 0, participant_count))

print(summary_table_full)