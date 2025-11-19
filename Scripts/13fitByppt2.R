#####################################################
########### REAL Model fit by participant ################ !!!!!!!!!
#####################################################

library(tidyverse)
library(here)

set.seed(12)

# --------- Load what we need ---------
source(here('Scripts', 'optimUtilsNNN.R')) # functions to optimise
# Unfitted model scores
load(here('Data', 'modelData', 'modelAndDataUnfitig.rda')) # df, 288 of 23
# Participant data
load(here('Data', 'Data.Rdata')) # This is one big data, 2580, of 215 ppts

byppt <- data |>
  select(subject_id, trial_id, node3)

# Participant data
data_list <- split(byppt, byppt$subject_id) # 215 elements each of 12x14

# Test what one of the elements looks like
test <- data_list[[23]] # 12 obs of 3: subjid, trialid, node (what they answered in each trial)

# Merges the model1 series into each ppt, so now sum$n=12 for each df in the list
ppt_merges <- lapply(data_list, function(ppt_data) {
  merged <- df |>
    left_join(ppt_data, by = c('trial_id', 'node3')) |>
    mutate(n = ifelse(!is.na(subject_id), 1, 0)) |> # now always uses 'n'
    select(-subject_id)
  merged
})

test2 <- ppt_merges[[23]] # 12 obs of 23: trialid, node3, model preds, n=1
test3 <- ppt_merges[[124]]
test4 <- ppt_merges[[215]]

# Test on one ppt (one element of the list)
get_optimisation(model_names, test4, operative = WITH_KAPPA) # This works

# Call optimize_models(model_names, df) for every element of data_list - throws error!
results_3par <- lapply(ppt_merges, function(dat) {
  get_optimisation(model_names, dat, operative = WITH_KAPPA)
})

print(results_3par)

# ---------- The two par version ---------

# Rename columns 8:16 of every df in the ppt_merges list
ppt_merges <- lapply(ppt_merges, function(dat) {
  colnames(dat)[8:16] <- model_names2
  dat
})

results_2par <- lapply(ppt_merges, function(dat) {
  get_optimsation(model_names2, dat, operative = NO_KAPPA)
})

# Backing up - actually the baseline model needs a less stringent BIC because it has no parameters
# So access the last element of the BIC elements and change it to just -2 x the logl
results_2par <- lapply(results_2par, function(res) {
  # access the first element, which contains logl and BIC vectors
  res[[1]]$BIC[9] <- -2 * as.numeric(res[[1]]$logl[9])
  res
})

# Get some random ones and test
test5 <- results_2par[[60]][[1]]$BIC[9] # -24.95 = 49.9 - it will be the same for everyone

#print(results_2par)

# Save results of both series
save(
  #results_2par,
  results_3par,
  file = here('Data', 'modelData', 'individFits3par.rda')
)

# Now unlist to access, then format and store the results
best_models <- lapply(seq_along(results_2par), function(i) {
  # extract numeric BICs for this participant
  b2 <- as.numeric(results_2par[[i]][[1]]$BIC)
  b3 <- as.numeric(results_3par[[i]][[1]]$BIC)

  # extract model names from each fit
  n2 <- results_2par[[i]][[1]]$model
  n3 <- results_3par[[i]][[1]]$model

  # combine
  all_bics <- c(b2, b3)
  all_names <- c(n2, n3)

  # find best model
  best_idx <- which.min(all_bics)

  list(
    best_model = all_names[best_idx],
    best_bic = all_bics[best_idx],
    all_bics = all_bics,
    all_names = all_names
  )
})

# Store as df
best_df <- data.frame(
  subject_id = names(results_2par),
  best_model = sapply(best_models, `[[`, "best_model"),
  best_bic = sapply(best_models, `[[`, "best_bic"),
  stringsAsFactors = FALSE
)

# Define the order you want: 3-par models first, then 2-par
model_order <- c(model_names, model_names2)

# Convert best_model to factor with custom levels
best_df$model <- factor(best_df$best_model, levels = model_order)

# Now summarise
summary_table <- best_df |>
  group_by(model) |>
  summarise(participant_count = n(), .groups = "drop")

print(sum(summary_table$participant_count)) # 215


# What if this is no different from chance?
# run a chisq test on this distribution of counts, against an equal distribution
expected_counts <- rep(
  nrow(best_df) / length(model_order),
  length(model_order)
) # 12.6
chisq_test <- chisq.test(
  summary_table$participant_count,
  p = rep(1 / length(model_order), length(model_order))
)
print(chisq_test)
# X-squared = 216.8, df = 16, p-value = 2.2e-16

# Doesn't actually tell us much. Let's split into whether the model name mentions noAct or not
summary_table <- summary_table |>
  mutate(noAct = ifelse(grepl("noAct", model), "noAct", "Act"))
summary_table <- summary_table |>
  mutate(noInf = ifelse(grepl("noInf", model), "noInf", "Inf"))
summary_table <- summary_table |>
  mutate(noSelect = ifelse(grepl("noSelect", model), "noSelect", "Select"))
summary_table <- summary_table |>
  mutate(noKind = ifelse(grepl("noKind", model), "noKind", "Kind"))

# Remove the baseline row because no point
summary_table2 <- summary_table |>
  filter(model != "baseline")

# Now sum the participant count for each column of whether it is present or not
# Act no Act
noAct_table <- summary_table2 |>
  group_by(noAct) |>
  summarise(participant_count = sum(participant_count))
print(noAct_table)
# 108:84 Act:noAct

chisq_test_noAct <- chisq.test(noAct_table$participant_count, p = c(0.5, 0.5))
print(chisq_test_noAct) # x 3, p .08, no difference

# Inf no Inf
noInf_table <- summary_table2 |>
  group_by(noInf) |>
  summarise(participant_count = sum(participant_count))
print(noInf_table)
# 105:87 Inf:noInf

chisq_test_noInf <- chisq.test(noInf_table$participant_count, p = c(0.5, 0.5))
print(chisq_test_noInf) # x 1.69, p.19, no difference

# select no select
noSelect_table <- summary_table2 |>
  group_by(noSelect) |>
  summarise(participant_count = sum(participant_count))
print(noSelect_table)
# 101:93 Select:noSelect

chisq_test_noSelect <- chisq.test(
  noSelect_table$participant_count,
  p = c(0.5, 0.5)
)
print(chisq_test_noSelect) # x 30.1, p4.14e-8, significant difference

# kind no kind
noKind_table <- summary_table2 |>
  group_by(noKind) |>
  summarise(participant_count = sum(participant_count))
print(noKind_table)
# 110:84 Kind:noKind

chisq_test_noKind <- chisq.test(noKind_table$participant_count, p = c(0.5, 0.5))
print(chisq_test_noKind) # x 13.02, p.00031, significant difference

# a blocky mosaic chart to show the proportions
library(ggplot2)


# Simple df with Yes/No on top, then the four components down the side
combined_table <- data.frame(
  Component = rep(c("Act", "Inf", "Select", "Kind"), each = 2),
  Presence = rep(c("Yes", "No"), times = 4),
  Count = c(
    noAct_table$participant_count,
    noInf_table$participant_count,
    noSelect_table$participant_count,
    noKind_table$participant_count
  )
)
print(combined_table)

# Vertical, stacked, with label
p <- ggplot(data = combined_table) +
  geom_bar(
    aes(x = Component, y = Count, fill = Presence),
    stat = "identity",
    position = "fill"
  ) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  labs(
    title = "Proportion of participants best fit by models with/without each component",
    x = "Component",
    y = "Proportion of Participants"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  #scale_fill_manual(values = c("Yes" = "orange", "No" = "lightgrey")) +
  theme(legend.position = "top")

print(p)

# Horizontal, with label
p <- ggplot(data = combined_table) +
  geom_bar(
    aes(y = Component, x = Count, fill = Presence),
    stat = "identity",
    position = "fill"
  ) +
  geom_vline(xintercept = 0.5, linetype = "dotted") +
  geom_text(
    aes(y = Component, x = Count, label = Component, fill = Presence),
    stat = "identity",
    position = position_fill(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  labs(
    #title = "Proportion of participants best fit by models with/without each component",
    y = "Component",
    x = "Proportion of Participants"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "top", axis.text.y = element_blank())

combined_table$Component <- factor(
  combined_table$Component,
  levels = c("Act", "Inf", "Select", "Kind"),
  labels = c("Actual", "Inference", "Selection", "Kindness")
)

combined_table$label <- ifelse(
  combined_table$Presence == "No",
  paste0("no ", combined_table$Component),
  as.character(combined_table$Component)
)

# combined_table$Component <- factor(combined_table$Component,
#                                    levels = c("Selection", "Kindness",  "Inference", "Actual"))

# Horizontal, no label, but has No
p <- ggplot(data = combined_table) +
  geom_bar(
    aes(y = Component, x = Count, fill = Presence),
    stat = "identity",
    position = "fill"
  ) +
  geom_vline(xintercept = 0.5, linetype = "dotted") +
  geom_text(
    aes(y = Component, x = Count, label = label, fill = Presence),
    stat = "identity",
    position = position_fill(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  labs(y = "Component", x = "Proportion of Participants") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none", axis.text.y = element_blank())

print(p)

ggsave(
  filename = "bestByComponent.pdf",
  plot = p,
  path = here("Other", "Plots"),
  width = 12,
  #height = 8,
  units = "in"
)
