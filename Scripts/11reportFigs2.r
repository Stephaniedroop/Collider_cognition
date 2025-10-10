###########################################################
########### Other plots not using functions  ##############
###########################################################

library(tidyverse)
library(ggnewscale) # Load these if you don't have them
library(here)
library(RColorBrewer)
library(ggplot2)


load(here('Data', 'modelData', 'fitforplot16ig.rda')) # 288 of 35
source(here('Scripts', 'plotUtils.R')) 

# Now just observed v unobserved for everything together, noKind model. 
# It needs chunks to calculate separate long and summary dfs for the new se values - they are different because different numbers of observations in the means.


# Correlations for the very first model fit results
cor.test(df$prop, df$full) # .817
cor.test(df$prop, df$noAct) # .822
cor.test(df$prop, df$noSelect) # .906


# --------- KNOWN, FULL ----------
# Follow pattern of the one below for observed

# This doesn't mean anything any more! because Known is now modelled - no - took it out 

# 1. Gather and calculate mean and SE per group - 576 of 5
df_long0 <- df |>
  select(trial_id, node3, prop, full, Known) |> 
  gather(key, val, prop:full) |>
  filter(!is.na(val)) |>
  mutate(key = factor(key, levels = c("full", "prop"),
                      labels = c("Model", "Participants")))

summary_df0 <- df_long0 |>
  group_by(Known, key) |>
  summarise(
    val_sum = sum(val),
    se = sd(val) / sqrt(n())) |>
  mutate(val = val_sum / 36) |>
  select(-val_sum) |>
  ungroup()

# 2. Plot with bars and error bars
pknown <- ggplot(summary_df0, aes(x = key, y = val, fill = Known)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = val - se, ymax = val + se),
                width = 0.2, position = position_dodge(0.9)) +
  labs(x = "Response", y = "Proportion/Prediction", fill = "Known status") +
  scale_fill_brewer(palette = "Set2") + #, labels = c("", "")
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))

# A version with *** added, although now with new model no use
pknown <- ggplot(summary_df0, aes(x = key, y = val, fill = Known)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = val - se, ymax = val + se),
                width = 0.2, position = position_dodge(0.9)) +
  annotate("text", x = 1.5, y = max(summary_df0$val + summary_df0$se) + 0.05, 
           label = "***", hjust = 0.5, vjust = 0.5, size = 5) +
  labs(x = "Response", y = "Proportion/Prediction", fill = "Known") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))

print(pknown)

ggsave(
  filename = "pknown.pdf",
  plot = pknown,
  path = here("Other", "Plots"),
  width = 3.5,
  height = 4,
  units = "in"
)

# Interaction of Known and Observed DOESNT WORK BECAUSE ALL OBSERVED ARE KNOWN. also all known are actual so that split doesn't work

# -------- OBSERVED, NOKIND  -------

# 1. Gather and calculate mean and SE per group
# df_long1 <- df |>
#   select(trial_id, node3, prop, noKind, Observed) |> 
#   gather(key, val, prop:noKind) |>
#   filter(!is.na(val)) |>
#   mutate(key = factor(key, levels = c("noKind", "prop"),
#                       labels = c("Model", "Participants")))
# 
# summary_df1 <- df_long1 |>
#   group_by(Observed, key) |>
#   summarise(
#     val_sum = sum(val),
#     se = sd(val) / sqrt(n())
#   ) |>
#   mutate(val = val_sum / 36) |>
#   select(-val_sum) |>
#   ungroup()
# 
# 
# # 2. Plot with bars and error bars
# punnk <- ggplot(summary_df1, aes(x = key, y = val, fill = Observed)) +
#   geom_bar(stat = "identity", position = position_dodge(0.9)) +
#   geom_errorbar(aes(ymin = val - se, ymax = val + se),
#                 width = 0.2, position = position_dodge(0.9)) +
#   annotate("text", x = 1.5, y = max(summary_df1$val + summary_df1$se) + 0.05, 
#            label = "***", hjust = 0.5, vjust = 0.5, size = 5) +
#   labs(x = "Response", y = "Proportion/Prediction", fill = "Variable type") +
#   scale_fill_brewer(palette = "Set2", labels = c("Unobserved \n(Au|Bu", "Observed \n(A|B)")) +
#   #scale_fill_discrete(labels = c("Observed \n(A|B)", "Unobserved \n(Au|Bu)")) +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         legend.text = element_text(size = 10),
#         legend.title = element_text(size = 10))
# 
# print(punnk)
# 
# ggsave(
#   filename = "punnk.pdf",
#   plot = punnk,
#   path = here("Other", "Plots"),
#   width = 6,
#   height = 6,
#   units = "in"
# )

# --------- OBSERVED, FULL ----------

# Repeat just the observed v unobs for the full model just in case it's needed, 
# although it gives the same results as noKind, because k is so small in the best fitting models

# 1. Gather and calculate mean and SE per group
df_longf <- df |>
  select(trial_id, node3, prop, full, Observed) |> 
  gather(key, val, prop:full) |>
  filter(!is.na(val)) |>
  mutate(key = factor(key, levels = c("full", "prop"),
                      labels = c("Model", "Participants")))

summary_dff <- df_longf |>
  group_by(Observed, key) |>
  summarise(
    val_sum = sum(val),
    se = sd(val) / sqrt(n())
  ) |>
  mutate(val = val_sum / 36) |>
  select(-val_sum) |>
  ungroup()


# 2. Plot with bars and error bars
punf <- ggplot(summary_dff, aes(x = key, y = val, fill = Observed)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = val - se, ymax = val + se),
                width = 0.2, position = position_dodge(0.9)) +
  annotate("text", x = 1.5, y = max(summary_dff$val + summary_dff$se) + 0.05, 
           label = "***", hjust = 0.5, vjust = 0.5, size = 5) +
  labs(x = "Response", y = "Proportion/Prediction", fill = "Observed") +
  scale_fill_brewer(palette = "Set2") + #, labels = c("Unobserved \n(Au|Bu)", "Observed \n(A|B)")
  #scale_fill_discrete(labels = c("Observed \n(A|B)", "Unobserved \n(Au|Bu)")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))

print(punf)

ggsave(
  filename = "punf.pdf",
  plot = punf,
  path = here("Other", "Plots"),
  width = 3.5,
  height = 4,
  units = "in"
)


# --------- ACTUAL, FULL -----------


# 1. Gather and calculate mean and SE per group
df_longA <- df |>
  select(trial_id, node3, prop, full, Actual) |> 
  gather(key, val, prop:full) |>
  filter(!is.na(val)) |>
  mutate(key = factor(key, levels = c("full", "prop"),
                      labels = c("Model", "Participants")))

summary_dfA <- df_longA |>
  group_by(Actual, key) |>
  summarise(
    val_sum = sum(val),
    se = sd(val) / sqrt(n())
  ) |>
  mutate(val = val_sum / 36) |>
  select(-val_sum) |>
  ungroup()


# 2. Plot with bars and error bars
punA <- ggplot(summary_dfA, aes(x = key, y = val, fill = Actual)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = val - se, ymax = val + se),
                width = 0.2, position = position_dodge(0.9)) +
  annotate("text", x = 1.5, y = max(summary_dfA$val + summary_dfA$se) + 0.05, 
           label = "***", hjust = 0.5, vjust = 0.5, size = 5) +
  labs(x = "Response", y = "Proportion/Prediction", fill = "Actual") +
  scale_fill_brewer(palette = "Set2") + #, labels = c("Unobserved \n(Au|Bu", "Observed \n(A|B)")
  #scale_fill_discrete(labels = c("Observed \n(A|B)", "Unobserved \n(Au|Bu)")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))

print(punA)

ggsave(
  filename = "punA.pdf",
  plot = punA,
  path = here("Other", "Plots"),
  width = 3.5,
  height = 4,
  units = "in"
)





# ------------- STRUCTURE AND EFFECT, FULL --------------

#### A panel splitting out observed v observed by E=0/1, for the full model

# Now same thing for structure only
df.s <- df |> 
  gather(key, val, prop:full) |> 
  mutate(key = factor(key, levels = c('full', 'prop'),
                      labels = c("Full Model", "Participants"))) |>
  group_by(Observed, key, structure, E) |> 
  summarise(val = sum(val)/3)

df.s <- df.s |> 
  filter(!is.na(key))

df.s$struct_effect <- paste0(df.s$structure, df.s$E)

# Better
df.s$val <- ifelse(df.s$struct_effect == "conjunctive0", df.s$val/4,
                   ifelse(df.s$struct_effect == "disjunctive0", df.s$val/4,
                          ifelse(df.s$struct_effect == "disjunctive1", df.s$val/3, df.s$val)))


# This is the two-panel plot split for Effect, for the full model, to compare structures where E=0 with E=1:


punobs_f <- ggplot(df.s, aes(y=val, x=key, fill=Observed)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position = position_dodge(), colour = 'black') +
  stat_summary(fun.data = mean_se, geom = "errorbar",  position = position_dodge(.9), width = .2)+
  labs(x = 'Response', y = 'Proportion/Prediction') +
  facet_grid(structure ~ E, labeller = labeller(E = function(x) paste("Effect =", x))) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.margin=margin(c(0,0,0,0)),
        axis.title.x = element_text(margin = margin(t = 1, r =0, b = 0, l = 0)))

print(punobs_f)

ggsave(
  filename = "punobs_f.pdf",
  plot = punobs_f,
  path = here("Other", "Plots"),
  width = 4,
  height = 4,
  units = "in"
)


# Is this reverse effect also seen for the Known variables? Perhaps people only choose known variables when E=0

# --------------- STRUCTURE, KNOWN, FULL --------------

# ------------- STRUCTURE AND EFFECT, KNOWN, FULL --------------

#### A panel splitting out observed v observed by E=0/1, for the full model

# Now same thing for structure only
df.k <- df |> 
  gather(key, val, prop:full) |> 
  mutate(key = factor(key, levels = c('full', 'prop'),
                      labels = c("Full Model", "Participants"))) |>
  group_by(Known, key, structure, E) |> 
  summarise(val = sum(val)/3)

df.k <- df.k |> 
  filter(!is.na(key))

df.k$struct_effect <- paste0(df.k$structure, df.k$E)

# Better
df.k$val <- ifelse(df.k$struct_effect == "conjunctive0", df.k$val/4,
                   ifelse(df.k$struct_effect == "disjunctive0", df.k$val/4,
                          ifelse(df.k$struct_effect == "disjunctive1", df.k$val/3, df.k$val)))


# This is the two-panel plot split for Effect, for the full model, to compare structures where E=0 with E=1:


pk_f <- ggplot(df.k, aes(y=val, x=key, fill=Known)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position = position_dodge(), colour = 'black') +
  stat_summary(fun.data = mean_se, geom = "errorbar",  position = position_dodge(.9), width = .2)+
  labs(x = 'Response', y = 'Proportion/Prediction') +
  facet_grid(structure ~ E, labeller = labeller(E = function(x) paste("Effect =", x))) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.margin=margin(c(0,0,0,0)),
        axis.title.x = element_text(margin = margin(t = 1, r =0, b = 0, l = 0)))

print(pk_f)

ggsave(
  filename = "pk_f.pdf",
  plot = pk_f,
  path = here("Other", "Plots"),
  width = 4,
  height = 4,
  units = "in"
)

# Yes people and model really like to choose Known variables 

# Should do the same for Actual 

# --------------- STRUCTURE, ACTUAL, FULL --------------

# ------------- STRUCTURE AND EFFECT, ACTUAL, FULL --------------

#### A panel splitting out observed v observed by E=0/1, for the full model

# Now same thing for structure only
df.a <- df |> 
  gather(key, val, prop:full) |> 
  mutate(key = factor(key, levels = c('full', 'prop'),
                      labels = c("Full Model", "Participants"))) |>
  group_by(Actual, key, structure, E) |> 
  summarise(val = sum(val)/3)

df.a <- df.a |> 
  filter(!is.na(key))

df.a$struct_effect <- paste0(df.a$structure, df.a$E)

# Better
df.a$val <- ifelse(df.a$struct_effect == "conjunctive0", df.a$val/4,
                   ifelse(df.a$struct_effect == "disjunctive0", df.a$val/4,
                          ifelse(df.a$struct_effect == "disjunctive1", df.a$val/3, df.a$val)))


# This is the two-panel plot split for Effect, for the full model, to compare structures where E=0 with E=1:


pk_a <- ggplot(df.a, aes(y=val, x=key, fill=Actual)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position = position_dodge(), colour = 'black') +
  stat_summary(fun.data = mean_se, geom = "errorbar",  position = position_dodge(.9), width = .2)+
  labs(x = 'Response', y = 'Proportion/Prediction') +
  facet_grid(structure ~ E, labeller = labeller(E = function(x) paste("Effect =", x))) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.margin=margin(c(0,0,0,0)),
        axis.title.x = element_text(margin = margin(t = 1, r =0, b = 0, l = 0)))

print(pk_a)

ggsave(
  filename = "pk_a.pdf",
  plot = pk_a,
  path = here("Other", "Plots"),
  width = 6,
  height = 6,
  units = "in"
)



# Haven't rerun

# --------------- STRUCTURE AND EFFECT, NOKIND -------------

#### The next two chunks make a plot for noKind for unobserved v Effect

# NOKIND
# df.s2 <- df |> gather(key, val, prop:noKind) |> mutate(key = factor(key, levels = c('prop', 'noKind'),
#                                                                       labels = c("Participants", "noKind Model"))) |>
#   group_by(Observed, key, structure, E) |> summarise(val = sum(val)/3)
# 
# df.s2 <- df.s2 |> filter(!is.na(key))
# 
# df.s2$struct_effect <- paste0(df.s2$structure, df.s2$E)
# 
# # Better
# df.s2$val <- ifelse(df.s2$struct_effect == "conjunctive0", df.s2$val/4,
#                     ifelse(df.s2$struct_effect == "disjunctive0", df.s2$val/4,
#                            ifelse(df.s2$struct_effect == "disjunctive1", df.s2$val/3, df.s2$val)))
# 
# 
# # This is unobserved v observed two-panel plot for the noKind model, ie. the full model but without Kindness:
# 
# punobs_nk <- ggplot(df.s2, aes(y=val, x=key, fill=Observed)) +
#   stat_summary(fun.y = mean,
#                geom = "bar",
#                position = position_dodge(), colour = 'black') +
#   stat_summary(fun.data = mean_se, geom = "errorbar",  position = position_dodge(.9), width = .2)+
#   labs(x = 'Response', y = 'Proportion/Prediction', title = 'noKind model') +
#   facet_grid(structure ~ E) +
#   scale_fill_brewer(palette = "Set2") +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         legend.margin=margin(c(0,0,0,0)),
#         #legend.position = c(-.1,-.2),
#         axis.title.x = element_text(margin = margin(t = 1, r =0, b = 0, l = 0))) 
# 
# print(punobs_nk)
# 
# ggsave(
#   filename = "punobs_nk.pdf",
#   plot = punobs_nk,
#   path = here("Other", "Plots"),
#   width = 12,
#   #height = 6,
#   units = "in"
# )

#### The next two chunks make a plot for noSelect v Effect

# Now same thing for structure only
# df.s3 <- df |> gather(key, val, prop:noSelect) |> mutate(key = factor(key, levels = c('prop', 'noSelect'),
#                                                                         labels = c("Participants", "noSelect Model"))) |>
#   group_by(Observed, key, structure, E) |> summarise(val = sum(val)/3)
# 
# df.s3 <- df.s3 |> filter(!is.na(key))
# 
# df.s3$struct_effect <- paste0(df.s3$structure, df.s3$E)
# 
# # Better
# df.s3$val <- ifelse(df.s3$struct_effect == "conjunctive0", df.s3$val/4,
#                     ifelse(df.s3$struct_effect == "disjunctive0", df.s3$val/4,
#                            ifelse(df.s3$struct_effect == "disjunctive1", df.s3$val/3, df.s3$val)))
# 
# 
# # This is unobserved v observed two-panel plot for the noSelect model, 
# # ie. the full model but without CESM causal selection:
# 
# punobs_ns <- ggplot(df.s3, aes(y=val, x=key, fill=Observed)) +
#   stat_summary(fun.y = mean,
#                geom = "bar",
#                position = position_dodge(), colour = 'black') +
#   stat_summary(fun.data = mean_se, geom = "errorbar",  position = position_dodge(.9), width = .2)+
#   labs(x = 'Response', y = 'Proportion/Prediction', title = 'noSelect model') +
#   facet_grid(structure ~ E) +
#   scale_fill_brewer(palette = "Set2") +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         legend.margin=margin(c(0,0,0,0)),
#         #legend.position = c(-.1,-.2),
#         axis.title.x = element_text(margin = margin(t = 1, r =0, b = 0, l = 0))) 
# 
# print(punobs_ns)
# 
# ggsave(
#   filename = "punobs_ns.pdf",
#   plot = punobs_ns,
#   path = here("Other", "Plots"),
#   width = 12,
#   #height = 6,
#   units = "in"
# )

# -------------- 111 -----------------

### A section of plots for simple 111s for a few different models: full, noSelect, noKind
# There is a separate re-fit of the model for E=1 but that's a separaet issue than filtering for these cases

# FULL - split out Observed ACTUALLY GET FROM THE REFIT FOR 111

p111fO <- ggplot(df |> 
                  filter(trial_structure_type %in% 
                           c("Conjunctive: A=1,B=1,E=1", "Disjunctive: A=1,B=1,E=1"), Actual==T), # There was Actual==T here but see no reason for it
                aes(x=Variable, y=prop, fill=Observed)) +
  geom_bar(stat = 'identity', colour = 'black', position = position_dodge()) +
  geom_errorbar(aes(ymin=prop-SE, ymax=prop+SE), width=.2, position = position_dodge(.9)) + 
  labs(x = 'Response', y = 'Proportion/Prediction', title = 'Full model (dots) against participants (bars)') +
  scale_fill_brewer(palette = "Set2")+
  geom_point(data = df |> 
               filter(trial_structure_type %in% 
                        c("Conjunctive: A=1,B=1,E=1", "Disjunctive: A=1,B=1,E=1"), Actual==T),  # There was Actual==T here but see no reason for it
             aes(y=full), colour = 'black', size = 3, position = position_dodge(.9)) + # also here ', Actual==T' after D111) 
  facet_grid(trial_structure_type ~ pgroup, 
             labeller = labeller(pgroup = column_labeller, trial_structure_type = row_labeller)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #legend.position = 'none',
        axis.title.x = element_text(margin = margin(t = 1, r =0, b = 0, l = 0))) 

print(p111fO)

ggsave(
  filename = "p111fO.pdf",
  plot = p111fO,
  path = here("Other", "Plots"),
  width = 8,
  height = 8,
  units = "in"
)

# FULL - split out Actual - no - all this shows is that people did choose Actual variables almoat all the time

p111fA <- ggplot(df |> 
                   filter(trial_structure_type %in% 
                            c("Conjunctive: A=1,B=1,E=1", "Disjunctive: A=1,B=1,E=1")), # There was Actual==T here but see no reason for it
                 aes(x=Response, y=prop, fill=Actual)) +
  geom_bar(stat = 'identity', colour = 'black', position = position_dodge()) +
  geom_errorbar(aes(ymin=prop-SE, ymax=prop+SE), width=.2, position = position_dodge(.9)) + 
  labs(x = 'Response', y = 'Proportion/Prediction', title = 'Full model (dots) against participants (bars)') +
  scale_fill_brewer(palette = "Set2")+
  geom_point(data = df |> 
               filter(trial_structure_type %in% 
                        c("Conjunctive: A=1,B=1,E=1", "Disjunctive: A=1,B=1,E=1")),  # There was Actual==T here but see no reason for it
             aes(y=full), colour = 'black', size = 3, position = position_dodge(.9)) + # also here ', Actual==T' after D111) 
  facet_grid(trial_structure_type ~ pgroup, 
             labeller = labeller(pgroup = column_labeller, trial_structure_type = row_labeller)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #legend.position = 'none',
        axis.title.x = element_text(margin = margin(t = 1, r =0, b = 0, l = 0))) 

print(p111fA)

ggsave(
  filename = "p111fO.pdf",
  plot = p111fO,
  path = here("Other", "Plots"),
  width = 12,
  height = 12,
  units = "in"
)




# NOSELECT

# p111ns <- ggplot(df |> 
#                    filter(trial_structure_type %in% c("Conjunctive: A=1,B=1,E=1", "Disjunctive: A=1,B=1,E=1"), Actual==T),
#                  aes(x=Variable, y=prop, fill=Observed)) +
#   geom_bar(stat = 'identity', colour = 'black', position = position_dodge()) +
#   geom_errorbar(aes(ymin=prop-SE, ymax=prop+SE), width=.2, position = position_dodge(.9)) + 
#   labs(x = 'Response', y = 'Proportion/Prediction', title = 'noSelect model')+
#   scale_fill_brewer(palette = "Set2")+
#   geom_point(data = df |> filter(trial_structure_type %in% c("Conjunctive: A=1,B=1,E=1", "Disjunctive: A=1,B=1,E=1"), Actual==T), aes(y=noSelect), colour = 'red', size = 3, position = position_dodge(.9)) +
#   facet_grid(trial_structure_type ~ pgroup,
#              labeller = labeller(pgroup = column_labeller, trial_structure_type = row_labeller)) +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         #legend.position = 'none',
#         axis.title.x = element_text(margin = margin(t = 1, r =0, b = 0, l = 0))) 
# 
# print(p111ns)
# 
# ggsave(
#   filename = "p111ns.pdf",
#   plot = p111ns,
#   path = here("Other", "Plots"),
#   width = 12,
#   #height = 6,
#   units = "in"
# )

# NOKIND

# p111nk <- ggplot(df |> filter(trial_structure_type %in% c("Conjunctive: A=1,B=1,E=1", "Disjunctive: A=1,B=1,E=1"), Actual==T),
#                  aes(x=Variable, y=prop, fill=Observed)) +
#   geom_bar(stat = 'identity', colour = 'black', position = position_dodge()) +
#   geom_errorbar(aes(ymin=prop-SE, ymax=prop+SE), width=.2, position = position_dodge(.9)) + 
#   labs(x = 'Response', y = 'Proportion/Prediction', title = 'noKind model')+
#   scale_fill_brewer(palette = "Set2")+
#   geom_point(data = df |> filter(trial_structure_type %in% c("Conjunctive: A=1,B=1,E=1", "Disjunctive: A=1,B=1,E=1"), Actual==T), aes(y=noKind), colour = 'blue', size = 3, position = position_dodge(.9)) +
#   facet_grid(trial_structure_type ~ pgroup,
#              labeller = labeller(pgroup = column_labeller, trial_structure_type = row_labeller)) +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         #legend.position = 'none',
#         axis.title.x = element_text(margin = margin(t = 1, r =0, b = 0, l = 0))) 
# 
# print(p111nk)
# 
# ggsave(
#   filename = "p111nk.pdf",
#   plot = p111nk,
#   path = here("Other", "Plots"),
#   width = 12,
#   #height = 6,
#   units = "in"
# )

### Compound decision

# Did they suggest A or B?
# Same split as above but for Variable only:

df4 <- df |> 
  gather(key, val, prop:full) |> 
  mutate(key = factor(key, levels = c('prop', 'full'),
                      labels = c("Participants", "Full Model"))) |>
  group_by(Variable, key) |> 
  summarise(val = sum(val)/36)

df4 <- df4 |> 
  filter(!is.na(key))

pab <- ggplot(df4, aes(y=val, x=key, fill=Variable)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position = position_dodge(), colour = 'black') +
  stat_summary(fun.data = mean_se, geom = "errorbar",  position = position_dodge(.9), width = .2)+
  labs(x = 'Response', y = 'Proportion/Prediction', title = 'A v B') +
  #facet_grid(structure ~ E) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.margin=margin(c(0,0,0,0)),
        #legend.position = c(-.1,-.2),
        axis.title.x = element_text(margin = margin(t = 1, r =0, b = 0, l = 0))) 

print(pab)

ggsave(
  filename = "pab.pdf",
  plot = pab,
  path = here("Other", "Plots"),
  width = 12,
  #height = 6,
  units = "in"
)

# Now we want a chart like the 111 one, but for a compound decision for AvB. 
# This will split the complex disjunction of two conjunctions up. FULLMODEL

# FULL MODEL
df5 <- df |> 
  filter(trial_structure_type %in% c("Conjunctive: A=1,B=1,E=1", "Disjunctive: A=1,B=1,E=1")) |> 
  select(trial_id, pgroup, full, prop, trial_structure_type, Variable, Actual, SE) 

# Summarize mean proportions by group (A, B)
df_bar <- df5 |>
  group_by(pgroup, trial_structure_type, Variable) |>
  summarise(Participants = sum(prop), 
            FullModel = sum(full),
            .groups = 'drop')

# Summarize SE separately for error bars
df_error <- df5 |>
  group_by(pgroup, trial_structure_type, Variable) |>
  summarise(
    mean_prop = sum(prop),
    se = sqrt(sum(SE^2)),  # Example assuming independence; adapt based on your data
    .groups = 'drop'
  )

p111abf <- ggplot() +
  geom_col(data = df_bar, aes(x = Variable, y = Participants, fill = Variable),
           position = position_dodge()) +
  geom_errorbar(data = df_error,
                aes(x = Variable, ymin = mean_prop - se, ymax = mean_prop + se),
                width = 0.2,
                position = position_dodge(.9)) +
  scale_fill_brewer(palette = "Set2", name = "Participants \nand variable") +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  geom_point(data = df_bar, aes(x = Variable, y = FullModel, shape = "Full Model", color = "Full Model"),
             size = 3, position = position_dodge(.9), show.legend = TRUE) +
  scale_shape_manual(name = "", values = c("Full Model" = 16)) +
  scale_color_manual(name = "", values = c("Full Model" = "blue")) +
  labs(x = 'Response', y = 'Proportion/Prediction', title = 'A v B for the 111 cases') +
  facet_grid(trial_structure_type ~ pgroup) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(vjust = 0.5, hjust=1),
        legend.margin=margin(c(0,0,0,0)),
        axis.title.x = element_text(margin = margin(t = 1, r =0, b = 0, l = 0)),
        legend.text = element_text(size = 10),  # here thats 'Full Model'
        legend.title = element_text(size = 10))  # here 'Participants and variable'


print(p111abf)

ggsave(
  filename = "p111abf.pdf",
  plot = p111abf,
  path = here("Other", "Plots"),
  width = 12,
  #height = 6,
  units = "in"
)

# Then just the same, but for noSelect

# noSelect MODEL
df6 <- df |> 
  filter(trial_structure_type %in% c("Conjunctive: A=1,B=1,E=1", "Disjunctive: A=1,B=1,E=1")) |> 
  select(trial_id, pgroup, noSelect, prop, trial_structure_type, Variable, Actual, SE) 

# Summarize mean proportions by group (A, B)
df_bar <- df6 |>
  group_by(pgroup, trial_structure_type, Variable) |>
  summarise(Participants = sum(prop), 
            nSModel = sum(noSelect),
            .groups = 'drop')

# Summarize SE separately for error bars
df_error <- df6 |>
  group_by(pgroup, trial_structure_type, Variable) |>
  summarise(
    mean_prop = sum(prop),
    se = sqrt(sum(SE^2)),  
    .groups = 'drop'
  )

p111abns <- ggplot() +
  geom_col(data = df_bar, aes(x = Variable, y = Participants, fill = Variable),
           position = position_dodge()) +
  geom_errorbar(data = df_error,
                aes(x = Variable, ymin = mean_prop - se, ymax = mean_prop + se),
                width = 0.2,
                position = position_dodge(.9)) +
  scale_fill_brewer(palette = "Set2", name = "Participants \nand variable") +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  geom_point(data = df_bar, aes(x = Variable, y = nSModel, shape = "noSelect Model", color = "noSelect Model"),
             size = 3, position = position_dodge(.9), show.legend = TRUE) +
  scale_shape_manual(name = "", values = c("noSelect Model" = 16)) +
  scale_color_manual(name = "", values = c("noSelect Model" = "red")) +
  labs(x = 'Response', y = 'Proportion/Prediction', title = 'A v B for the 111 cases') +
  facet_grid(trial_structure_type ~ pgroup) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(vjust = 0.5, hjust=1),
        legend.margin=margin(c(0,0,0,0)),
        axis.title.x = element_text(margin = margin(t = 1, r =0, b = 0, l = 0)),
        legend.text = element_text(size = 10),  # here thats 'noSelect Model'
        legend.title = element_text(size = 10))  # here 'Participants and variable'


print(p111abns)

# NOTE: WHEN I WANT TO PUT THESE IN THE GITHUB README, CONVERT TO PNG OR JPEG FIRST THEN EMBED IN HTML OR MKD - it won't do pdf

ggsave(
  filename = "p111abns.pdf",
  plot = p111abns,
  path = here("Other", "Plots"),
  width = 12,
  #height = 6,
  units = "in"
)