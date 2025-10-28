###########################################################
########### Other plots not using functions  ##############
###########################################################

library(tidyverse)
library(ggnewscale) # Load these if you don't have them
library(here)
library(RColorBrewer)
library(ggplot2)


load(here('Data', 'modelData', 'fitforplot16k.rda')) # 288 of 35
source(here('Scripts', 'plotUtils.R')) 

# Correlations for the very first model fit results
cor.test(df$prop, df$full) # .89
cor.test(df$prop, df$noAct) # .88
cor.test(df$prop, df$noSelect) # .84



# ------------  Just 111, for compound decision AvB -----------------

# noAct
df5 <- df |> 
  filter(trial_structure_type %in% c("Conjunctive: A=1,B=1,E=1", "Disjunctive: A=1,B=1,E=1")) |> 
  select(trial_id, pgroup, noAct, prop, trial_structure_type, Variable, Actual, SE) 

# Summarize mean proportions by group (A, B)
df_bar <- df5 |>
  group_by(pgroup, trial_structure_type, Variable) |>
  summarise(Participants = sum(prop), 
            Model = sum(noAct),
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
  geom_point(data = df_bar, aes(x = Variable, y = Model, shape = "Model", color = "Model"),
             size = 3, position = position_dodge(.9), show.legend = TRUE) +
  scale_shape_manual(name = "", values = c("Model" = 16)) +
  scale_color_manual(name = "", values = c("Model" = "blue")) +
  labs(x = 'Response', y = 'Proportion/Prediction') + #, title = 'A v B for the 111 cases'
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
  height = 6,
  units = "in"
)





# Tried hunners more plots but none had good message, so none used, examples below: contact me for others


# ------------- STRUCTURE AND EFFECT, FULL --------------

#### A panel splitting out observed v observed by E=0/1, for the full model

# Now same thing for structure only
df.s <- df |> 
  gather(key, val, prop:noAct) |> 
  mutate(key = factor(key, levels = c('noAct', 'prop'),
                      labels = c("Model", "Participants"))) |>
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
  gather(key, val, prop:noAct) |> 
  mutate(key = factor(key, levels = c('noAct', 'prop'),
                      labels = c("Model", "Participants"))) |>
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

