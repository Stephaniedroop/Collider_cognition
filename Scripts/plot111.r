# =================================
# Plot 111
# =================================


library(tidyverse)
library(ggnewscale) # Load these if you don't have them
library(here)
library(RColorBrewer)
library(ggplot2)


load(here('Data', 'ModelData', 'fitforplot111.rda')) # df, 96 of 35
source(here('Scripts', 'plotUtils.R')) 



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

print(p111fO) # It's the same as when pulled out of the whole fit, so whatever

ggsave(
  filename = "p111fO.pdf",
  plot = p111fO,
  path = here("Other", "Plots"),
  width = 8,
  height = 8,
  units = "in"
)