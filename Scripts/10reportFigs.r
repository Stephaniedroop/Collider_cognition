################################################################################
########### Generate plots and other reporting figs using functions ##############
#################################################################################


library(tidyverse)
library(here)
library(ggnewscale) 
library(RColorBrewer)
library(ggplot2)




source(here('Scripts', 'plotUtils.R')) # Functions for plotting to compare model and ppts, using ggplot
load(here('Data', 'modelData', 'fitforplot16k.rda')) # 288 of 35
#df <- fitforplot
pgroups <- levels(df$pgroup)

# Individual plots for all models for all pgroups: (can be used for visual comparisons but otherwise not expected to be needed)
# Uncomment if needed but it will print a lot of plots

# for (model in models) {
# for (pgroup in pgroups) {
# print(plot_model_pgroup(model, pgroup, df))
# }
# }

# Usage
# Instead, call a single model and pgroup plot like this for example full model for pgroup3:
  
plotna3 <- plot_model_pgroup('noAct', 'A=.1,Au=.7,B=.8,Bu=.5', df)
print(plotna3)

ggsave(
  filename = "noAct3.pdf",
  plot = plotna3,
  path = here("Other", "Plots"),
  width = 12,
  height = 6,
  units = "in"
)

# Best fitting model for other pgroups:

plotna2 <- plot_model_pgroup('noAct', 'A=.5,Au=.1,B=.5,Bu=.8', df)
print(plotna2)

ggsave(
  filename = "noAct2.pdf",
  plot = plotna2,
  path = here("Other", "Plots"),
  width = 12,
  height = 6,
  units = "in"
)

plotna1 <- plot_model_pgroup('noAct', 'A=.1,Au=.5,B=.8,Bu=.5', df)
print(plotna1)

ggsave(
  filename = "noAct1.pdf",
  plot = plotna1,
  path = here("Other", "Plots"),
  width = 12,
  height = 6,
  units = "in"
)

# ----------- Compare full and noAct to see why it is better --------------

pcomp1 <- plot_two_models_pgroup('full', 'noAct', 'A=.1,Au=.5,B=.8,Bu=.5', df) #1

ggsave(
  filename = "compfullna1.pdf",
  plot = pcomp1,
  path = here("Other", "Plots"),
  width = 12,
  height = 6,
  units = "in"
)

pcomp2 <- plot_two_models_pgroup('full', 'noAct', 'A=.5,Au=.1,B=.5,Bu=.8', df) #2

ggsave(
  filename = "compfullna2.pdf",
  plot = pcomp2,
  path = here("Other", "Plots"),
  width = 12,
  height = 6,
  units = "in"
)

pcomp3 <- plot_two_models_pgroup('full', 'noAct', 'A=.1,Au=.7,B=.8,Bu=.5', df) #3

ggsave(
  filename = "compfullna3.pdf",
  plot = pcomp3,
  path = here("Other", "Plots"),
  width = 12,
  height = 6,
  units = "in"
)



