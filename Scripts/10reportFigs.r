################################################################################
########### Generate plots and other reporting figs using functions ##############
#################################################################################


library(tidyverse)
library(here)
library(ggnewscale) 
library(RColorBrewer)
library(ggplot2)




source(here('Scripts', 'plotUtils.R')) # Functions for plotting to compare model and ppts, using ggplot
load(here('Data', 'modelData', 'fitforplot16ig.rda')) # 288 of 35
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
  
plotf <- plot_model_pgroup('full', 'A=.1,Au=.7,B=.8,Bu=.5', df)
print(plotf)

ggsave(
  filename = "full3ig.pdf",
  plot = plotf,
  path = here("Other", "Plots"),
  width = 12,
  height = 6,
  units = "in"
)

# Another example of the model plot, this time for model noSelect:
  
plotns <- plot_model_pgroup('noSelect', 'A=.1,Au=.7,B=.8,Bu=.5', df)
print(plotns)

ggsave(
  filename = "ns3.pdf",
  plot = plotns,
  path = here("Other", "Plots"),
  width = 12,
  height = 6,
  units = "in"
)

# Usage of no model just ppts
#plot1 <- plot_nomodel_pgroup("A=.1,Au=.5,B=.8,Bu=.5", df)
#plot2 <- plot_nomodel_pgroup("A=.5,Au=.1,B=.5,Bu=.8", df)
#plot3 <- plot_nomodel_pgroup('A=.1,Au=.7,B=.8,Bu=.5', df)


# Usage: compare full and noSelect 

p4 <- plot_two_models_pgroup('full', 'noSelect', 'A=.1,Au=.5,B=.8,Bu=.5', df) #1
p4

ggsave(
  filename = "compfullns1.pdf",
  plot = p4,
  path = here("Other", "Plots"),
  width = 12,
  height = 6,
  units = "in"
)

p5 <- plot_two_models_pgroup('full', 'noSelect', 'A=.5,Au=.1,B=.5,Bu=.8', df) #2

ggsave(
  filename = "compfullns2.pdf",
  plot = p5,
  path = here("Other", "Plots"),
  width = 12,
  height = 6,
  units = "in"
)

p6 <- plot_two_models_pgroup('full', 'noSelect', 'A=.1,Au=.7,B=.8,Bu=.5', df) #3

ggsave(
  filename = "compfullns3.pdf",
  plot = p6,
  path = here("Other", "Plots"),
  width = 12,
  height = 6,
  units = "in"
)
