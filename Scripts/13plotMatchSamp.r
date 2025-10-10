############################################################################################
########### Plotting and tests on the matched sampled model explanations  ##########
#########################################################################################

library(tidyverse)
library(here)

load(here('Data', 'modelData', 'matchedBypptig.rda')) # loads merged 5160 of 14
df <- merged


# --------- KNOWN ----------
summary_dfK <- df |> 
  group_by(Respondent, Known) |> 
  summarise(n=n())

# 2580 because 5160/2, aka 12 trials per ppt, 215 ppts
ggplot(summary_dfK, aes(x = Respondent, y = n/2580, fill = Known)) +
  geom_bar(stat = "identity", position = "dodge") 


# --------- OBSERVED -----------

summary_dfO <- df |> 
  group_by(Respondent, Observed) |> 
  summarise(n=n())

ggplot(summary_dfO, aes(x = Respondent, y = n/2580, fill = Observed)) +
  geom_bar(stat = "identity", position = "dodge") 



