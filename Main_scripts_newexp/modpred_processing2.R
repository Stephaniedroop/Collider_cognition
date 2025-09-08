####################################################### 
###### Collider - tidy up model predictions FULL  #####
#######################################################


# The functions from 'functions2' generated model predictions for CESM only. 
# To make the full model and all the lesioned version, run this preprocessing step and then combine in 'modelCombLesions'

library(tidyverse)

rm(list=ls())
all <- read.csv('../model_data/allpn.csv') # 1440

all$pgroup <- as.factor(all$pgroup)

# Bring in trialtype and rename as the proper string name 
all$trialtype <- all$groupPost
all$trialtype[all$trialtype==1 & all$structure=='disjunctive'] <- 'd1'
all$trialtype[all$trialtype==2 & all$structure=='disjunctive'] <- 'd2'
all$trialtype[all$trialtype==3 & all$structure=='disjunctive'] <- 'd3'
all$trialtype[all$trialtype==4 & all$structure=='disjunctive'] <- 'd4'
all$trialtype[all$trialtype==5 & all$structure=='disjunctive'] <- 'd5'
all$trialtype[all$trialtype==6 & all$structure=='disjunctive'] <- 'd6'
all$trialtype[all$trialtype==7 & all$structure=='disjunctive'] <- 'd7'

all$trialtype[all$trialtype==1 & all$structure=='conjunctive'] <- 'c1'
all$trialtype[all$trialtype==2 & all$structure=='conjunctive'] <- 'c2'
all$trialtype[all$trialtype==3 & all$structure=='conjunctive'] <- 'c3'
all$trialtype[all$trialtype==4 & all$structure=='conjunctive'] <- 'c4'
all$trialtype[all$trialtype==5 & all$structure=='conjunctive'] <- 'c5'

# First we have to average the model runs - goes from 1920 to 192
all <- all %>% group_by(pgroup, structure, index) %>% 
  mutate(A_cesm = mean(mA), Au_cesm = mean(mAu), B_cesm = mean(mB), Bu_cesm = mean(mBu)) %>% 
  distinct(pgroup, structure, index, .keep_all = TRUE)


# Pivot longer and list node names with their CESM values
all <- all %>% pivot_longer(cols = c(A_cesm:Bu_cesm), names_to = c('node', '.value'), names_sep = '_') 

all <- all %>% select(-(mA:run))

# 768 is then 1920/10 = 192 x 4 variables


# The unobserved variables have different explanatory role depending what we presume their value to be.
# So we need to split them out. First one with 6 (just for unobserved) 
all$node2 <- all$node
all$node[all$Au=='0' & all$node2=="Au"] <- 'Au=0'
all$node[all$Au=='1' & all$node2=="Au"] <- 'Au=1'
all$node[all$Bu=='0' & all$node2=="Bu"] <- 'Bu=0'
all$node[all$Bu=='1' & all$node2=="Bu"] <- 'Bu=1'
# Also need one with 8, where every node takes the value it has
all$node3 <- all$node
all$node3[all$A=='0' & all$node2=='A'] <- 'A=0'
all$node3[all$A=='1' & all$node2=='A'] <- 'A=1'
all$node3[all$B=='0' & all$node2=='B'] <- 'B=0'
all$node3[all$B=='1' & all$node2=='B'] <- 'B=1'

# Get a tag of the unobserved variables' settings. Then we can group data by this for plotting
all <- all %>% unite("uAuB", Au,Bu, sep= "", remove = FALSE)

# ------- 

# Also need a column for the actual settings
# They should be:
# c1: 000 
# c2: 010
# c3: 100
# c4: 110
# c5: 111
# d1: 000
# d2: 010
# d3: 011
# d4: 100
# d5: 101
# d6: 110
# d7: 111



# write this as csv in case need it later - 576 rows because: 3 pgroups x 12 trialtypes x 4 nodes x 4 prior possible settings of unobserved variables  
write.csv(all, '../model_data/tidied_predpn.csv')
