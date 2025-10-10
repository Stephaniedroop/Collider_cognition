############################################################################################
########### Statistical tests on forced model samples matched to participant rows ##########
###############################################################################################


library(tidyverse)
library(here)
library(lme4)
library(lmerTest)

# Test the matched sampled explanations against participants for our theory metrics:
# - OBSERVED
# - ACTUAL
# - KNOWN
# - VARIABLE?

load(here('Data', 'modelData', 'matchedBypptig.rda')) # loads merged 5160 of 14

# Reference level is the MODEL and participants are compared to that.
# For each measure, the reference level is FALSE and TRUE is compared to that.

# ------------- Test for OBSERVED  --------------
# It shows participants select OBSERVED more often than predicted by the model. 
# This is reported in the section `Unobserved vs observed variables` section.

# Respondent is model 0; ppt 1. Observed is F/T
predObs <- glmer(Observed ~ Respondent + (1|subject_id) + (1|trial_id), 
               data = merged, family = binomial(link='logit'))

summary(predObs)

coef <- fixef(predObs) # .097

est <- exp(coef) # 1.102 - exp converts logodds to odds. 
prob <- plogis(coef) #  plogis is exp/(1+exp) and converts logodds to probs .589

se <- sqrt(diag(vcov(predObs))) 

lower_logodds <- coef-(1.96*se)
upper_logodds <- coef+(1.96*se)

lower_or <- exp(lower_logodds) # .98
upper_or <- exp(upper_logodds) # 1.24

# z 1.6 .11 NO RESULT

# ------------- Test for ACTUAL  --------------
# Participants select Actual causes more often than the model

predAct <- glmer(Actual ~ Respondent + (1|subject_id) + (1|trial_id), 
                 data = merged, family = binomial(link='logit'))

summary(predAct)

coef <- fixef(predAct) # .322

est <- exp(coef) # 1.38 exp converts logodds to odds. 
prob <- plogis(coef) #  plogis is exp/(1+exp) and converts logodds to probs .603

se <- sqrt(diag(vcov(predAct))) 

lower_logodds <- coef-(1.96*se)
upper_logodds <- coef+(1.96*se)

lower_or <- exp(lower_logodds) # 1.21
upper_or <- exp(upper_logodds) # 1.57

# z 4.81 p 1.53e-06

# ------------ Test for KNOWN -------------

# Participants select Known causes more often than the model HAVENT RERUN BECAUSE NOW WE MODEL KNOWN

predKn <- glmer(Known ~ Respondent + (1|subject_id) + (1|trial_id), 
                 data = merged, family = binomial(link='logit'))

summary(predKn)

coef <- fixef(predKn) # .52

est <- exp(coef) # 1.68 - exp converts logodds to odds. 
prob <- plogis(coef) #  plogis is exp/(1+exp) and converts logodds to probs .627

se <- sqrt(diag(vcov(predKn))) 

lower_logodds <- coef-(1.96*se)
upper_logodds <- coef+(1.96*se)

lower_or <- exp(lower_logodds) # 1.47
upper_or <- exp(upper_logodds) # 1.92

# z 7.65 p 2.01e-14
