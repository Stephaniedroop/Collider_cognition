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

load(here('Data', 'modelData', 'matchedByppt.rda')) 

# ------------- Test for OBSERVED  --------------
# It shows participants select OBSERVED less often than predicted by the model. 
# This is reported in the section `Unobserved vs observed variables` section.

predObs <- glmer(Observed ~ Respondent + (1|subject_id) + (1|trial_id), 
               data = merged, family = binomial(link='logit'))

summary(predObs)

coef <- fixef(predObs) # -0.22

est <- exp(coef) # 0.803 - exp converts logodds to odds. 
prob <- plogis(coef) #  plogis is exp/(1+exp) and converts logodds to probs

se <- sqrt(diag(vcov(predObs))) 

lower_logodds <- coef-(1.96*se)
upper_logodds <- coef+(1.96*se)

lower_or <- exp(lower_logodds) # .714
upper_or <- exp(upper_logodds) # .903

# ------------- Test for ACTUAL  --------------
# Not sure if we do need these further ones or if we need to parametrise. Still to discuss 