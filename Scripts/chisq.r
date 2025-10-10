# ================================================================
# General correlations between model and people
# ================================================================


library(tidyverse)
library(here)
#library(xtable)
#source(here('Scripts', 'optimUtils.R')) # functions to optimise

load(here('Data', 'modelData', 'modelAndDataUnfitig.rda')) # df, 288 of 23



# As a top-level sanity check that people are answering some sort of question: do they answer differently from uniform?
# We check this by first grouping by world, and then run a series of 36 chi-square tests.

# The first section prints a 36 Bonferroni-corrected chisq test results, and finds 
# in every one of the 36 worlds, that people answer in a patterned way, ie. different from uniform or at random.

results <- df |> 
  group_by(trial_id) |> 
  summarise(
    chi_sq_stat = chisq.test(n, p = rep(1/n(), n()))$statistic,
    p_value     = chisq.test(n, p = rep(1/n(), n()))$p.value
  ) %>%
  mutate(
    p_adj = p.adjust(p_value, method = "bonferroni")
  )

print(results)

print(min(results$chi_sq_stat)) # 50.5
print(max(results$p_adj)) # 4e-7

# Now Actual

# Similarly, to get a high level check of how often participants selected ACTUAL causes as their explanation, vs non actual.

df$Actual[is.na(df$Actual)] <- FALSE

res <- df |> 
  group_by(Actual) |> 
  summarise(n=sum(n)) # 1972 / 608 = 2580

print(chisq.test(res$n, p=c(0.5,0.5)))



# Unobserved

# And then again, to get a high-level check of whether they select unobserved causes over observed causes. 
# This is theoretically interesting.

df <- df |> 
  mutate(Observed = factor(!node3%in%c('Au=0','Au=1','Bu=0', 'Bu=1'), levels = c(T,F)))
# Then, like res
unobs <- df |> 
  group_by(Observed) |> 
  summarise(n=sum(n)) # 1152 / 1428 = 55.3%

print(chisq.test(unobs$n, p=c(0.5,0.5)))

# Known

df$Known <- as.factor(df$Known)
known <- df |> 
  group_by(Known) |> 
  summarise(n=sum(n)) # 1882 / 698 = 2580

print(chisq.test(known$n, p=c(0.5,0.5)))
