# ================================================================
# General fit between model and people
# ================================================================


library(tidyverse)
library(here)

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

