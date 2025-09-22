##################################################################
######## Optimise params and NLL ################



library(tidyverse)
rm(list=ls())

#df <- read.csv('../Data/modelData/modelAndDataUnfitpn.csv')
df <- readRDS('../Data/modelData/modelAndDataUnfitpn.rda')

# Let's create variables coding the actual observation
df.map<-data.frame(condition=c('c1','c2','c3','c4','c5','d1','d2','d3','d4','d5','d6','d7'),
                   A=c(0,0,1,1,1, 0,0,0,1,1,1,1),
                   B=c(0,1,0,1,1, 0,1,1,0,0,1,1),
                   E=c(0,0,0,0,1, 0,0,1,0,1,0,1))

for (i in 1:nrow(df))
{
  df$A[i]<-df.map$A[df.map$condition==df$trialtype[i]]
  df$B[i]<-df.map$B[df.map$condition==df$trialtype[i]]
  df$E[i]<-df.map$E[df.map$condition==df$trialtype[i]]
}


df <- df |>
  mutate(include = !( (node3=='B=0' & B==1) | (node3=='B=1' & B==0) | (node3=='A=0' & A==1) | (node3=='A=1' & A==0)))

df <- df |> 
  rename(trial_id = pg_tt)
# Any result not 'real' or 'Actual' still needs some small prob value allocated by the softmax, so give -Inf here
df[df$include == FALSE, 9:16] <- -Inf


nons <- sum(df$n[df$include == FALSE]) #132 / 2580

ppl <- df |> 
  group_by(trial_id) |> 
  summarise(n=sum(n)) 

sum(ppl$n) # 2580

# For old experiment it is 50/3408 = .0147
# For new experiment it is 132/2580 = .0511

# Initial values for testing:
pars <- c(1, 1, 1)
mod_name <- 'full'
i <- 1




# Usage:
model_names <- c('full', 
                 'noAct', 
                 'noInf', 
                 'noSelect', 
                 'noActnoInf', 
                 'noActnoSelect', 
                 'noInfnoSelect', 
                 'noActnoInfnoSelect')  

results1 <- optimize_models(model_names, df)

print(results1)

# Initial values for testing:
pars <- c(1, 1)
mod_name <- 'noKind'
i <- 1



# Usage:
model_names2 <- c('noKind', 
                 'noActnoKind', 
                 'noInfnoKind', 
                 'noKindnoSelect', 
                 'noActnoInfnoKind', 
                 'noActnoKindnoSelect', 
                 'noInfnoKindnoSelect', 
                 'noActnoInfnoKindnoSelect',
                 'baseline')  

# Replace the predictions with the model_names2 (this is fine because the presence or absence of the other modules remains same)
df <- df |> 
  relocate(baseline, .before = Actual)

colnames(df)[9:17] <- model_names2

results2 <- optimize_models2(model_names2, df)

print(results2)


allpredictions <- rbind(results1$predictions, results2$predictions) # 4608: 36tt x 8 nodevals x 16 models 


## ---------------------------------------------------------------------------------------------------------------
df_wide <- allpredictions |>
  pivot_wider(
    id_cols = c(trial_id, node3),
    names_from = model,
    values_from = predicted_prob
  )



## ---------------------------------------------------------------------------------------------------------------
justppt <- df |> 
  select(trial_id, node3, n, prop, pgroup, Actual, A, B, E, include)

fitforplot <- merge(df_wide, justppt, by = c('trial_id', 'node3'))
fitforplot[, 22][is.na(fitforplot[, 22])] <- FALSE


## ---------------------------------------------------------------------------------------------------------------
#write.csv(fitforplot, '../Data/modelData/fit16mpn.csv')  

saveRDS(fitforplot, '../Data/modelData/fit16mpn.rda')

