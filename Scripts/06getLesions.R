##################################################################
######## Get lesions and combine with data ############


library(tidyverse)
library(stringr)
rm(list=ls())

# Script takes the processed data from the collider ppt expt (`DATA.RDATA`) 
# and combines it with the pre-processed model predictions to get all the other model modules and lesions


load('../Data/Data.Rdata', verbose = T) # This is one big df, 'data', 2580, of 215 ppts
data <- df2
mp <- read.csv('../Data/modelData/tidied_predpn.csv') # 576 of 26 - 576 rows because: 3 pgroups x 12 trialtypes x 4 nodes x 4 prior possible settings of unobserved variables  


mp$pgroup <- as.factor(mp$pgroup)
mp$node3 <- as.factor(mp$node3)
mp$trialtype <- as.factor(mp$trialtype)
mp$structure <- as.factor(mp$structure)
mp$E.x <- as.factor(mp$E.x)
mp$E.y <- as.factor(mp$E.y)

# TO DO later - get the comments from the Rmd file and put them in here as well

# Condition 1
mp <- mp |> # 
  mutate(Actual = case_when(
    node2 == 'A' ~ A==E.x,
    node2 == 'B' ~ B==E.x,
    node2 == 'Au' ~ Au==E.x,
    node2 == 'Bu' ~ Bu==E.x
  ))

# Condition 2 - many of these are already caught but just to catch the extras 
mp$Actual[mp$A=='0' & mp$node3=='Au=1'] <- FALSE
mp$Actual[mp$B=='0' & mp$node3=='Bu=1'] <- FALSE



mp <- mp |> 
  mutate(cesmActual = cesm*Actual)


# FULL inc kindness #1
full <- mp |>  #1 
  group_by(pgroup, trialtype, node3, .drop=F) |> 
  summarise(full = sum(cesmActual*posterior)) 

# Unnormalised of course and not incorporating K / EMD yet



noAct <- mp |>  #2
  group_by(pgroup, trialtype, node3, .drop=F) |> 
  summarise(noAct = sum(cesm*posterior)) 

noInf <- mp |>  #3
  group_by(pgroup, trialtype, node3, .drop=F) |> 
  summarise(noInf = sum(cesmActual*PrUn)) 

noActnoInf <- mp |>  #6
  group_by(pgroup, trialtype, node3, .drop=F) |> 
  summarise(noActnoInf = sum(cesm*PrUn)) 




getpost <- mp |> # 120 obs of 4
  filter(!node2 %in% c('A','B')) |> # 
  group_by(pgroup, trialtype, node) |> 
  summarise(post = sum(posterior),
            prior = sum(PrUn),
            tv = round(abs((post-prior)),3)) # Maybe don't divide here by 2? Because both Au=1 and Au=0 need the value separately



postmp <- merge(mp, getpost, by = c('pgroup', 'trialtype', 'node'), all.x = TRUE)

# For A and B, gives 1 when E matches, 0 if not. This sets B to 1 for actual cause, eg. if B=0 when E=0
postmp <- postmp |> 
  mutate(Act1 = case_when(
    node2 == 'A' ~ Actual,
    node2 == 'B' ~ Actual,
    node2 == 'Au' ~ post,
    node2 == 'Bu' ~ post
  ))

postmp <- postmp |> 
  mutate(noSelect = case_when(
    node2 == 'A' ~ Actual,
    node2 == 'B' ~ Actual,
    node2 == 'Au' ~ post*Actual,
    node2 == 'Bu' ~ post*Actual
  ))



noSelect <- postmp |> #14
  group_by(pgroup, trialtype, node3, .drop=F) |> 
  summarise(noSelect = mean(noSelect)) 


noActnoSelect <- postmp |> #14
  group_by(pgroup, trialtype, node3, .drop=F) |> 
  summarise(noActnoSelect = mean(Act1)) 



mp <- mp |> 
  mutate(Act3 = case_when(
    node2 == 'A' ~ Actual,
    node2 == 'B' ~ Actual,
    node2 == 'Au' ~ peA*Actual,
    node2 == 'Bu' ~ peB*Actual
  ))

noInfnoSelect <- mp |> #10
  group_by(pgroup, trialtype, node3, .drop = F) |> 
  summarise(noInfnoSelect = mean(Act3))





mp <- mp |> # 
  mutate(Act2 = case_when(
    node2 == 'A' ~ Actual,
    node2 == 'B' ~ Actual,
    node2 == 'Au' ~ peA,
    node2 == 'Bu' ~ peB
  ))


noActnoInfnoSelect <- mp |> # no act here means for the unobserved variables 
  group_by(pgroup, trialtype, node3, .drop = F) |> 
  summarise(noActnoInfnoSelect = mean(Act2))




df_list <- list(full, 
                noAct, 
                noInf, 
                noSelect, 
                noActnoInf, 
                noActnoSelect, 
                noInfnoSelect, 
                noActnoInfnoSelect) 

models <- df_list |> reduce(full_join, by = c('pgroup', 'trialtype', 'node3'))



Actual <- mp |> select(pgroup, trialtype, node3, Actual) |> unique()


models2 <- merge(models, Actual, all.x = TRUE)


# ------------- 2. Summarise participant data in same format ---------------------

# First set factors so we can use tally
data$pgroup <- as.factor(data$pgroup)
data$node3 <- as.factor(data$node3)
data$trialtype <- as.factor(data$trialtype)

# dataNorm <- data |> # 289
#   group_by(pgroup, trialtype, node3, .drop=FALSE) |> 
#   tally |> 
#   mutate(prop=n/sum(n))
# r
dataNorm <- data |> 
  group_by(pgroup, trialtype, node3, .drop=FALSE) |> 
  tally() |> 
  mutate(prop=n/sum(n))


tv2 <- getpost |> 
  select(pgroup, trialtype, node, tv) |> 
  rename(node3 = node)

# Merge with data just because it's needed across all models so best do it once here
dataNorm <- merge(x=dataNorm, y=tv2, all.x = T) 



# ----------- 3. The actual merge! ------------ 

modelAndData <- merge(x=dataNorm, y=models2) 

modelAndData <- modelAndData |> 
  unite('pg_tt', pgroup, trialtype, sep = "_", remove = FALSE)

#modelAndData <- modelAndData[, c(1:4, 7:12, 5, 13:16)]

modelAndData <- modelAndData |>
  group_by(pg_tt) |>
  mutate(baseline = 1 / n())



## ---------------------------------------------------------------------------------------------------------------
save(modelAndData, file = '../Data/modelData/modelAndDataUnfitpn.rda')
 

