# Scripts and data for a behavioural experiment and computational models to study how human cognition uses both causal selection and causal inference to generate explanations for observed phenomena

## Authors

- Stephanie Droop (stephanie.droop@ed.ac.uk)
- Neil Bramley
- Tadeg Quillien
- (some consultation from Christopher Lucas)

## Languages

All scripts are in R, v.4.1. Packages needed: tidyverse, lme4, lmerTest, stringr, ggnewscale, RColorBrewer

## How to run

- or **analysis** go to the `Scripts` folder and run `master.R`, or if you know what script you are looking for you can go straight there and run that only. See list of scripts below.
- This structure assumed you have a renv and .rproj doc with here set to the .rproj.
- To see and try the behavioural experiment go to https://eco.ppls.ed.ac.uk/~s0342840/collidern/collidertop.html. Code for the task interface and structure of running it in JavaScript in in the folder `Other/hostExperiment`. To click through the experiment: at the comprehension check enter Yes, No, True, 12.

## Files, folders, model

### FOLDER Scripts

- `master.R` - top level analysis script. **Go here first**

It will first source a few scripts holding static utilities: 
- `cesmUtils.R` - functions to run the CESM model
- `optimUtils.R` - functions to fit and optimise models
- `plotUtils.R` - functions to generate plots

The masterscript sources scripts in the following order:

a) Processing behavioural experiment data (see folder `Other/hostExperiment` for the JS code of experiment):

- `01preprocess.R` - Get the participants' behavioural experiment data ready. Saves as `Data.Rdata` in folder `Data`

2. Set up Collider worlds (see note on worlds below) and get CESM (Quillien+Lucas23) model predictions for those for the _causal selection_ part of the model:

_Btw in the middle of a reconstruction of files and names at 21 Sept 2025_

- `02setParams.R` - short script to set the manipulated probabilities of the variables
- `03getPreds.R` - get CESM model predictions. Output `all.rda`
- `04processPreds.R` - put model predictions in a user-friendly format, renames variables, splits out node values 0 and 1. Input: `all.rda`, output `modelproc.rda`

3. Other modelling

- `05getLesions.R` calculates the _inference_, _computational kindness_ and _actual causality_ parts of the full models, then progressively lesions. Input `modelproc.rda` and `Data.rdata`, output `modelAndDataUnfit.rda`
- `06optimise.Rmd` fits and optimises models. Input `modelAndDataUnfit.rda`, output `fit16mpn.csv`.
- `07processForPlot.Rmd` - an intermediate file for the clumsy relabelling, to stop the actual plotting file getting unwieldy. Input: `fit16mpn.csv`; output `fitforplot16mpn.csv`
- `08samplePreds.Rmd` - get an actual sample from model for each participant trial. Input: `Data.Rdata` and `fitforplot16mpn.csv`, output: a glmer result in the console and reported in the Unobserved variables section of the paper and `forplotbyppt.csv`. (TO DO: should this not be used for the freestanding analyses scripts too)
- `09testSamp.R` - Test the matched sampled explanations against participants for our theory metrics
- `10reportFigs.R` generates plots on a pattern with functions from `plotUtils.R`. Most are 'nice to have' and not reported. Input `fitforplot16mpn`, output figures saved in folder `Other`, `Plots`. _TO DO_: 1) add code to save plots as .png too for wider comms.
- `11reportFigs2.R` individual plots using customised calls, for reporting.
- `12fitByppt.R` how many ppts are best fit by each model. Input `Data.Rdata` and `fitforplot16mpn`. Output: table of results in the console

4. Free-standing analyses for reporting

- `coverTest.Rmd` - Freestanding analysis to check if cover story affects answers (it doesn't - except lightly in 2/36 conditions, due to noise)
- `demogs.R` - demographics on participant behavioural data
- `abnormalInflation.Rmd` - check for presence of abnormal inflation and deflation (documented behavioural phenomena of causal selection) are found in our data, which would be support for causal selection theory. [Preliminary analysis finds no evidence, unlike in the cogsci paper]. Input `Data.Rdata`, output: table of results in the console.
- `chisq.r` - check whether participants answer non-uniformly in each world. Input `modelAndDataUnfitpn.csv`, output: table of results in the console.



### FOLDER Experiment

Holds the Javascript and html to run the behavioural experiment, which is an online interface with a task like a game. Participants from Prolific were paid to complete the task in early July 2024.

### FOLDER Data, pilot_data

Participant data from the behavioural experiment.

## Glossary

Some everyday words have a special sense in this project.

- `world` - a setting of observed node variables A and B, and outcome E set by deterministic structural equations. A single iteration of how things started off and turned out. Each node can take 0 or 1. Sometimes represented by the values of A,B,E in order, eg. 110 means A=1, B=1, E=0, ie. that A and B both happened and the effect didn't.
- #TO DO

## What is a Collider and why is it important?

# TO DO CHANGE

A collider is a causal structure where multiple possible causes can cause an effect, E. It can be either conjunctive (A _and_ B needed for E) or disjunctive (A _or_ B needed for E). It seems that when several things can potentially cause an outcome, and people want to decide what specifically caused it _this time_, they pick the thing that reliably occurs at the same time as the effect.

But the world is messy and things don't stay the same for long enough to really count everything up. We often have to explain events without having had the luxury of running a proper randomised controlled experiment. Somehow we all do manage to do this, and well enough not to notice how remarkable it is! One theory of how we do this is that our minds simulate `counterfactuals`, (aka other ways things could have turned out), under the hood, under our conscious awareness. Under this theory, we imagine other similar situations, and we decide something is a cause when it robustly correlates with the effect even as everything around shifts and changes. This is elegantly modelled in Quillien's `counterfactual effect size model`, which went a step further than the other existing accounts like Lewis `direct dependency` (where literally everything else has to stay the same to calculate a cause's strength, ie. it only goes 'one layer deep') and Icard/Morris necessity-sufficiency model. Quillien's model 'jiggles', 'flips' or 'resamples' all the variables just like things vary in real life, and then calculates how invariant the cause is across these counterfactuals.

However elegant this model is, it can't be the whole story. The causal score it allocates is only based on base rate or probability, whereas people are more sensitive to the structure they see in the world. It seems we naturally gravitate to causes that are more informative. THat's what this project is about, to model how we do that.
