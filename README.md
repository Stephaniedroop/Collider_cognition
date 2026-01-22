# Scripts and data for a behavioural experiment and computational models to study how human cognition uses both causal selection and causal inference to generate explanations for observed phenomena

## Authors

- Stephanie Droop (stephanie.droop@ed.ac.uk)
- Neil Bramley
- Tadeg Quillien
- (some consultation from Christopher Lucas)

## Languages

All scripts are in R, v.4.1. Packages needed are below with citations. 

## How to run

- or **analysis** go to the `Scripts` folder and run `master.R`, or if you know what script you are looking for you can go straight there and run that only. See list of scripts below.
- This structure assumed you have a renv and .rproj doc with here set to the .rproj.
- To see and try the behavioural experiment go to https://eco.ppls.ed.ac.uk/~s0342840/collidern/collidertop.html. Code for the task interface and structure of running it in JavaScript in in the folder `Other/hostExperiment`. To click through the experiment: at the comprehension check enter Yes, No, True, 12.

## Files, folders, model

### FOLDER Scripts

- `master.R` - top level analysis script. **Go here first**


library(tidyverse)
#library(rjson)
library(ggnewscale)
library(here)
library(RColorBrewer)
library(ggplot2)
library(lme4)
library(lmerTest)

set.seed(12)

# ------------- 0. Source utils -------------
source(here('Scripts', 'cesmUtils.R')) # Functions for running the cesm model - used in 03
source(here('Scripts', 'optimUtils4par.R')) # Functions for modelling likelihood calculation - used in 06
source(here('Scripts', 'modelNames.R')) # Static lists with model characteristics - used in 05
source(here('Scripts', 'plotUtils.R')) # Functions for plotting to compare model and ppts used in 10, 11


#--------------- 1. Get ppt data from behavioural experiment  -------------------
\\ (JavaScript for the experiment itself is in the folder Experiment.
\\ For each participant a csv was saved on the server and then transferred out of there into Data)
\\ Demographics are in the `preprocessing` script too.
source(here('Scripts', '01preprocess.R')) # Collates individual csvs, reconciles with prolific report, saves `Data.Rdata` and also `ppts.csv`

#-------------- 2. Create parameters, run cesm, get model predictions and save them ------------
source(here('Scripts', '02setParams.R')) # The baserates of the causal model
source(here('Scripts', '03getPreds.R')) # Gets cesm model predictions using `cesmUtils.R`.

# Process model predictions to be more user friendly: take average of 10 model runs, wrangles and renames variables, splits out node values 0 and 1
source(here('Scripts', '04processPreds.R')) # Also sets a column of 1s and tags like Actual for the lesions

# -------------3. Results: fit model, compare predictions, plot etc -----------------

source(here('Scripts', '05optimise.R')) # Uses `optimUtils4par.R` to fit models.
source(here('Scripts', '06processForPlot.R')) # Make model predictions use friendly for plotting

source(here('Scripts', '07reportFigs.R')) # Main plots on every trial at once. Uses `plotUtils` functions to compare models
source(here('Scripts', '08reportFigs2.R')) # Other plots not using functions; aggregate and split plots

source(here('Scripts', '09fitByppt.R')) # Best model fit by participant. Input: data.rda
source(here('Scripts', '10presentByppt.R')) # chisq tests and component bar plot


# ------------- 4. Standalone analyses ------------
source(here("Scripts", "Standalone", "demogs.R"))
source(here("Scripts", "Standalone", "chisq.R")) # Uses the `unfitmodelpredictions` data but only to get grouped ppt numbers. Doesn't vary with different models
source(here("Scripts", "Standalone", "abnormalInflation.R")) # Tests for overall presence of the effect seen in the plot of the 111 conditions by pgroup and structure - doesn't find any because it's only seen in pgroup 1

# Check if cover story affects answers (it doesn't - except in 2/36 conditions due to noise)
rmarkdown::render(
  input = here("Scripts", "Standalone", "coverTest.Rmd"),
  output_file = here("Other", "Reports", "coverTest.html")
)



### FOLDER Experiment

Holds the Javascript and html to run the behavioural experiment, which is an online interface with a task like a game. Participants from Prolific were paid to complete the task in early July 2024.

### FOLDER Data, pilot_data

Participant data from the behavioural experiment.



## Package citations

*ellmer* **light use!**
Wickham H, Cheng J, Jacobs A, Aden-Buie G, Schloerke B (2025). _ellmer: Chat with Large Language
  Models_. R package version 0.3.2, <https://CRAN.R-project.org/package=ellmer>.

*gander* **light use!**
Couch S (2025). _gander: High Performance, Low Friction Large Language Model Chat_. R package version
  0.1.0, <https://CRAN.R-project.org/package=gander>.

*ggnewscale*
Campitelli E (2024). _ggnewscale: Multiple Fill and Colour Scales in 'ggplot2'_. R package version
  0.5.0, <https://CRAN.R-project.org/package=ggnewscale>.

*ggplot2*
H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.

*here*
Müller K (2025). _here: A Simpler Way to Find Your Files_. R package version 1.0.2,
  <https://CRAN.R-project.org/package=here>.

*knitr*
Xie Y (2023). _knitr: A General-Purpose Package for Dynamic Report Generation in R_. R package version
  1.45, <https://yihui.org/knitr/>.

*lme4*
Douglas Bates, Martin Maechler, Ben Bolker, Steve Walker (2015). Fitting Linear Mixed-Effects Models
  Using lme4. Journal of Statistical Software, 67(1), 1-48. doi:10.18637/jss.v067.i01.
  
*lmerTest*
Kuznetsova A, Brockhoff PB, Christensen RHB (2017). “lmerTest Package: Tests in Linear Mixed Effects
  Models.” _Journal of Statistical Software_, *82*(13), 1-26. doi:10.18637/jss.v082.i13
  <https://doi.org/10.18637/jss.v082.i13>.
  
*RColorBrewer*
Neuwirth E (2022). _RColorBrewer: ColorBrewer Palettes_. R package version 1.1-3,
  <https://CRAN.R-project.org/package=RColorBrewer>.
  
*renv*
Ushey K, Wickham H (2025). _renv: Project Environments_. R package version 1.1.5,
  <https://CRAN.R-project.org/package=renv>.
  
*rjson*
Couture-Beil A (2024). _rjson: JSON for R_. R package version 0.2.23,
  <https://CRAN.R-project.org/package=rjson>.
  
*rmarkdown*
Allaire J, Xie Y, Dervieux C, McPherson J, Luraschi J, Ushey K, Atkins A, Wickham H, Cheng J, Chang W,
  Iannone R (2024). _rmarkdown: Dynamic Documents for R_. R package version 2.29,
  <https://github.com/rstudio/rmarkdown>.
  
*stringr*
Wickham H (2023). _stringr: Simple, Consistent Wrappers for Common String Operations_. R package version
  1.5.1, <https://CRAN.R-project.org/package=stringr>.
  
*tidyverse*
Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester J,
  Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi K,
  Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to the tidyverse.” _Journal of Open Source
  Software_, *4*(43), 1686. doi:10.21105/joss.01686 <https://doi.org/10.21105/joss.01686>.
  
*xtable*
Dahl D, Scott D, Roosen C, Magnusson A, Swinton J (2019). _xtable: Export Tables to LaTeX or HTML_. R
  package version 1.8-4, <https://CRAN.R-project.org/package=xtable>.
  