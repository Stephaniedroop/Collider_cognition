# Inference and selection in causal explanation
## Scripts and data for behavioral experiment and computational models
### Authors

- Stephanie Droop (stephanie.droop@ed.ac.uk)
- Neil Bramley
- Tadeg Quillien
- (some consultation from Christopher Lucas)

### Languages

All scripts are in R, v.4.1. Packages needed are below with citations. 

### How to run

- For all **analysis** in order, go to the `Scripts` folder and run `master.R`, or see individual scripts.
- This structure assumed you have a renv and .rproj doc with here set to the .rproj. Packages listed below.
- To see and do the behavioral experiment go to https://eco.ppls.ed.ac.uk/~s0342840/collidern/collidertop.html. Code for the task interface and structure of running it in JavaScript in in the folder `Other/hostExperiment`. To click through the experiment: at the comprehension check enter Yes, No, True, 12.


### Summary of folders and contents

#### FOLDER Scripts

- `master.R` - top level analysis script. **Go here first**.  

Other numbered scripts are grouped under the following headings:  

-- 0. Sources utils.  

-- 1. Gets ppt data from behavioral experiment.  

-- 2. Creates parameters, runs cesm, gets model predictions.  

-- 3. Fits models, compares predictions, plots etc.  

-- 4. Standalone analyses.  


#### FOLDER Other

/hostExperiment Holds the Javascript and html to run the behavioral experiment, which is an online interface with a task like a game. Participants from Prolific were paid to complete the task in early July 2024.
/plots Plots for reporting
/Reports Reports for internal use, including cover stories and model comparison

#### FOLDER Data. 

/new Participant data from the behavioral experiment.  

/pilot_data Same but early run of same design.  

/modelData Cover stories for experiment, also in paper appendix.


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
  