# [Estimating the Effect of Plate Discipline Using a Causal Inference Framework: An Application of the G-Computation Algorithm](https://sites.google.com/site/dmvock/)

### [David M. Vock](https://sites.google.com/site/dmvock/)

### [University of Minnesota Division of Biostatistics](http://sph.umn.edu/biostatistics/)


---

The files in this repository comprise the source code for reproducing the work in the manuscript **Estimating the Effect of Plate Discipline Using a Causal Inference Framework: An Application of the G-Computation Formula**.

### File Organization

The code to obtain Gameday data, including pitchf/x data, for Starlin Castro and Andrew McCutchen from the XML files available from Major League Baseball's Gameday website is contained in `data_generate_3yr.R`. This code produces the raw dataset `data/pitchfx_sc_am_3yr.txt` which is used throughout the project and is described in detail below. The file `data_manipulate_3yr.R` takes the raw data in `pitchfx_sc_am_3yr.txt` and generates several derived variables which are used throughout the analysis and generates the processed dataset `data/pitchfx_processed_3yr.txt`. The code to estimate the generalized additive models (GAMs) used to model the outcomes of various pitches is contained in `pitchfx_models_3yr.R` which call the functions saved in `functions/pitchfx_models_function_3yr.R` and `functions/pitchfx_models_graphics.R`. The R workspace `models_3yr.Rdata` contains the output from the GAMs. Due to the size of the file, this workspace is not saved in this repository.

The estimated batting average, on-base percentage, and slugging percentage under different conterfactuals is produced in `counterfactual_outcomes_3yr.R` which uses the functions saved in `functions/gcomputation_3yr_count.R` and `functions/bootstrap.R` (the functions for the G-computation algorithm and the bootstrap respectively).

Diagnostic graphics and miscellaneous graphics for the manuscript are generated in `pitchfx_graphics.R` and `manuscript_graphics.R`, respectively. Graphics are output in the `/baseball_graphics_3yr/` directory.  
Finally, code to calculate the batting average, on-base percentage, and slugging percentage, among plate appearances included in this analysis is contained in `calibrating_models.R`. 

### Reproduce the Output and Manuscript

The main directory contains a `Makefile` which can be run to obtain all the output for the manuscript and compile the manuscript.  This makefile encodes the dependencies between the different files described above. For those not familiar with makefiles, we have included a "Make-like" file, `master_source.R` which indicates the order in which the .R files should be sourced.

### Analysis Dataset

The analysis dataset `data/pitchfx_sc_am_3yr.txt` contains data on all pitches thrown to Starlin Castro and Andrew McCutchen during the 2012 - 2014 seasons and the outcome of those pitches. All of the data is taken directly from MLB Gameday without any processing. The contents of this file are summarized below. A more detailed explanation of many of these terms is given in Marchi and Albert (2013).  


|varnames        |description                                                                                         |
|:---------------|:---------------------------------------------------------------------------------------------------|
|des             |Description of result of pitch                                                                      |
|id              |Pitch number in game (useful for sorting)                                                           |
|type            |Ball (B), strike (S), or out (X)                                                                    |
|sv_id           |Time of pitch (YYMMDD_HHMMSS)                                                                       |
|start_speed     |Starting speed of pitch (mph)                                                                       |
|end_speed       |Ending speed of pitch (mph)                                                                         |
|sz_top          |Top of strikezone from ground (ft)                                                                  |
|sz_bot          |Bottom of strikezone from ground (ft)                                                               |
|pfx_x           |Horizontal distance when crossing plate from hypothetical pitch with no spin (in)                   |
|pfx_z           |Vertical distance when crossing plate from hypothetical pitch with no spin (in)                     |
|px              |Horizontal location when crossing plate from plate center(ft)                                       |
|pz              |Vertical location when crossing plate from ground (ft)                                              |
|vx0             |Initial velocity in horizontal direction (ft/s)                                                     |
|vy0             |Initial velocity in direction toward plate (ft/s)                                                   |
|vz0             |Initial velcotiy in vertical direction (ft/s)                                                       |
|ax              |Acceleration in horizontal direction (ft/s/s)                                                       |
|ay              |Acceleration in direction toward plate (ft/s/s)                                                     |
|az              |Acceleration in vertical direction (ft/s/s)                                                         |
|break_y         |Distance from plate to point where pitch deviates most from hypothetical pitch with no spin (ft)    |
|break_angle     |Direction of deviation from pitch trajectory to trajectory of hypothetical pitch with no spin (deg) |
|break_length    |Greatest distance from observed trajectory to trajectory of hypothetical pitch with no spin (in)    |
|pitch_type      |Pitch type                                                                                          |
|type_confidence |Confidence of pitch type classification                                                             |
|zone            |Region of strike zone                                                                               |
|nasty           |Nasty factor (scale from 0-100)                                                                     |
|spin_dir        |Spin direction where 0 (180) degrees is perfect top (bottom) spin (deg)                             |
|spin_rate       |Spin rate (rpm)                                                                                     |
|inning_side     |Top or bottom of inning                                                                             |
|inning          |Inning of pitch                                                                                     |
|num             |At-bat number in game (useful for sorting)                                                          |
|gameday_link    |Gameday identification (gives date and teams playing)                                               |
|count           |Count of pitch                                                                                      |
|pitcher         |Pitcher ID number                                                                                   |
|batter          |Batter ID Number                                                                                    |
|start_tfs_zulu  |Time of start of at-bat                                                                             |
|stand           |Batter stance (left or right)                                                                       |
|b_height        |Batter height                                                                                       |
|p_throws        |Pitcher throws (left or right)                                                                      |
|atbat_des       |Lengthier description of at-bat                                                                     |
|event           |End result of at-bat                                                                                |
|batter_name     |Batter name                                                                                         |
|pitcher_name    |Pitcher name                                                                                        |

### Session Info
This manuscript was compiled with [RStudio](http://www.rstudio.com/) (v. 0.98.983 for Windows) with the following R session:


```
## R version 3.2.3 (2015-12-10)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 7 x64 (build 7601) Service Pack 1
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] knitr_1.12   dplyr_0.4.3  xtable_1.8-0 mgcv_1.8-9   nlme_3.1-122
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.2     lattice_0.20-33 assertthat_0.1  grid_3.2.3     
##  [5] R6_2.1.1        DBI_0.3.1       formatR_1.2.1   magrittr_1.5   
##  [9] evaluate_0.8    highr_0.5.1     stringi_1.0-1   Matrix_1.2-3   
## [13] tools_3.2.3     stringr_1.0.0   parallel_3.2.3
```

---


