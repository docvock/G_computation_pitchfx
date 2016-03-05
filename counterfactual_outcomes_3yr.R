###################################
# File: ./model_outcomes.R
#   Purpose: Function which gives counterfactual outcomes
#   Author: David M. Vock
#   Last Modified: March 5, 2016
#   Relies On: ./data_generate.R ./data_manipulate.R ./pitchfx_models_function.R 
#              ./gcomputation.R
#   Files Created: NA 
####################################

# Read-in workpace with model results
load("models_3yr.Rdata")

# libraries used
library(mgcv)
library(xtable)
library(dplyr)

# Read-in data
pitchfx <- read.table("data/pitchfx_processed_3yr_nomiss.txt",sep="\t",header=T)
pitchfx <- as.data.frame(pitchfx)

# Read-in Model, G-computation, Boostrap Function
source("functions/gcomputation_3yr_count.R")
source("functions/pitchfx_models_3yr_function.R")
source("functions/bootstrap.R")

OUT.ITER <- NULL
for (j in c("Castro", "McCutchen")) {
	for (k in c("Castro", "McCutchen")) {
		for (l in c("Castro", "McCutchen")) {
			for (m in c("Castro", "McCutchen")) {
				OUT.ITER <- cbind(OUT.ITER, 
					unlist(model_outcomes(hitting_ability = j, plate_discipline = k, pitch_selection = l,
					strikezone = j, mccutchen_models = mccutchen_models, castro_models = castro_models, 
					pitchfx = pitchfx)))
}}}}


start <- proc.time() 
boot.data <- bootstrap.se(pitchfx = pitchfx, B = 2, seed = 1101985)
proc.time()-start

effects <- expand.grid(strikezone = c("Castro", "McCutchen"), 
                       pitch_selection = c("Castro", "McCutchen"),
                       plate_discipline = c("Castro", "McCutchen"), 
                       hitting_ability = c("Castro", "McCutchen"))

BA_results <- as.data.frame(cbind(effects, round(OUT.ITER[4, ], digits = 3), 
                                  round(apply(boot.data[4, , ], 1, sd), digits = 3),
                                  round(OUT.ITER[4, ] - OUT.ITER[4, 1], digits = 3),
                                  round(apply(t(boot.data[4, , ]) - boot.data[4, 1, ], 2, sd), digits = 3),		
                                  round(OUT.ITER[4, ] - OUT.ITER[4, 16], digits = 3),
                                  round(apply(t(boot.data[4, , ]) - boot.data[4, 16, ], 2, sd), digits = 3)))
colnames(BA_results) <- c("Strikezone","Pitch Selection", "Plate Discipline", "Hitting Ability",
                          "Estimated Batting Average", "SE Batting Average", 
                          "Difference from All Castro", "SE Difference",
                          "Difference from All McCutchen", "SE Difference") 
print(xtable(BA_results, digits = c(rep(0, 5), rep(3, 6))), include.rownames=F)

OBP_results <- as.data.frame(cbind(effects, round(OUT.ITER[5, ], digits = 3), 
                                   round(apply(boot.data[5, , ], 1, sd), digits = 3),	
                                   round(OUT.ITER[5, ] - OUT.ITER[5, 1], digits = 3),
                                   round(apply(t(boot.data[5, , ]) - boot.data[5, 1, ], 2, sd), digits = 3),			
                                   round(OUT.ITER[5, ] - OUT.ITER[5, 16], digits = 3),
                                   round(apply(t(boot.data[5, , ]) - boot.data[5, 16, ], 2, sd), digits = 3)))
colnames(OBP_results) <- c("Strikezone","Pitch Selection", "Plate Discipline", "Hitting Ability",                   
                           "Estimated On Base Percentage", "SE On Base Percentage", 
                           "Difference from All Castro", "SE Difference",
                           "Difference from All McCutchen", "SE Difference") 
print(xtable(OBP_results, digits = c(rep(0, 5), rep(3, 6))), include.rownames = F)

SLG_results <- as.data.frame(cbind(effects, round(OUT.ITER[6, ], digits = 3), 
                                   round(apply(boot.data[6, , ], 1, sd), digits = 3),	
                                   round(OUT.ITER[6, ] - OUT.ITER[6, 1], digits = 3),
                                   round(apply(t(boot.data[6, , ]) - boot.data[6, 1, ], 2, sd), digits = 3),			
                                   round(OUT.ITER[6, ] - OUT.ITER[6, 16], digits = 3),
                                   round(apply(t(boot.data[6, , ]) - boot.data[6, 16, ], 2, sd), digits = 3)))
colnames(SLG_results) <- c("Strikezone","Pitch Selection", "Plate Discipline", "Hitting Ability",                   
                           "Estimated Slugging Percentage", "SE Slugging Percentage",
                           "Difference from All Castro", "SE Difference", 
                           "Difference from All McCutchen", "SE Difference") 
print(xtable(SLG_results, digits = c(rep(0, 5), rep(3, 6))), include.rownames = F)

