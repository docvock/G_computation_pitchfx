###################################
# File: ./pitchfx_models.R
#   Purpose: Develop models for whether or not batter swings, outcome if batter swings, and
#		         whether or not pitch is called strike if the batter does not swing 
#   Author: David M. Vock
#   Last Modified: Augst 1, 2014
#   Relies On: ./data_generate.R ./data_manipulate.R ./pitchfx_models_function.R 
#              ./pitchfx_models_graphics.R
#   Files Created: ./models.Rdata - contains all the models fit
#                  Graphical results of GAMs
#                    ./baseball_graphics/swing_gam_Castro.pdf
#                    ./baseball_graphics/whiff_gam_Castro.pdf
#                    ./baseball_graphics/foul_gam_Castro.pdf
#                    ./baseball_graphics/hit_gam_Castro.pdf
#                    ./baseball_graphics/total_bases_gam_Castro.pdf
#                    ./baseball_graphics/called_strike_gam_Castro.pdf
#                    ./baseball_graphics/swing_gam_McCutchen.pdf
#                    ./baseball_graphics/whiff_gam_McCutchen.pdf
#                    ./baseball_graphics/foul_gam_McCutchen.pdf
#                    ./baseball_graphics/hit_gam_McCutchen.pdf
#                    ./baseball_graphics/total_bases_gam_McCutchen.pdf
#                    ./baseball_graphics/called_strike_gam_McCutchen.pdf
#                  Plots of the marginal probability of various outcomes of a pitch
#                    ./baseball_graphics/prob_whiff_Castro.pdf
#                    ./baseball_graphics/prob_foul_Castro.pdf
#                    ./baseball_graphics/prob_hit_Castro.pdf
#                    ./baseball_graphics/prob_called_strike_Castro.pdf
#                    ./baseball_graphics/prob_ball_Castro.pdf
#                    ./baseball_graphics/expect_total_bases_Castro.pdf
#                    ./baseball_graphics/prob_whiff_McCutchen.pdf
#                    ./baseball_graphics/prob_foul_McCutchen.pdf
#                    ./baseball_graphics/prob_hit_McCutchen.pdf
#                    ./baseball_graphics/prob_called_strike_McCutchen.pdf
#                    ./baseball_graphics/prob_ball_McCutchen.pdf
#                    ./baseball_graphics/expect_total_bases_McCutchen.pdf
#                  Plots of the probability of various outcomes of a pitch given batter swings
#                    ./baseball_graphics/prob_whiff_given_swing_Castro.pdf
#                    ./baseball_graphics/prob_foul_given_swing_Castro.pdf
#                    ./baseball_graphics/prob_hit_given_swing_Castro.pdf
#                    ./baseball_graphics/expect_total_bases_given_swing_Castro.pdf
#                    ./baseball_graphics/prob_whiff_given_swing_McCutchen.pdf
#                    ./baseball_graphics/prob_foul_given_swing_McCutchen.pdf
#                    ./baseball_graphics/prob_hit_given_swing_McCutchen.pdf
#                    ./baseball_graphics/expect_total_bases_given_swing_McCutchen.pdf
#                  Plots of the probability of various outcomes of a pitch given batter does NOT
#                  swing
#                    ./baseball_graphics/prob_called_strike_given_noswing_Castro.pdf
#                    ./baseball_graphics/prob_ball_given_noswing_Castro.pdf
#                    ./baseball_graphics/prob_called_strike_given_noswing_McCutchen.pdf
#                    ./baseball_graphics/prob_ball_given_noswing_McCutchen.pdf
####################################

# Libraries used
library(mgcv)
library(xtable)
library(dplyr)

# Read-in data
pitchfx <- read.table("data/pitchfx_processed.txt",sep="\t",header=T)
pitchfx <- as.data.frame(pitchfx)

# Read-in GAM function
source("functions/pitchfx_models_function.R")
source("functions/pitchfx_models_graphics.R")

# Fit GAM models 
castro_models <- gam_models(pitchfx = pitchfx, batter_id = 516770)
mccutchen_models <- gam_models(pitchfx = pitchfx, batter_id = 457705)

# save the models to the workspace
rm(pitchfx, gam_models, model_diagnostic_graphics)
save.image(file="models.Rdata")

rm(list = ls())




