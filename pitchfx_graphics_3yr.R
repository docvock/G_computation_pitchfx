###################################
# File: ./pitchfx_graphics.R
#   Purpose: File which generates all the graphics needed for manuscript. Outputs to the directory
#							./baseball_graphics_3yr
#   Author: David M. Vock
#   Last Modified: Augst 25, 2015
#   Relies On: ./data_generate_3yr.R ./data_manipulate_3yr.R ./pitchfx_models_3yr.R ./pitchfx_models_function_3yr.R
#   Files Created: many graphics files
####################################


# Read-in workpace with model results
load("models_3yr.Rdata")

# Libraries used
library(dplyr)
library(lattice)
library(spatstat)
library(RColorBrewer)
library(mgcv)

# Define color palette used throughout
color_use <- brewer.pal(10, "RdBu")[10:1]

# Define edge of plate and top and bottom of strike zone for graphics
# (based on Starlin Castro)
topKzone <- 3.47
botKzone <- 1.6
inKzone <- -.71
outKzone <- 0.71

# Read-in data
pitchfx <- read.table("data/pitchfx_processed_3yr.txt",sep="\t",header=T)
pitchfx <- as.data.frame(pitchfx)

# Create data set with only pitches swung at
pitchfx_swing <- filter(pitchfx, swing == 1)

# Functions used

# Function to plot spatial density of pitches.
spat.density <- function(dataset, crit.sc, crit.am, main_title) {
	# Get spatial density of Starlin Castro
  sp.sc <- ppp(dataset$px[crit.sc], dataset$pz[crit.sc], c(-2, 2), c(0.5, 4.5))
  dens.sc <- density(sp.sc)
  grid_dens.sc <- expand.grid(dens.sc$yrow, dens.sc$xcol)
  sum.sc <- sum(c(dens.sc$v) * dens.sc$xstep * dens.sc$ystep)
  grid_dens.sc <- cbind(grid_dens.sc, c(dens.sc$v) / sum.sc, "Castro")
  colnames(grid_dens.sc) <- c("pz", "px", "dens", "player")
  grid_dens.sc <- as.data.frame(grid_dens.sc)
  # Get spatial density of Andrew McCutchen
  sp.am <- ppp(dataset$px[crit.am], dataset$pz[crit.am], c(-2, 2), c(0.5, 4.5))
  dens.am <- density(sp.am)
  grid_dens.am <- expand.grid(dens.am$yrow, dens.am$xcol)
  sum.am <- sum(c(dens.am$v) * dens.am$xstep * dens.am$ystep)
  grid_dens.am <- cbind(grid_dens.am, c(dens.am$v) / sum.am ,"McCutchen")
  colnames(grid_dens.am) <- c("pz", "px", "dens", "player")
  grid_dens.am <- as.data.frame(grid_dens.am)
  grid_dens <- rbind(grid_dens.am, grid_dens.sc)
  # Plot densities  
  contourplot(dens ~ px * pz|player, data = grid_dens,
              aspect="iso",
              at=seq(from = 0, to = 0.27, by = 0.03), region = T, col.regions = c(color_use),
              xlab = "Distance from Center of Home Plate (ft)",
              ylab = "Distance from Ground (ft)",
              main = paste("Density of ", main_title, " by Player", sep=""),
              panel = function(...){
                panel.contourplot(...)
                panel.rect(inKzone, botKzone, outKzone, topKzone,
                           border = "green", lty = 1,lwd=3)} )
}


# Function to plot unadjusted probability of swinging by location and by pitch type.

swing_pred_plot <- function(pt, pt_name){
  if (pt == "all") {
    test_gam <- gam(swing~s(px, pz, bs = "ts", by = batter_name), family = "binomial", 
                    link = "logit", data = pitchfx) 
  } else {
    test_gam <- gam(swing~s(px, pz, bs = "ts", by = batter_name), family = "binomial",
                    link="logit", data=pitchfx[pitchfx$pitch_type_alt==pt,])
  }
  
  grid_pred <- expand.grid(seq(from = -2, to = 2, by = 0.05), seq(from = 0.5, to = 4.5, by = 0.05),
                           c("Andrew McCutchen", "Starlin Castro"))
  colnames(grid_pred) <- c("px", "pz", "batter_name")
  grid_pred <- as.data.frame(grid_pred)
  grid_pred$batter_name <- as.factor(grid_pred$batter_name)
  pred_test_gam <- predict(test_gam, newdata = grid_pred, type = "response")
  grid_pred <- cbind(grid_pred, pred_test_gam)
  
  contourplot(pred_test_gam ~ px * pz | batter_name, data = grid_pred,
              aspect="iso",
              at=seq(from = 0,to = 1,by = 0.1), region = T, col.regions = color_use,
              xlab = "Distance from Center of Home Plate (ft)",
              ylab = "Distance from Ground (ft)",
              main = paste("Probability of Swinging at",pt_name,"by Player",sep=" "),
              panel = function(...){
                panel.contourplot(...)
                panel.rect(inKzone, botKzone, outKzone, topKzone,
                           border = "green", lty = 1, lwd = 3)} )
}


# Function to plot adjusted probability of swinging by pitch type, count, and location.
# Prediction uses average vertical and horizontal break of the indicated pitch type

swing_adj_pred_plot <- function(pt, pt_name, count){
  grid_pred <- expand.grid(seq(from = -2, to = 2, by = 0.05), seq(from = 0.5, to = 4.5, by = 0.05),
  	                       count)
  colnames(grid_pred) <- c("px", "pz", "count")
  
  grid_pred <- as.data.frame(grid_pred)
  grid_pred$count <- as.factor(grid_pred$count)
  
  # Use average for specified pitch type
  grid_pred$pfx_x <- tapply(pitchfx$pfx_x, pitchfx$pitch_type_alt, mean, na.rm=T)[pt]
  grid_pred$pfx_z <- tapply(pitchfx$pfx_z, pitchfx$pitch_type_alt, mean, na.rm=T)[pt]
  grid_pred$start_speed <- tapply(pitchfx$start_speed, pitchfx$pitch_type_alt, mean, na.rm=T)[pt]
	
  pred_test_M <- predict(mccutchen_models$swing, newdata=grid_pred, type="response")
  pred_test_S <- predict(castro_models$swing, newdata=grid_pred, type="response")
  
  pred_test_M <- cbind(grid_pred, probswing = pred_test_M, hitter="Andrew McCutchen")              
  pred_test_S <- cbind(grid_pred, probswing =pred_test_S, hitter="Starlin Castro")
  
  grid_pred <- as.data.frame(rbind(pred_test_M, pred_test_S))
  grid_pred$hitter <- as.factor(grid_pred$hitter)
  
  contourplot(probswing ~ px * pz | hitter, data = grid_pred,
              aspect="iso",
              at=seq(from = 0, to = 1, by = 0.1), region = T, col.regions = color_use,
              xlab = "Distance from Center of Home Plate (ft)",
              ylab = "Distance from Ground (ft)",
              main = paste("Probability of Swinging at", pt_name, "with", count, 
                            "Count by Player", sep=" "),
              panel = function(...){
                panel.contourplot(...)
                panel.rect(inKzone, botKzone, outKzone, topKzone,
                           border = "green", lty = 1, lwd = 3)} )
}


# Graphics created for anlysis

# density plots for pitches swung at
pdf("baseball_graphics_3yr\\denisty_pitches_swing.pdf", height = 6, width = 12)
spat.density(dataset = pitchfx_swing,
	crit.sc = which(is.na(pitchfx_swing$px) == F & pitchfx_swing$starlin == 1),
	crit.am = which(is.na(pitchfx_swing$px) == F & pitchfx_swing$starlin == 0),
	main_title="All Pitches Swung At")
dev.off()

pdf("baseball_graphics_3yr\\denisty_sliders_swing.pdf", height = 6, width = 12)
spat.density(dataset = pitchfx_swing,
	crit.sc = which(is.na(pitchfx_swing$px) == F & pitchfx_swing$starlin == 1 & 
							pitchfx_swing$pitch_type_alt == "SL"),
	crit.am = which(is.na(pitchfx_swing$px) == F & pitchfx_swing$starlin == 0 & 
							pitchfx_swing$pitch_type_alt=="SL"),
main_title="Sliders Swung At")
dev.off()


# density plots for all pitches
pdf("baseball_graphics_3yr\\denisty_pitches.pdf", height = 6, width = 12)
spat.density(dataset=pitchfx,
	crit.sc = which(is.na(pitchfx$px) == F & pitchfx$starlin == 1),
	crit.am = which(is.na(pitchfx$px) == F & pitchfx$starlin == 0),
	main_title="All Pitches")
dev.off()

pdf("baseball_graphics_3yr\\denisty_sliders.pdf", height = 6, width = 12)
spat.density(dataset=pitchfx, 
	crit.sc = which(is.na(pitchfx$px) == F & pitchfx$starlin == 1 & pitchfx$pitch_type_alt == "SL"),
	crit.am = which(is.na(pitchfx$px) == F & pitchfx$starlin == 0 & pitchfx$pitch_type_alt == "SL"),
	main_title="All Sliders")
dev.off()


# Graphically demonstrate plate discipline between players 


# unadjusted analysis
pdf("baseball_graphics_3yr\\swing_prob_unadjusted.pdf", height = 6, width = 12)
swing_pred_plot(pt = "all", pt_name = "All Pitches")
dev.off()

pdf("baseball_graphics_3yr\\swing_prob_SL_unadjusted.pdf", height = 6, width = 12)
swing_pred_plot(pt = "SL", pt_name = "Sliders")
dev.off()

pdf("baseball_graphics_3yr\\swing_prob_FF_unadjusted.pdf", height = 6, width = 12)
swing_pred_plot(pt = "FF", pt_name = "Four-Seam Fastball")
dev.off()

pdf("baseball_graphics_3yr\\swing_prob_FT_unadjusted.pdf", height = 6, width = 12)
swing_pred_plot(pt = "FT", pt_name = "Two-Seam Fastball")
dev.off()


# model based analysis
pdf("baseball_graphics_3yr\\swing_prob_SL_count32_adjusted.pdf", height = 6, width = 12)
swing_adj_pred_plot(pt = "SL", pt_name = "Sliders", count = "3-2")
dev.off()

pdf("baseball_graphics_3yr\\swing_prob_FF_count32_adjusted.pdf", height = 6,width = 12)
swing_adj_pred_plot(pt = "FF", pt_name = "Four-Seam Fastball", count = "3-2")
dev.off()

pdf("baseball_graphics_3yr\\swing_prob_FT_count32_adjusted.pdf", height = 6, width = 12)
swing_adj_pred_plot(pt = "FT", pt_name = "Two-Seam Fastball", count = "3-2")
dev.off()

rm(list=ls())
