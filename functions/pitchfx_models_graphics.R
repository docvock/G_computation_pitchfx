###################################
# File: ./pitchfx_model_graphics.R
#   Purpose: File which generates function to produce graphics to describe generalized additve 
#            models. Called in pitchfx_models_function. Outputs to the directory 
#            ./baseball_graphics
#   Author: David M. Vock
#   Last Modified: Augst 10, 2014
#   Relies On: NA
#   Files Created: NA
####################################


# Libraries used
library(dplyr)
library(lattice)
library(RColorBrewer)

# Define color palette used throughout
color_use <- brewer.pal(10, "RdBu")[10:1]

# Define edge of plate and top and bottom of strike zone for graphics
# (based on Starlin Castro)
topKzone <- 3.47
botKzone <- 1.6
inKzone <- -.71
outKzone <- 0.71

# Get average speed and movement of pitches by pitch type
avg_pfx_x <- tapply(pitchfx$pfx_x, pitchfx$pitch_type_alt1, mean, na.rm=T)
avg_pfx_z <- tapply(pitchfx$pfx_z, pitchfx$pitch_type_alt1, mean, na.rm=T)
avg_start_speed <- tapply(pitchfx$start_speed, pitchfx$pitch_type_alt1, mean, na.rm=T)
pitch_type_avg <- data.frame(avg_pfx_x, avg_pfx_z, avg_start_speed,names(avg_pfx_x))
colnames(pitch_type_avg) <- c("pfx_x","pfx_z","start_speed","pitch_type_alt1")

model_diagnostic_graphics <- function(data, outcome, model, count, main_title, plot_points = "Y",
	surv_prob = 1, complement = "N") {
	# Function which produces plots of the estimated probability of event across the strike zone 
	# at the average movement and speed for the six major pitch types. 
	#
	# Args:
	# data: Data from which the spatial location of observations used to estimate the model
	#       should be plotted if plot_points = "Y" or "S".
	# outcome: Vector of binary outcome variables that correspond to the spatial points in data.
	# model: Model output which will be used to generate estimated probabilities. 
	# count: Count at which predictions should be made.
	# main_title: Main title of plot.
	# plot_points: If "Y", then the spatial location of all observations used to estimate the 
	#              probability should be plotted. If "S", only then the spatial location of 
	#              the successes should be plotted. If "N" no points should be plotted. 
	# surv_prob: Vector that the estimated probability will be multiplied by. Useful if we are estimating a 
	#            conditional model (i.e., probability of getting hit, given ball put in play) but want an
	#            estimate of the marginal probability.
	# complement: If "Y", then plot 1 - estimated probability. 
	
	grid_pred <- expand.grid(seq(from = -2, to = 2, by = 0.05), seq(from = 0.5, to = 4.5, by = 0.05),
		c("CH", "SL", "CU", "FF", "FT", "FC"))
	colnames(grid_pred) <- c("px", "pz", "pitch_type_alt1")
	grid_pred <- as.data.frame(grid_pred)
	grid_pred <- inner_join(grid_pred,pitch_type_avg)
	grid_pred$count <- grid_pred$count_alt <-count
	grid_pred$strikecount <- as.numeric(substr(grid_pred$count,3,3))
	grid_pred$ballcount = as.numeric(substr(grid_pred$count,1,1))
	grid_pred$pred_outcome <- predict(model, newdata=grid_pred, type="response")
	if( complement == "Y") {
		grid_pred$pred_outcome <- 1 - grid_pred$pred_outcome 	
	}
	grid_pred$pred_outcome <- grid_pred$pred_outcome * surv_prob
	 
	
myplot <-	contourplot(pred_outcome ~ px * pz | pitch_type_alt1 , data = grid_pred,
              aspect="iso",
              at=seq(from = 0, to = 1, by = 0.1), region = T, col.regions = color_use,
              xlab = "Distance from Center of Home Plate (ft)",
              ylab = "Distance from Ground (ft)",
              main = main_title,
              panel = function(x, y, z, subscripts, ...){
                panel.contourplot(x, y, z, subscripts, ...)
              	t1 <- unique(grid_pred$pitch_type_alt1[subscripts])
                panel.rect(inKzone, botKzone, outKzone, topKzone,
                           border = "green", lty = 1, lwd = 3)
              	if(plot_points == "Y") {
              	panel.points(data$px[data$pitch_type_alt1==t1], data$pz[data$pitch_type_alt1==t1], 
              		col = ifelse(outcome[data$pitch_type_alt1==t1]==1, "yellow", "purple"), pch = 19)
              	}              	
              	if(plot_points == "S") {
              	panel.points(data$px[data$pitch_type_alt1 == t1 & outcome == 1], 
              		data$pz[data$pitch_type_alt1 == t1 & outcome == 1], col = c("yellow"), pch = 19)
              	} #close if statement 
              } # close panel function
	) # close contourplot function

	return( list(grid_pred$pred_outcome,myplot))
} # close graphics function


