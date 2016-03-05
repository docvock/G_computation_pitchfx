###################################
# File: ./manuscript_graphics.R
#   Purpose: File which generates function to produce graphics used in the manuscript. 
#            Called in pitchfx_models_function. Outputs to the directory 
#            ./manuscript_graphics
#   Author: Laura Boehm Vock
#   Last Modified: March 5, 2016
#   Relies On: ./data_generate.R ./data_manipulate.R ./pitchfx_models.R ./pitchfx_models_function.R
#   Files Created: many graphics files; specifically the ones used in Vock & Boehm Vock (2016)
####################################


# Libraries used
library(dplyr)
library(lattice)
library(RColorBrewer)

# Load data
pitchfx <- read.table("data/pitchfx_processed_3yr_nomiss.txt",sep="\t",header=T)
pitchfx <- as.data.frame(pitchfx)

# Load models
load("models_3yr.Rdata")

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
pitch_type_avg <- tbl_df(data.frame(avg_pfx_x, avg_pfx_z, avg_start_speed,names(avg_pfx_x)))
colnames(pitch_type_avg) <- c("pfx_x","pfx_z","start_speed","pitch_type_alt1")


modeled_comparison_graphics <- function(adjust=TRUE, data=NULL, outcome=NULL, model=NULL, 
                                        count, pitch_type, main_title, plot_points = "Y",
                                      surv_prob = 1, complement = "N") {
  # Function which produces plots of the estimated probability of event across the strike zone 
  # at the average movement and speed for the six major pitch types. 
  #
  # Args:
  # adjust: Whether to fit the adjusted or unadjusted predicitions. If adjust=TRUE, need model specified.
  # data: Data from which the spatial location of observations used to estimate the model
  #       should be plotted if plot_points = "Y" or "S". Onlly needed if plotting points.
  # outcome: Vector of binary outcome variables that correspond to the spatial points in data. Only needed if plotting points.
  # model: Model which will be used to generate estimated probabilities. This will be something like "swing"
  # count: Count at which predictions should be made.
  # pitch_type: "CH", "SL", "CU", "FF", "FT", or "FC"
  # main_title: Main title of plot.
  # plot_points: If "Y", then the spatial location of all observations used to estimate the 
  #              probability should be plotted. If "S", only then the spatial location of 
  #              the successes should be plotted. If "N" no points should be plotted. 
  # surv_prob: Vector that the estimated probability will be multiplied by. Useful if we are estimating a 
  #            conditional model (i.e., probability of getting hit, given ball put in play) but want an
  #            estimate of the marginal probability.
  # complement: If "Y", then plot 1 - estimated probability. 
  if(!adjust){cat("Current plotting function only allows unadjusted plots of swing outcome \n")}
  if(plot_points %in% c("Y", "S") & is.null(outcome)){cat("Must specify an outcome in order to plot it \n")}
  
  grid_pred <- expand.grid(seq(from = -2, to = 2, by = 0.05), 
                           seq(from = 0.5, to = 4.5, by = 0.05), 
                           pitch_type, 
                           count)
  colnames(grid_pred) <- c("px", "pz", "pitch_type_alt1", "count")
  grid_pred <- merge(grid_pred, 
                     subset(pitch_type_avg, pitch_type_alt1 == pitch_type))

  
  if(adjust){
    #grid_pred$strikecount <- as.numeric(substr(grid_pred$count,3,3))
    #grid_pred$ballcount = as.numeric(substr(grid_pred$count,1,1))
    grid_pred$count <- grid_pred$count_alt <- grid_pred$count_alt2 <- count
    grid_pred$count_alt2[grid_pred$count=="3-0"] <- "2-0"
    
    pred_AM <- predict(mccutchen_models[[model]], newdata=grid_pred, type="response")
    pred_SC <- predict(castro_models[[model]], newdata=grid_pred, type="response")
  }
  
  if(!adjust){
    unadjAM <- gam(swing~s(px, pz, bs="ts"), family="binomial", link="logit",
                   data = subset(data, batter == 457705 & 
                                   as.character(pitch_type_alt1) == pitch_type)
                   )
                   #data=data[data$batter==457705 & as.character(data$pitch_type_alt1)==pitch_type,])
                  #data=data, subset=(batter==457705 & as.character(pitch_type_alt1)==pitch_type))
    pred_AM <- predict(unadjAM, newdata=grid_pred, type="response")
    unadjSC <- gam(swing~s(px, pz, bs="ts"), family="binomial", link="logit",
                   data=data[data$batter==516770 & as.character(data$pitch_type_alt1)==pitch_type,])
                  #    data=data, subset=(batter == 516770 & as.character(pitch_type_alt1)==pitch_type))
    pred_SC <- predict(unadjSC, newdata=grid_pred, type="response")

  }
  
  pred_AM <- data.frame(grid_pred, pred_outcome = pred_AM, batter_name = "Andrew McCutchen")              
  pred_SC <- data.frame(grid_pred, pred_outcome = pred_SC, batter_name = "Starlin Castro")
  
  grid_pred <- bind_rows(pred_AM, pred_SC)
  grid_pred$batter_name <- as.factor(grid_pred$batter_name)
  
  
  if( complement == "Y") {
    grid_pred$pred_outcome <- 1 - grid_pred$pred_outcome 	
  }
  grid_pred$pred_outcome <- grid_pred$pred_outcome * surv_prob
  
  
  myplot <-	contourplot(pred_outcome ~ px * pz | batter_name , data = grid_pred,
                        aspect="iso",
                        at=seq(from = 0, to = 1, by = 0.1), region = T, 
                        col.regions = color_use,
                        xlab = "Distance from Center of Home Plate (ft)",
                        ylab = "Distance from Ground (ft)",
                        main = main_title,
                        panel = function(x, y, z, subscripts, ...){
                          panel.contourplot(x, y, z, subscripts, ...)
                          t1 <- unique(grid_pred$batter_name[subscripts])
                          panel.rect(inKzone, botKzone, outKzone, topKzone,
                                     border = "green", lty = 1, lwd = 3)
                          if(plot_points == "Y") {
                            data_t1 <- data[as.character(data$pitch_type_alt1) == pitch_type & 
                                              data$batter_name == t1, ]
                            
                            outcome_t1 <- outcome[as.character(data$pitch_type_alt1) == pitch_type &
                                                  data$batter_name == t1]
                            
                           panel.points(data_t1$px, data_t1$pz,
                                        col = ifelse(outcome_t1 == 1,
                                                    "yellow", "purple"), 
                                        pch = 19)
                          }              	
                          if(plot_points == "S") {
                            data_t1 <- data[as.character(data$pitch_type_alt1) == pitch_type & 
                                              data$batter_name == t1, ]
                            
                            outcome_t1 <- outcome[as.character(data$pitch_type_alt1) == pitch_type &
                                                    data$batter_name == t1]
                            

                            panel.points(data_t1$px[outcome_t1 == 1], 
                                         data_t1$pz[outcome_t1 == 1], 
                                         col = c("yellow"), pch = 19)
                          } #close if statement 
                        } # close panel function
  ) # close contourplot function
  return( list(predicted=grid_pred$pred_outcome, plot=myplot))
  
#  return(myplot)
} # close graphics function

FF_unadjust <- modeled_comparison_graphics(data=pitchfx, adjust=FALSE, 
                                           outcome=pitchfx$swing, plot_points="N", 
                                           count="", pitch_type="FF", main_title="") 

SL_unadjust <- modeled_comparison_graphics(data=pitchfx, adjust=FALSE, plot_points="N", 
                                           outcome=pitchfx$swing,
                                           count="", pitch_type="SL", main_title="")


#plot(FF_unadjust$plot)
#plot(SL_unadjust$plot)

swing_FF_0_0 <- modeled_comparison_graphics(model="swing", count="0-0", pitch_type="FF", main_title="", 
                                      data=pitchfx, outcome=pitchfx$swing, plot_points = "N")
swing_FF_3_2 <- modeled_comparison_graphics(model="swing", count="3-2", pitch_type="FF", main_title="", 
                                            data=pitchfx, outcome=pitchfx$swing, plot_points = "N")
swing_SL_0_0 <- modeled_comparison_graphics(model="swing", count="0-0", pitch_type="SL", main_title="", 
                                            data=pitchfx, outcome=pitchfx$swing, plot_points = "N")
swing_SL_3_2 <- modeled_comparison_graphics(model="swing", count="3-2", pitch_type="SL", main_title="", 
                                            data=pitchfx, outcome=pitchfx$swing, plot_points = "N")


## Probability of whiff given swing
whiff_FF_0_0 <- modeled_comparison_graphics(model="whiff", count="0-0", pitch_type="FF", 
                                            main_title="",
                                            data=pitchfx, outcome=pitchfx$whiff, plot_points= "S")
whiff_FF_1_1 <- modeled_comparison_graphics(model="whiff", count="1-1", pitch_type="FF", 
                                            main_title="",
                                            data=pitchfx, outcome=pitchfx$whiff, plot_points= "S")
#plot(whiff_FF_0_0$plot)                                            
#plot(whiff_FF_1_1$plot)                                            

whiff_FF_3_2 <- modeled_comparison_graphics(model="whiff", count="3-2", pitch_type="FF", 
                                            main_title="",
                                            data=pitchfx, outcome=pitchfx$whiff, plot_points= "S")
#plot(whiff_FF_3_2$plot)


whiff_SL_0_0 <- modeled_comparison_graphics(model="whiff", count="0-0", pitch_type="SL", 
                                            main_title="",
                                            data=pitchfx, outcome=pitchfx$whiff, plot_points= "S")
#plot(whiff_SL_0_0$plot)                                            

whiff_SL_1_1 <- modeled_comparison_graphics(model="whiff", count="1-1", pitch_type="SL", 
                                            main_title="",
                                            data=pitchfx, outcome=pitchfx$whiff, plot_points= "S")
#plot(whiff_SL_1_1$plot)                                            

whiff_SL_3_2 <- modeled_comparison_graphics(model="whiff", count="3-2", pitch_type="SL", 
                                            main_title="",
                                            data=pitchfx, outcome=pitchfx$whiff, plot_points= "S")
#plot(whiff_SL_3_2$plot)


## To calculate E(Total bases), need P(in play) to use as survival prob.
## (the expected total bases from the total_bases model is E(Total Bases|Ball in play))
foul_FF_0_0 <- modeled_comparison_graphics(model="foul", count="0-0", pitch_type="FF", 
                                            main_title="",
                                            data=pitchfx, plot_points= "N")
prob_in_play_FF_0_0 <- swing_FF_0_0$predicted*(1-whiff_FF_0_0$predicted)*(1-foul_FF_0_0$predicted)

foul_FF_1_1 <- modeled_comparison_graphics(model="foul", count="1-1", pitch_type="FF", 
                                           main_title="",
                                           data=pitchfx, plot_points= "N")
swing_FF_1_1 <- modeled_comparison_graphics(model="swing", count="1-1", pitch_type="FF", 
                                           main_title="",
                                           data=pitchfx, plot_points= "N")

prob_in_play_FF_1_1 <- swing_FF_1_1$predicted*(1-whiff_FF_1_1$predicted)*(1-foul_FF_1_1$predicted)


foul_FF_3_2 <- modeled_comparison_graphics(model="foul", count="3-2", pitch_type="FF", 
                                           main_title="",
                                           data=pitchfx, plot_points= "N")
prob_in_play_FF_3_2 <- swing_FF_3_2$predicted*(1-whiff_FF_3_2$predicted)*(1-foul_FF_3_2$predicted)


tb_FF_0_0 <- modeled_comparison_graphics(model="total_bases", count="0-0", pitch_type="FF", 
                                            main_title="", 
                                            surv_prob = prob_in_play_FF_0_0,
                                            data=pitchfx, outcome=pitchfx$hit_safe, plot_points= "S")
#plot(tb_FF_0_0$plot)                                            

tb_FF_1_1 <- modeled_comparison_graphics(model="total_bases", count="1-1", pitch_type="FF", 
                                         main_title="", 
                                         surv_prob = prob_in_play_FF_1_1,
                                         data=pitchfx, outcome=pitchfx$hit_safe, plot_points= "S")
#plot(tb_FF_1_1$plot)                                            


tb_FF_1_1_N <- modeled_comparison_graphics(model="total_bases", count="1-1", pitch_type="FF", 
                                         main_title="", 
                                         surv_prob = prob_in_play_FF_1_1,
                                         data=pitchfx, outcome=pitchfx$hit_safe, plot_points= "N")
#plot(tb_FF_1_1_N$plot)   
tb_FF_3_2 <- modeled_comparison_graphics(model="total_bases", count="3-2", pitch_type="FF", 
                                            main_title="",
                                         surv_prob = prob_in_play_FF_3_2,
                                         data=pitchfx, outcome=pitchfx$hit_safe, plot_points= "S")
#plot(tb_FF_3_2$plot)


## sliders
foul_SL_0_0 <- modeled_comparison_graphics(model="foul", count="0-0", pitch_type="SL", 
                                           main_title="",
                                           data=pitchfx, plot_points= "N")
prob_in_play_SL_0_0 <- swing_SL_0_0$predicted*(1-whiff_SL_0_0$predicted)*(1-foul_SL_0_0$predicted)

foul_SL_1_1 <- modeled_comparison_graphics(model="foul", count="1-1", pitch_type="SL", 
                                           main_title="",
                                           data=pitchfx, plot_points= "N")
swing_SL_1_1 <- modeled_comparison_graphics(model="swing", count="1-1", pitch_type="SL", 
                                           main_title="",
                                           data=pitchfx, plot_points= "N")

prob_in_play_SL_1_1 <- swing_SL_1_1$predicted*(1-whiff_SL_1_1$predicted)*(1-foul_SL_1_1$predicted)


foul_SL_3_2 <- modeled_comparison_graphics(model="foul", count="3-2", pitch_type="SL", 
                                           main_title="",
                                           data=pitchfx, plot_points= "N")
prob_in_play_SL_3_2 <- swing_SL_3_2$predicted*(1-whiff_SL_3_2$predicted)*(1-foul_SL_3_2$predicted)


tb_SL_0_0 <- modeled_comparison_graphics(model="total_bases", count="0-0", pitch_type="SL", 
                                         main_title="", 
                                         surv_prob = prob_in_play_SL_0_0,
                                         data=pitchfx, outcome=pitchfx$hit_safe, plot_points= "S")
#plot(tb_SL_0_0$plot)                                            

tb_SL_1_1 <- modeled_comparison_graphics(model="total_bases", count="1-1", pitch_type="SL", 
                                         main_title="", 
                                         surv_prob = prob_in_play_SL_1_1,
                                         data=pitchfx, outcome=pitchfx$hit_safe, plot_points= "S")
#plot(tb_SL_1_1$plot)   

tb_SL_1_1_N <- modeled_comparison_graphics(model="total_bases", count="1-1", pitch_type="SL", 
                                         main_title="", 
                                         surv_prob = prob_in_play_SL_1_1,
                                         data=pitchfx, outcome=pitchfx$hit_safe, plot_points= "N")
#plot(tb_SL_1_1_N$plot)    


tb_SL_3_2 <- modeled_comparison_graphics(model="total_bases", count="3-2", pitch_type="SL", 
                                         main_title="",
                                         surv_prob = prob_in_play_SL_3_2,
                                         data=pitchfx, outcome=pitchfx$hit_safe, plot_points= "S")
#plot(tb_SL_3_2$plot)






# ball_prob3 <- model_diagnostic_graphics(data = pitchfx, outcome = pitchfx$ball, 
#                                         model = "strike", count = count_use, plot_points = "S", surv_prob = rep(1,length(swing_prob)),
#                                         complement = "Y",
#                                         main_title = paste("Probability of Pitch Called Ball Given", batter_name_use, 
#                                                            "Does Not Swing", sep=" "))
# 


jpeg("manuscript_graphics/swing_prob_FF_unadjusted.jpg", height=5,width=10,
	units="in", res=200)
plot(FF_unadjust$plot)
dev.off()

jpeg("manuscript_graphics/swing_prob_SL_unadjusted.jpg", height=5,width=10,
	units="in", res=200)
plot(SL_unadjust$plot)
dev.off()


jpeg("manuscript_graphics/swing_prob_FF_count0_0_adjusted.jpg", height=5,width=10,
	units="in", res=200)
plot(swing_FF_0_0$plot)
dev.off()

jpeg("manuscript_graphics/swing_prob_FF_count3_2_adjusted.jpg", height=5,width=10,
	units="in", res=200)
plot(swing_FF_3_2$plot)
dev.off()

jpeg("manuscript_graphics/swing_prob_SL_count0_0_adjusted.jpg", height=5,width=10,
	units="in", res=200)
plot(swing_SL_0_0$plot)
dev.off()

jpeg("manuscript_graphics/swing_prob_SL_count3_2_adjusted.jpg", height=5,width=10,
	units="in", res=200)
plot(swing_SL_3_2$plot)
dev.off()


###WHIFF GIVEN SWING

jpeg("manuscript_graphics/whiff_given_swing_FF_count0_0_adjusted.jpg", height=5,width=10,
	units="in", res=200)
plot(whiff_FF_0_0$plot)
dev.off()

jpeg("manuscript_graphics/whiff_given_swing_FF_count1_1_adjusted.jpg", height=5,width=10,
	units="in", res=200)
plot(whiff_FF_1_1$plot)
dev.off()

jpeg("manuscript_graphics/whiff_given_swing_FF_count3_2_adjusted.jpg", height=5,width=10,
	units="in", res=200)
plot(whiff_FF_3_2$plot)
dev.off()


jpeg("manuscript_graphics/whiff_given_swing_SL_count0_0_adjusted.jpg", height=5,width=10,
	units="in", res=200)
plot(whiff_SL_0_0$plot)
dev.off()


jpeg("manuscript_graphics/whiff_given_swing_SL_count1_1_adjusted.jpg", height=5,width=10,
	units="in", res=200)
plot(whiff_SL_1_1$plot)
dev.off()

jpeg("manuscript_graphics/whiff_given_swing_SL_count3_2_adjusted.jpg", height=5,width=10,
	units="in", res=200)
plot(whiff_SL_3_2$plot)
dev.off()

### Expected bases

jpeg("manuscript_graphics/expected_bases_FF_count0_0_adjusted.jpg", height=5,width=10,
	units="in", res=200)
plot(tb_FF_0_0$plot)
dev.off()


jpeg("manuscript_graphics/expected_bases_FF_count1_1_adjusted.jpg", height=5,width=10,
	units="in", res=200)
plot(tb_FF_1_1$plot)
dev.off()


jpeg("manuscript_graphics/expected_bases_FF_count1_1_adjusted_N.jpg", height=5,width=10,
	units="in", res=200)
plot(tb_FF_1_1_N$plot)
dev.off()

jpeg("manuscript_graphics/expected_bases_FF_count3_2_adjusted.jpg", height=5,width=10,
	units="in", res=200)
plot(tb_FF_3_2$plot)
dev.off()


jpeg("manuscript_graphics/expected_bases_SL_count0_0_adjusted.jpg", height=5,width=10,
	units="in", res=200)
plot(tb_SL_0_0$plot)
dev.off()

jpeg("manuscript_graphics/expected_bases_SL_count1_1_adjusted.jpg", height=5,width=10,
	units="in", res=200)
plot(tb_SL_1_1$plot)
dev.off()

jpeg("manuscript_graphics/expected_bases_SL_count1_1_adjusted_N.jpg", height=5,width=10,
	units="in", res=200)
plot(tb_SL_1_1_N$plot)
dev.off()

jpeg("manuscript_graphics/expected_bases_SL_count3_2_adjusted.jpg", height=5,width=10,
	units="in", res=200)
plot(tb_SL_3_2$plot)
dev.off()

# Clear workspace
rm(list=ls())
