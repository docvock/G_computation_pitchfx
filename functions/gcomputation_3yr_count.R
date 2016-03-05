###################################
# File: ./gcomputation_3yr_count.R
#   Purpose: File with function to implement G-computation. 
#							Allow plate discipline to vary by count. 
#   Author: David M. Vock
#   Last Modified: August 25, 2015
#   Relies On: NA
#   Files Created: NA
####################################

# function for predicted outcome

model_outcomes <- function(hitting_ability, plate_discipline, pitch_selection, strikezone,
	mccutchen_models = mccutchen_models, castro_models = castro_models, 
	pitchfx = pitchfx) {
	# Computes the estimated batting average, on-base percentage, and slugging percentage
	# using the G-computation algorithm using the generalized additive model results of either
	# Andrew McCutchen's or Starlin Castro's hitting ability, plate discipline, pitch selection,
	# and strike zone called.
	#
	# Args:
	# plate_discipline: "Castro" or "McCutchen" or a vector of counts. If "Castro" or "McCutchen" this
	#										defines which player's model for the probability of swinging at the pitch is used
	#										for all counts in the G-computation. If a vector of ball-strike counts, this is the
	#										counts for which McCutchen's plate discipline should be used in the G-computation.  
	# hitting_ability: Either "Castro" or "McCutchen". Defines which player's model for the 
	#                  probability the pitch results in a whiff, foul, out, or hit (and the 
	#                  number of bases), given the pitch is swung at, is used in the G-computation 
	# strike_zone: Either "Castro" or "McCutchen". Defines which player's model for the 
	#              probability a pitch is called a strike is used in the G-computation.
	# pitch_selection: Either "Castro" or "McCutchen". The pitches that this player saw during the 
	#                  season are used in the G-computation.
	# mccutchen_models: List of generalized additive models for Andrew McCutchen.
	# castro_models: List of generalized additive models for Starlin Castro
	# pitchfx: Dataset of pitch information for Castro and McCutchen
	#
	# Returns:
	#   A list with the proportion of plate appearances that result in a hit, walk, out,
	#   batting average, on-base percentage, and slugging percentage.
	
	## Define outcomes of interest
	outcomevars <- c("px", "pz", "pfx_x", "pfx_z", "start_speed", "count", "count_alt", "count_alt2",
		"strikecount", "ballcount", "fullcount")
	
	swingvars <- c("px", "pz", "pfx_x", "pfx_z", "start_speed", "count", "count_alt", "count_alt2",
		"strikecount", "ballcount", "fullcount")
	
	## Designate the appropriate pitch dataset and models to use
	if(pitch_selection == "McCutchen") {
		pitchdata <- filter(pitchfx, starlin == 0 & is.na(px) == F)
	} else {
		if(pitch_selection=="Castro") {
			pitchdata <- filter(pitchfx, starlin == 1 & is.na(px) == F)
		} else {
			print("ERROR - NO PLAYER SELECTED FOR PITCH SELECTION") } }
	
	## Designate the appropriate models for result of swinging
	if(hitting_ability == "McCutchen") {
		whiffmodel <- mccutchen_models$whiff 
		foulmodel <- mccutchen_models$foul
		hitsafemodel <- mccutchen_models$hit
		tbmodel <- mccutchen_models$total_bases
		pitchdata$count_alt <- pitchdata$count
	} else {
		if(hitting_ability == "Castro") {
			whiffmodel <- castro_models$whiff 
			foulmodel <- castro_models$foul
			hitsafemodel <- castro_models$hit
			tbmodel <- castro_models$total_bases
			pitchdata$count_alt <- pitchdata$count
			pitchdata$count_alt[pitchdata$count_alt == "3-0"] <- "2-0"
		} else {
			print("ERROR - NO PLAYER SELECTED FOR HITTING ABILITY") } }
	
	## Designate the appropriate models for probability of swinging
	if(plate_discipline == "McCutchen") {
		swingmodel <- mccutchen_models$swing
		swingmodel_player <- 1
	} else {
		if(plate_discipline == "Castro") {
			swingmodel <- castro_models$swing 
			swingmodel_player <- 1
		} else {
			swingmodel_player <- 0
			print("NO PLAYER SELECTED FOR PLATE DISCIPLINE USING COUNTS") } }
	
	
	## Designate the appropriate models for probability of being a ball.
	if(strikezone == "McCutchen") {
		pitchdata$callball <- 1 - predict(mccutchen_models$strike, newdata = pitchdata, 
			type = "response") 
	} else {
		if(strikezone == "Castro") {
			pitchdata$callball <- 1-predict(castro_models$strike, newdata = pitchdata,
				type="response") 
		} else {
			print("ERROR - NO PLAYER SELECTED FOR STRIKEZONE") } }
	
	
	########
	### 0-0
	########
	if(swingmodel_player == 0) {
		if("0-0" %in% plate_discipline) {
			swingmodel <- mccutchen_models$swing
		} else {
			swingmodel <- castro_models$swing
		}
	}
	
	grid_pred <- pitchdata[pitchdata$count=="0-0", outcomevars]
	grid_pred <- as.data.frame(grid_pred)
	
	pred_whiff <- predict(whiffmodel, newdata=grid_pred,type="response")
	pred_foul <- predict(foulmodel, newdata=grid_pred,type="response")
	pred_hit_safe <- predict(hitsafemodel, newdata=grid_pred,type="response")
	pred_tb <- predict(tbmodel,newdata =grid_pred,type = "response")
	
	pred_whiff_0_0 <- pred_whiff
	pred_foul_0_0 <- (1-pred_whiff)*pred_foul
	pred_hit_safe_0_0 <- (1-pred_whiff)*(1-pred_foul)*pred_hit_safe
	pred_inplay_out_0_0 <-(1-pred_whiff)*(1-pred_foul)*(1-pred_hit_safe)
	pred_tb_0_0 <- (1-pred_whiff)*(1-pred_foul)*pred_tb
	
	#Predict whether swing at pitch
	grid_predM <- as.data.frame(pitchdata[pitchdata$count=="0-0", swingvars])
	pred_swing_0_0 <- predict(swingmodel, newdata=grid_predM, type="response") 
	
	pball_0_0 <- mean(pitchdata$callball[pitchdata$count=="0-0"]*(1-pred_swing_0_0))
	pcallstrike_0_0 <- mean((1-pitchdata$callball[pitchdata$count=="0-0"])*(1-pred_swing_0_0))
	pstrike_0_0 <- pcallstrike_0_0 + mean(pred_foul_0_0*pred_swing_0_0) + 
		mean(pred_whiff_0_0*pred_swing_0_0)
	
	pcount_0_0 <- (1)
	
	HS_0_0 <- mean(pred_hit_safe_0_0*pred_swing_0_0)*pcount_0_0
	TB_0_0 <- mean(pred_tb_0_0*pred_swing_0_0)*pcount_0_0
	OUT_0_0 <- mean(pred_inplay_out_0_0*pred_swing_0_0)*pcount_0_0
	
	rm(grid_pred, grid_predM)
	
	
	#######################################################################
	
	########
	### 1-0
	### Move to 1-0 (ball)
	########
	
	if(swingmodel_player == 0) {
		if("1-0" %in% plate_discipline) {
			swingmodel <- mccutchen_models$swing
		} else {
			swingmodel <- castro_models$swing
		}
	}
	
	grid_pred <- pitchdata[pitchdata$count=="1-0", outcomevars]
	grid_pred <- as.data.frame(grid_pred)
	
	pred_whiff <- predict(whiffmodel, newdata=grid_pred,type="response")
	pred_foul <- predict(foulmodel, newdata=grid_pred,type="response")
	pred_hit_safe <- predict(hitsafemodel, newdata=grid_pred,type="response")
	pred_tb <- predict(tbmodel, newdata=grid_pred,type="response")
	
	pred_whiff_1_0 <- pred_whiff
	pred_foul_1_0 <- (1-pred_whiff)*pred_foul
	pred_hit_safe_1_0 <- (1-pred_whiff)*(1-pred_foul)*pred_hit_safe
	pred_inplay_out_1_0 <-(1-pred_whiff)*(1-pred_foul)*(1-pred_hit_safe)
	pred_tb_1_0 <- (1-pred_whiff)*(1-pred_foul)*pred_tb
	
	#Predict whether swing at pitch
	grid_predM <- as.data.frame(pitchdata[pitchdata$count=="1-0", swingvars])
	pred_swing_1_0 <- predict(swingmodel, newdata=grid_predM, type="response") 
	
	pball_1_0 <- mean(pitchdata$callball[pitchdata$count=="1-0"]*(1-pred_swing_1_0))
	pcallstrike_1_0 <- mean((1-pitchdata$callball[pitchdata$count=="1-0"])*(1-pred_swing_1_0))
	pstrike_1_0 <- pcallstrike_1_0 + mean(pred_foul_1_0*pred_swing_1_0) + 
		mean(pred_whiff_1_0*pred_swing_1_0)
	
	pcount_1_0 <- pcount_0_0*pball_0_0
	
	HS_1_0 <- mean(pred_hit_safe_1_0*pred_swing_1_0)*pcount_1_0
	TB_1_0 <- mean(pred_tb_1_0*pred_swing_1_0)*pcount_1_0
	OUT_1_0 <- mean(pred_inplay_out_1_0*pred_swing_1_0)*pcount_1_0
	
	rm(grid_pred, grid_predM)
	
	
	########
	### 0-1
	### Move to 0-1 (whiff, foul, called strike)
	########
	
	if(swingmodel_player == 0) {
		if("0-1" %in% plate_discipline) {
			swingmodel <- mccutchen_models$swing
		} else {
			swingmodel <- castro_models$swing
		}
	}
	
	grid_pred <- pitchdata[pitchdata$count=="0-1", outcomevars]
	grid_pred <- as.data.frame(grid_pred)
	
	pred_whiff <- predict(whiffmodel, newdata=grid_pred,type="response")
	pred_foul <- predict(foulmodel, newdata=grid_pred,type="response")
	pred_hit_safe <- predict(hitsafemodel, newdata=grid_pred,type="response")
	pred_tb <- predict(tbmodel, newdata=grid_pred,type="response")
	
	pred_whiff_0_1 <- pred_whiff
	pred_foul_0_1 <- (1-pred_whiff)*pred_foul
	pred_hit_safe_0_1 <- (1-pred_whiff)*(1-pred_foul)*pred_hit_safe
	pred_inplay_out_0_1 <-(1-pred_whiff)*(1-pred_foul)*(1-pred_hit_safe)
	pred_tb_0_1 <- (1-pred_whiff)*(1-pred_foul)*pred_tb
	
	#Predict whether swing at pitch
	grid_predM <- as.data.frame(pitchdata[pitchdata$count=="0-1", swingvars])
	pred_swing_0_1 <- predict(swingmodel, newdata=grid_predM, type="response") 
	
	pball_0_1 <- mean(pitchdata$callball[pitchdata$count=="0-1"]*(1-pred_swing_0_1))
	pcallstrike_0_1 <- mean((1-pitchdata$callball[pitchdata$count=="0-1"])*(1-pred_swing_0_1))
	pstrike_0_1 <- pcallstrike_0_1 + mean(pred_foul_0_1*pred_swing_0_1) + 
		mean(pred_whiff_0_1*pred_swing_0_1)
	
	pcount_0_1 <- pcount_0_0*pstrike_0_0
	
	HS_0_1 <- mean(pred_hit_safe_0_1*pred_swing_0_1)*pcount_0_1
	TB_0_1 <- mean(pred_tb_0_1*pred_swing_0_1)*pcount_0_1
	OUT_0_1 <- mean(pred_inplay_out_0_1*pred_swing_0_1)*pcount_0_1
	
	rm(grid_pred, grid_predM)
	
	
	#####################################################################################################
	
	########
	### 2-0
	### Move to 2-0 (ball from 1-0)
	########
	
	if(swingmodel_player == 0) {
		if("2-0" %in% plate_discipline) {
			swingmodel <- mccutchen_models$swing
		} else {
			swingmodel <- castro_models$swing
		}
	}
	
	grid_pred <- pitchdata[pitchdata$count=="2-0", outcomevars]
	grid_pred <- as.data.frame(grid_pred)
	
	pred_whiff <- predict(whiffmodel, newdata=grid_pred,type="response")
	pred_foul <- predict(foulmodel, newdata=grid_pred,type="response")
	pred_hit_safe <- predict(hitsafemodel, newdata=grid_pred,type="response")
	pred_tb <- predict(tbmodel, newdata=grid_pred,type="response")
	
	pred_whiff_2_0 <- pred_whiff
	pred_foul_2_0 <- (1-pred_whiff)*pred_foul
	pred_hit_safe_2_0 <- (1-pred_whiff)*(1-pred_foul)*pred_hit_safe
	pred_inplay_out_2_0 <-(1-pred_whiff)*(1-pred_foul)*(1-pred_hit_safe)
	pred_tb_2_0 <- (1-pred_whiff)*(1-pred_foul)*pred_tb
	
	#Predict whether swing at pitch
	grid_predM <- as.data.frame(pitchdata[pitchdata$count=="2-0", swingvars])
	pred_swing_2_0 <- predict(swingmodel, newdata=grid_predM, type="response") 
	
	pball_2_0 <- mean(pitchdata$callball[pitchdata$count=="2-0"]*(1-pred_swing_2_0))
	pcallstrike_2_0 <- mean((1-pitchdata$callball[pitchdata$count=="2-0"])*(1-pred_swing_2_0))
	pstrike_2_0 <- pcallstrike_2_0 + mean(pred_foul_2_0*pred_swing_2_0) + 
		mean(pred_whiff_2_0*pred_swing_2_0)
	
	pcount_2_0 <- pball_1_0*pcount_1_0
	
	HS_2_0 <- mean(pred_hit_safe_2_0*pred_swing_2_0)*pcount_2_0
	TB_2_0 <- mean(pred_tb_2_0*pred_swing_2_0)*pcount_2_0
	OUT_2_0 <- mean(pred_inplay_out_2_0*pred_swing_2_0)*pcount_2_0
	
	rm(grid_pred, grid_predM)
	
	
	########
	### 0-2
	### Move to 0-2 (foul, whiff or called strike from 0-1 OR foul from 0-2)
	########
	
	if(swingmodel_player == 0) {
		if("0-2" %in% plate_discipline) {
			swingmodel <- mccutchen_models$swing
		} else {
			swingmodel <- castro_models$swing
		}
	}
	
	grid_pred <- pitchdata[pitchdata$count=="0-2", outcomevars]
	grid_pred <- as.data.frame(grid_pred)
	
	pred_whiff <- predict(whiffmodel, newdata=grid_pred,type="response")
	pred_foul <- predict(foulmodel, newdata=grid_pred,type="response")
	pred_hit_safe <- predict(hitsafemodel, newdata=grid_pred,type="response")
	pred_tb <- predict(tbmodel, newdata=grid_pred,type="response")
	
	pred_whiff_0_2 <- pred_whiff
	pred_foul_0_2 <- (1-pred_whiff)*pred_foul
	pred_hit_safe_0_2 <- (1-pred_whiff)*(1-pred_foul)*pred_hit_safe
	pred_inplay_out_0_2 <-(1-pred_whiff)*(1-pred_foul)*(1-pred_hit_safe)
	pred_tb_0_2 <- (1-pred_whiff)*(1-pred_foul)*pred_tb
	
	#Predict whether swing at pitch
	grid_predM <- as.data.frame(pitchdata[pitchdata$count=="0-2", swingvars])
	pred_swing_0_2 <- predict(swingmodel, newdata=grid_predM, type="response") 
	
	pball_0_2 <- mean(pitchdata$callball[pitchdata$count=="0-2"]*(1-pred_swing_0_2))
	pcallstrike_0_2 <- mean((1-pitchdata$callball[pitchdata$count=="0-2"])*(1-pred_swing_0_2))
	pstrike_0_2 <- pcallstrike_0_2 + mean(pred_whiff_0_2*pred_swing_0_2)  # FOUL IS NOT A STRIKE
	
	pcount_0_2 <- (pstrike_0_1*pcount_0_1)/(1-mean(pred_foul_0_2*pred_swing_0_2))
	
	HS_0_2 <- mean(pred_hit_safe_0_2*pred_swing_0_2)*pcount_0_2
	TB_0_2 <- mean(pred_tb_0_2*pred_swing_0_2)*pcount_0_2
	OUT_0_2 <- mean(pred_inplay_out_0_2*pred_swing_0_2)*pcount_0_2+pstrike_0_2*pcount_0_2
	
	rm(grid_pred, grid_predM)
	
	
	########
	### 1-1
	### Move to 1-1 (foul, whiff or called strike from 1-0 OR ball from 0-1)
	########
	
	if(swingmodel_player == 0) {
		if("1-1" %in% plate_discipline) {
			swingmodel <- mccutchen_models$swing
		} else {
			swingmodel <- castro_models$swing
		}
	}
	
	grid_pred <- pitchdata[pitchdata$count=="1-1", outcomevars]
	grid_pred <- as.data.frame(grid_pred)
	
	pred_whiff <- predict(whiffmodel, newdata=grid_pred,type="response")
	pred_foul <- predict(foulmodel, newdata=grid_pred,type="response")
	pred_hit_safe <- predict(hitsafemodel, newdata=grid_pred,type="response")
	pred_tb <- predict(tbmodel, newdata=grid_pred,type="response")
	
	pred_whiff_1_1 <- pred_whiff
	pred_foul_1_1 <- (1-pred_whiff)*pred_foul
	pred_hit_safe_1_1 <- (1-pred_whiff)*(1-pred_foul)*pred_hit_safe
	pred_inplay_out_1_1 <-(1-pred_whiff)*(1-pred_foul)*(1-pred_hit_safe)
	pred_tb_1_1 <- (1-pred_whiff)*(1-pred_foul)*pred_tb
	
	#Predict whether swing at pitch
	grid_predM <- as.data.frame(pitchdata[pitchdata$count=="1-1", swingvars])
	pred_swing_1_1 <- predict(swingmodel, newdata=grid_predM, type="response") 
	
	pball_1_1 <- mean(pitchdata$callball[pitchdata$count=="1-1"]*(1-pred_swing_1_1))
	pcallstrike_1_1 <- mean((1-pitchdata$callball[pitchdata$count=="1-1"])*(1-pred_swing_1_1))
	pstrike_1_1 <- pcallstrike_1_1 + mean(pred_foul_1_1*pred_swing_1_1) + 
		mean(pred_whiff_1_1*pred_swing_1_1)
	
	pcount_1_1 <- pstrike_1_0*pcount_1_0 + pball_0_1*pcount_0_1
	
	HS_1_1 <- mean(pred_hit_safe_1_1*pred_swing_1_1)*pcount_1_1
	TB_1_1 <- mean(pred_tb_1_1*pred_swing_1_1)*pcount_1_1
	OUT_1_1 <- mean(pred_inplay_out_1_1*pred_swing_1_1)*pcount_1_1
	
	rm(grid_pred, grid_predM)
	
	
	######################################################################################################
	
	########
	### 3-0
	### Move to 3-0 (ball from 2-0)
	########
	
	if(swingmodel_player == 0) {
		if("3-0" %in% plate_discipline) {
			swingmodel <- mccutchen_models$swing
		} else {
			swingmodel <- castro_models$swing
		}
	}
	
	grid_pred <- pitchdata[pitchdata$count=="3-0", outcomevars]
	grid_pred <- as.data.frame(grid_pred)
	
	pred_whiff <- predict(whiffmodel, newdata=grid_pred,type="response")
	pred_foul <- predict(foulmodel, newdata=grid_pred,type="response")
	pred_hit_safe <- predict(hitsafemodel, newdata=grid_pred,type="response")
	pred_tb <- predict(tbmodel, newdata=grid_pred,type="response")
	
	pred_whiff_3_0 <- pred_whiff
	pred_foul_3_0 <- (1-pred_whiff)*pred_foul
	pred_hit_safe_3_0 <- (1-pred_whiff)*(1-pred_foul)*pred_hit_safe
	pred_inplay_out_3_0 <-(1-pred_whiff)*(1-pred_foul)*(1-pred_hit_safe)
	pred_tb_3_0 <- (1-pred_whiff)*(1-pred_foul)*pred_tb
	
	#Predict whether swing at pitch
	grid_predM <- as.data.frame(pitchdata[pitchdata$count=="3-0", swingvars])
	pred_swing_3_0 <- predict(swingmodel, newdata=grid_predM, type="response") 
	
	pball_3_0 <- mean(pitchdata$callball[pitchdata$count=="3-0"]*(1-pred_swing_3_0))
	pcallstrike_3_0 <- mean((1-pitchdata$callball[pitchdata$count=="3-0"])*(1-pred_swing_3_0))
	pstrike_3_0 <- pcallstrike_3_0 + mean(pred_foul_3_0*pred_swing_3_0) + 
		mean(pred_whiff_3_0*pred_swing_3_0)
	
	pcount_3_0 <- pball_2_0*pcount_2_0
	
	HS_3_0 <- mean(pred_hit_safe_3_0*pred_swing_3_0)*pcount_3_0
	TB_3_0 <- mean(pred_tb_3_0*pred_swing_3_0)*pcount_3_0 
	OUT_3_0 <- mean(pred_inplay_out_3_0*pred_swing_3_0)*pcount_3_0
	WALK_3_0 <- pball_3_0*pcount_3_0
	
	rm(grid_pred, grid_predM)
	
	
	########
	### 2-1
	### Move to 2-1 (ball from 1-1 OR strike from 2-0)
	########
	
	if(swingmodel_player == 0) {
		if("2-1" %in% plate_discipline) {
			swingmodel <- mccutchen_models$swing
		} else {
			swingmodel <- castro_models$swing
		}
	}
	
	grid_pred <- pitchdata[pitchdata$count=="2-1", outcomevars]
	grid_pred <- as.data.frame(grid_pred)
	
	pred_whiff <- predict(whiffmodel, newdata=grid_pred,type="response")
	pred_foul <- predict(foulmodel, newdata=grid_pred,type="response")
	pred_hit_safe <- predict(hitsafemodel, newdata=grid_pred,type="response")
	pred_tb <- predict(tbmodel, newdata=grid_pred,type="response")
	
	pred_whiff_2_1 <- pred_whiff
	pred_foul_2_1 <- (1-pred_whiff)*pred_foul
	pred_hit_safe_2_1 <- (1-pred_whiff)*(1-pred_foul)*pred_hit_safe
	pred_inplay_out_2_1 <-(1-pred_whiff)*(1-pred_foul)*(1-pred_hit_safe)
	pred_tb_2_1 <- (1-pred_whiff)*(1-pred_foul)*pred_tb
	
	#Predict whether swing at pitch
	grid_predM <- as.data.frame(pitchdata[pitchdata$count=="2-1", swingvars])
	pred_swing_2_1 <- predict(swingmodel, newdata=grid_predM, type="response") 
	
	pball_2_1 <- mean(pitchdata$callball[pitchdata$count=="2-1"]*(1-pred_swing_2_1))
	pcallstrike_2_1 <- mean((1-pitchdata$callball[pitchdata$count=="2-1"])*(1-pred_swing_2_1))
	pstrike_2_1 <- pcallstrike_2_1 + mean(pred_foul_2_1*pred_swing_2_1) + 
		mean(pred_whiff_2_1*pred_swing_2_1)
	
	pcount_2_1 <- pball_1_1*pcount_1_1+ pstrike_2_0*pcount_2_0
	
	HS_2_1 <- mean(pred_hit_safe_2_1*pred_swing_2_1)*pcount_2_1
	TB_2_1 <- mean(pred_tb_2_1*pred_swing_2_1)*pcount_2_1
	OUT_2_1 <- mean(pred_inplay_out_2_1*pred_swing_2_1)*pcount_2_1
	
	rm(grid_pred, grid_predM)
	
	
	########
	### 1-2
	### Move to 1-2 (ball from 0-2 OR strike from 1-1 OR foul from 1-2)
	########
	
	if(swingmodel_player == 0) {
		if("1-2" %in% plate_discipline) {
			swingmodel <- mccutchen_models$swing
		} else {
			swingmodel <- castro_models$swing
		}
	}
	
	grid_pred <- pitchdata[pitchdata$count=="1-2", outcomevars]
	grid_pred <- as.data.frame(grid_pred)
	
	pred_whiff <- predict(whiffmodel, newdata=grid_pred,type="response")
	pred_foul <- predict(foulmodel, newdata=grid_pred,type="response")
	pred_hit_safe <- predict(hitsafemodel, newdata=grid_pred,type="response")
	pred_tb <- predict(tbmodel, newdata=grid_pred,type="response")
	
	pred_whiff_1_2 <- pred_whiff
	pred_foul_1_2 <- (1-pred_whiff)*pred_foul
	pred_hit_safe_1_2 <- (1-pred_whiff)*(1-pred_foul)*pred_hit_safe
	pred_inplay_out_1_2 <-(1-pred_whiff)*(1-pred_foul)*(1-pred_hit_safe)
	pred_tb_1_2 <- (1-pred_whiff)*(1-pred_foul)*pred_tb
	
	#Predict whether swing at pitch
	grid_predM <- as.data.frame(pitchdata[pitchdata$count=="1-2", swingvars])
	pred_swing_1_2 <- predict(swingmodel, newdata=grid_predM, type="response") 
	
	pball_1_2 <- mean(pitchdata$callball[pitchdata$count=="1-2"]*(1-pred_swing_1_2))
	pcallstrike_1_2 <- mean((1-pitchdata$callball[pitchdata$count=="1-2"])*(1-pred_swing_1_2))
	pstrike_1_2 <- pcallstrike_1_2 + mean(pred_whiff_1_2*pred_swing_1_2) #Foul is not a STRIKE
	
	pcount_1_2 <- (pball_0_2*pcount_0_2 + pstrike_1_1*pcount_1_1)/(1-mean(pred_foul_1_2*pred_swing_1_2))
	
	HS_1_2 <- mean(pred_hit_safe_1_2*pred_swing_1_2)*pcount_1_2
	TB_1_2 <- mean(pred_tb_1_2*pred_swing_1_2)*pcount_1_2
	OUT_1_2 <- mean(pred_inplay_out_1_2*pred_swing_1_2)*pcount_1_2+pstrike_1_2*pcount_1_2
	
	rm(grid_pred, grid_predM)
	
	
	######################################################################################################
	
	########
	### 3-1
	### Move to 3-1 (ball from 2-1 or strike from 3-0)
	########
	
	if(swingmodel_player == 0) {
		if("3-1" %in% plate_discipline) {
			swingmodel <- mccutchen_models$swing
		} else {
			swingmodel <- castro_models$swing
		}
	}
	
	grid_pred <- pitchdata[pitchdata$count=="3-1", outcomevars]
	grid_pred <- as.data.frame(grid_pred)
	
	pred_whiff <- predict(whiffmodel, newdata=grid_pred,type="response")
	pred_foul <- predict(foulmodel, newdata=grid_pred,type="response")
	pred_hit_safe <- predict(hitsafemodel, newdata=grid_pred,type="response")
	pred_tb <- predict(tbmodel, newdata=grid_pred,type="response")
	
	pred_whiff_3_1 <- pred_whiff
	pred_foul_3_1 <- (1-pred_whiff)*pred_foul
	pred_hit_safe_3_1 <- (1-pred_whiff)*(1-pred_foul)*pred_hit_safe
	pred_inplay_out_3_1 <-(1-pred_whiff)*(1-pred_foul)*(1-pred_hit_safe)
	pred_tb_3_1 <- (1-pred_whiff)*(1-pred_foul)*pred_tb
	
	#Predict whether swing at pitch
	grid_predM <- as.data.frame(pitchdata[pitchdata$count=="3-1", swingvars])
	pred_swing_3_1 <- predict(swingmodel, newdata=grid_predM, type="response") 
	
	pball_3_1 <- mean(pitchdata$callball[pitchdata$count=="3-1"]*(1-pred_swing_3_1))
	pcallstrike_3_1 <- mean((1-pitchdata$callball[pitchdata$count=="3-1"])*(1-pred_swing_3_1))
	pstrike_3_1 <- pcallstrike_3_1 + mean(pred_foul_3_1*pred_swing_3_1) + 
		mean(pred_whiff_3_1*pred_swing_3_1)
	
	pcount_3_1 <- pball_2_1*pcount_2_1 + pstrike_3_0*pcount_3_0
	
	HS_3_1 <- mean(pred_hit_safe_3_1*pred_swing_3_1)*pcount_3_1
	TB_3_1 <- mean(pred_tb_3_1*pred_swing_3_1)*pcount_3_1 
	OUT_3_1 <- mean(pred_inplay_out_3_1*pred_swing_3_1)*pcount_3_1
	WALK_3_1 <- pball_3_1*pcount_3_1
	
	rm(grid_pred, grid_predM)
	
	
	########
	### 2-2
	### Move to 2-2 (ball from 1-2 or strike from 2-1 or FOUL from 2-2)
	########
	
	if(swingmodel_player == 0) {
		if("2-2" %in% plate_discipline) {
			swingmodel <- mccutchen_models$swing
		} else {
			swingmodel <- castro_models$swing
		}
	}
	
	grid_pred <- pitchdata[pitchdata$count=="2-2", outcomevars]
	grid_pred <- as.data.frame(grid_pred)
	
	pred_whiff <- predict(whiffmodel, newdata=grid_pred,type="response")
	pred_foul <- predict(foulmodel, newdata=grid_pred,type="response")
	pred_hit_safe <- predict(hitsafemodel, newdata=grid_pred,type="response")
	pred_tb <- predict(tbmodel, newdata=grid_pred,type="response")
	
	pred_whiff_2_2 <- pred_whiff
	pred_foul_2_2 <- (1-pred_whiff)*pred_foul
	pred_hit_safe_2_2 <- (1-pred_whiff)*(1-pred_foul)*pred_hit_safe
	pred_inplay_out_2_2 <-(1-pred_whiff)*(1-pred_foul)*(1-pred_hit_safe)
	pred_tb_2_2 <- (1-pred_whiff)*(1-pred_foul)*pred_tb
	
	
	#Predict whether swing at pitch
	grid_predM <- as.data.frame(pitchdata[pitchdata$count=="2-2", swingvars])
	pred_swing_2_2 <- predict(swingmodel, newdata=grid_predM, type="response") 
	
	pball_2_2 <- mean(pitchdata$callball[pitchdata$count=="2-2"]*(1-pred_swing_2_2))
	pcallstrike_2_2 <- mean((1-pitchdata$callball[pitchdata$count=="2-2"])*(1-pred_swing_2_2))
	pstrike_2_2 <- pcallstrike_2_2 + mean(pred_whiff_2_2*pred_swing_2_2)  #Foul is not a strike
	
	pcount_2_2 <- (pball_1_2*pcount_1_2 + pstrike_2_1*pcount_2_1)/(1-mean(pred_foul_2_2*pred_swing_2_2))
	
	HS_2_2 <- mean(pred_hit_safe_2_2*pred_swing_2_2)*pcount_2_2
	TB_2_2 <- mean(pred_tb_2_2*pred_swing_2_2)*pcount_2_2 
	OUT_2_2 <- mean(pred_inplay_out_2_2*pred_swing_2_2)*pcount_2_2+pstrike_2_2*pcount_2_2
	
	rm(grid_pred, grid_predM)
	
	
	######################################################################################################
	
	# Move to 3-2 (ball from 2-2 or strike from 3-1 or FOUL from 3-2)
	########
	### 3-2
	########
	
	if(swingmodel_player == 0) {
		if("3-2" %in% plate_discipline) {
			swingmodel <- mccutchen_models$swing
		} else {
			swingmodel <- castro_models$swing
		}
	}
	
	grid_pred <- pitchdata[pitchdata$count=="3-2", outcomevars]
	grid_pred <- as.data.frame(grid_pred)
	
	pred_whiff <- predict(whiffmodel, newdata=grid_pred,type="response")
	pred_foul <- predict(foulmodel, newdata=grid_pred,type="response")
	pred_hit_safe <- predict(hitsafemodel, newdata=grid_pred,type="response")
	pred_tb <- predict(tbmodel, newdata=grid_pred,type="response")
	
	pred_whiff_3_2 <- pred_whiff
	pred_foul_3_2 <- (1-pred_whiff)*pred_foul
	pred_hit_safe_3_2 <- (1-pred_whiff)*(1-pred_foul)*pred_hit_safe
	pred_inplay_out_3_2 <-(1-pred_whiff)*(1-pred_foul)*(1-pred_hit_safe)
	pred_tb_3_2 <- (1-pred_whiff)*(1-pred_foul)*pred_tb
	
	#Predict whether swing at pitch
	grid_predM <- as.data.frame(pitchdata[pitchdata$count=="3-2", swingvars])
	pred_swing_3_2 <- predict(swingmodel, newdata=grid_predM, type="response") 
	
	pball_3_2 <- mean(pitchdata$callball[pitchdata$count=="3-2"]*(1-pred_swing_3_2))
	pcallstrike_3_2 <- mean((1-pitchdata$callball[pitchdata$count=="3-2"])*(1-pred_swing_3_2))
	pstrike_3_2 <- pcallstrike_3_2 + mean(pred_whiff_3_2*pred_swing_3_2)  #Foul is not a strike
	
	pcount_3_2 <- (pball_2_2*pcount_2_2 + pstrike_3_1*pcount_3_1)/(1-mean(pred_foul_3_2*pred_swing_3_2))
	
	HS_3_2 <- mean(pred_hit_safe_3_2*pred_swing_3_2)*pcount_3_2
	TB_3_2 <- mean(pred_tb_3_2*pred_swing_3_2)*pcount_3_2
	OUT_3_2 <- mean(pred_inplay_out_3_2*pred_swing_3_2)*pcount_3_2+pstrike_3_2*pcount_3_2
	WALK_3_2 <- pball_3_2*pcount_3_2
	
	rm(grid_pred, grid_predM)
	
	########
	##  batting outcomes
	####
	
	hit_safe_prop <- HS_0_0 + HS_1_0 + HS_0_1 + HS_2_0 + HS_0_2 +  HS_1_1 + HS_3_0 + HS_2_1 + HS_1_2 + 
		HS_3_1 + HS_2_2 + HS_3_2
	out_prop <- OUT_0_0 + OUT_1_0 + OUT_0_1 + OUT_2_0 + OUT_0_2 +  OUT_1_1 + OUT_3_0 + OUT_2_1 + OUT_1_2 + 
		OUT_3_1 + OUT_2_2 + OUT_3_2
	walk_prop <- WALK_3_0 + WALK_3_1 + WALK_3_2
	tb_expect <- TB_0_0 + TB_1_0 + TB_0_1 + TB_2_0 + TB_0_2 + TB_1_1 + TB_3_0 + TB_2_1 + TB_1_2 + 
		TB_3_1 + TB_2_2 + TB_3_2
	
	outcome <- c(hit_safe_prop,out_prop,walk_prop)
	names(outcome) <- c("hit_safe","out","walk")
	
	
	batting_average <- hit_safe_prop/(hit_safe_prop+out_prop)
	on_base_percentage <- hit_safe_prop+walk_prop
	slugging_percentage <- tb_expect/(1 - walk_prop)
	
	model_outcomes <- list(outcome, batting_average, on_base_percentage, slugging_percentage)
	names(model_outcomes) <- c("outcome", "batting_average", "on_base_percentage", 
		"slugging_percentage")
	return(model_outcomes)
}