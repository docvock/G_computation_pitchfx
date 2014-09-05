###################################
# File: ./pitchfx_models_function.R
#   Purpose: Function to develop models for whether or not batter swings, outcome if batter swings, and
#		         whether or not pitch is called strike if the batter does not swing 
#   Author: David M. Vock
#   Last Modified: Augst 10, 2014
#   Relies On: ./pitchfx_models_graphics.R
#   Files Created: NA 
####################################

# Create function for generating necessary models for specific player

gam_models <- function(pitchfx, batter_id, verbose = T, count_use = "1-1", sp.swing = rep(-1, 3), 
                       sp.whiff = rep(-1, 3), sp.foul = rep(-1, 3), sp.hit = rep(-1, 3),
                       sp.tb = rep(-1, 3), sp.strike = rep(-1, 3)) {
	# Computes generalized additive models for the probability of swinging at pitch, probability of
	# whiffing given pitch is swung at, probability of fouling off pitch given batter makes contact,
	# probability of hitting safely given ball is put in play, the expected total bases given ball is
	# put in play, and probability pitch is called strike given batter does not swing.
	#
	# Args:
	# pitchfx: Dataset with pitchfx data and outcomes of pitch possibly for multiple players.
	# batter_id: Pitchfx id of the batter for whom we are developing models.
	# verbose: If equal to TRUE then the summary of the GAM should be printed and a pdf of the
	#          estimated probability by location and pitch type should be output.
	# count_use: The count that should be used for the estimated probability.
	# sp.swing, sp.whiff, sp.foul, sp.hit, sp.tb, sp.strike: The smoothing parameters that should be
	#           used in the model for the probability of swinging, whiffing, fouling, hitting safely, 
	#           total bases, and having a strike called.  The default is -1 for all parameters which
	#           indicates that these parameters should be estimated. 
  # Returns:
	# List of the generalized additive model output from each model. 
	
	pitchfx_id <- filter(pitchfx, batter == batter_id)
	pitchfx_swing_id <- filter(pitchfx_id, swing == 1)
	pitchfx_noswing_id <- filter(pitchfx_id, swing == 0)
	
	if (batter_id == 516770) {
		print("Models for Starlin Castro")
		batter_name_use <- "Castro"
	}
	if (batter_id == 457705) {
		print("Models for Andrew McCutchen")
		batter_name_use <- "McCutchen"
	}	
	
	# Estimate the probability batter swings at pitch using GAM	
	print("Fitting swing model")
	swing_gam <- gam(swing ~ s(start_speed, bs = "ts", k = 30) + as.factor(count) +
	                 te(px, pz, pfx_x, pfx_z, bs = "ts", d = c(2, 2), k = c(15, 15)), 
	                 family = "binomial", link = "logit", sp = sp.swing, data = pitchfx_id)
	if (verbose == T) {
		print(summary(swing_gam))
		pdf(paste("baseball_graphics/swing_gam_", batter_name_use, ".pdf", sep=""),
			width = 16, height = 10)
		swing_prob <- model_diagnostic_graphics(data = pitchfx, outcome = pitchfx$swing, 
			model = swing_gam, count = count_use, plot_points = "N", surv_prob = 1,
			main_title = paste("Probability of", batter_name_use,  "Swinging at Pitch", sep=" "))
		plot(swing_prob[[2]])
		swing_prob <- swing_prob[[1]]
		dev.off()
	}
	
	# Given batter swings, estimate probability the batter whiffs using GAM
	print("Fitting whiff model")
	whiff_gam <- gam(whiff ~ s(start_speed, bs = "ts", k = 30) + as.factor(strikecount) + 
                    te(px, pz, pfx_x, pfx_z, bs = "ts", d = c(2, 2), k = c(15, 15)),
                    family = "binomial", link = "logit", sp = sp.whiff, data = pitchfx_swing_id)
	if (verbose == T) {
		print(summary(whiff_gam))
		pdf(paste("baseball_graphics/whiff_gam_", batter_name_use, ".pdf", sep=""), 
			width = 16, height = 10)
		whiff_prob <- model_diagnostic_graphics(data = pitchfx_swing_id, 
			outcome = pitchfx_swing_id$whiff, model = whiff_gam, count = count_use, plot_points = "Y", 
			surv_prob = 1,
			main_title = paste("Probability of Whiffing Pitch Given", batter_name_use, "Swings", sep=" "))
		plot(whiff_prob[[2]])
		whiff_prob <- whiff_prob[[1]]
		dev.off()

		pdf(paste("baseball_graphics/prob_whiff_", batter_name_use, ".pdf", sep=""), 
			width = 16, height = 10)
		whiff_prob2 <- model_diagnostic_graphics(data = pitchfx_id, outcome = pitchfx_id$whiff, 
			model = whiff_gam, count = count_use, plot_points = "S", surv_prob = swing_prob, 
			main_title = paste("Probability of", batter_name_use, "Whiffing Pitch", sep=" "))
		plot(whiff_prob2[[2]])
		dev.off()
		
		pdf(paste("baseball_graphics/prob_whiff_given_swing_", batter_name_use, ".pdf", sep=""), 
			width = 16, height = 10)
		whiff_prob3 <- model_diagnostic_graphics(data = pitchfx_id, outcome = pitchfx_id$whiff, 
			model = whiff_gam, count = count_use, plot_points = "S", surv_prob = rep(1,length(swing_prob)), 
			main_title = paste("Probability of Whiffing Pitch Given", batter_name_use, 
				"Swings", sep=" "))
		plot(whiff_prob3[[2]])
		dev.off()
	}
	
	# Given batter swings and makes contact, estimate the probability the batter fouls off pitch using
	# GAM
	print("Fitting foul model")
	pitchfx_swing_id_sub1 <- filter(pitchfx_swing_id, result != "whiff")
	foul_gam <- gam(foul ~ s(start_speed, bs = "ts", k = 30) + as.factor(strikecount) +
                   s(px, pz, bs = "ts", k = 30) + s(pfx_x, pfx_z, bs = "ts", k = 30),
                   family = "binomial", link = "logit", sp = sp.foul, data = pitchfx_swing_id_sub1)
	if (verbose == T) {
		print(summary(foul_gam))
		pdf(paste("baseball_graphics/foul_gam_", batter_name_use, ".pdf", sep=""), 
			width = 16, height = 10)
		foul_prob <- model_diagnostic_graphics(data = pitchfx_swing_id_sub1, 
			outcome = pitchfx_swing_id_sub1$foul, model = foul_gam, count = count_use, plot_points = "Y",
			surv_prob = 1,
			main_title = paste("Probability of Fouling Off Pitch Given", batter_name_use, 
				"Makes Contact", sep=" "))
		plot(foul_prob[[2]])
		foul_prob <- foul_prob[[1]]
		dev.off()
		
		pdf(paste("baseball_graphics/prob_foul_", batter_name_use, ".pdf", sep=""), 
			width = 16, height = 10)
		foul_prob2 <- model_diagnostic_graphics(data = pitchfx_id, outcome = pitchfx_id$foul, 
			model = foul_gam, count = count_use, plot_points = "S", surv_prob = swing_prob * (1 - whiff_prob), 
			main_title = paste("Probability of", batter_name_use, "Fouling Off Pitch", sep=" "))
		plot(foul_prob2[[2]])
		dev.off()
		
		pdf(paste("baseball_graphics/prob_foul_given_swing_", batter_name_use, ".pdf", sep=""), 
			width = 16, height = 10)
		foul_prob3 <- model_diagnostic_graphics(data = pitchfx_id, outcome = pitchfx_id$foul, 
			model = foul_gam, count = count_use, plot_points = "S", surv_prob = 1-whiff_prob, 
			main_title = paste("Probability of Fouling Off Pitch Given", batter_name_use, 
				"Swings", sep=" "))
		plot(foul_prob3[[2]])
		dev.off()
	}
	
	# Given batter swings, makes contact, and put ball into play, estimate the probability the batter 
	# hits safely using GAM
	print("Fitting hit model")
	pitchfx_swing_id_sub2 <- filter(pitchfx_swing_id, result != "whiff" & result != "foul")
	hit_gam <- gam(hit_safe ~ s(start_speed, bs = "ts", k = 30) + as.factor(strikecount) +
                  s(px, pz, bs = "ts", k = 30) + s(pfx_x, pfx_z, bs = "ts", k = 30),
	                family = "binomial", link = "logit", sp = sp.hit, data = pitchfx_swing_id_sub2)
	if (verbose == T) {
		print(summary(hit_gam))
		pdf(paste("baseball_graphics/hit_gam_", batter_name_use, ".pdf", sep=""), 
			width = 16, height = 10)
		hit_prob <- model_diagnostic_graphics(data = pitchfx_swing_id_sub2, 
			outcome = pitchfx_swing_id_sub2$hit_safe, model = hit_gam, count = count_use, plot_points = "Y",
			surv_prob = 1, main_title = paste("Probability of Hitting Safely Given", batter_name_use, 
				"Puts Ball In-Play", sep=" "))
		plot(hit_prob[[2]])
		hit_prob <- hit_prob[[1]]
		dev.off()
		
		pdf(paste("baseball_graphics/prob_hit_", batter_name_use, ".pdf", sep=""), 
			width = 16, height = 10)
		hit_prob2 <- model_diagnostic_graphics(data = pitchfx_id, outcome = pitchfx_id$hit_safe, 
			model = hit_gam, count = count_use, plot_points = "S", 
			surv_prob = swing_prob * (1 - whiff_prob) * (1-foul_prob), 
			main_title = paste("Probability of", batter_name_use, "Hitting Safely", sep=" "))
		plot(hit_prob2[[2]])
		dev.off()
		
		pdf(paste("baseball_graphics/prob_hit_given_swing_", batter_name_use, ".pdf", sep=""), 
			width = 16, height = 10)
		hit_prob3 <- model_diagnostic_graphics(data = pitchfx_id, outcome = pitchfx_id$hit_safe, 
			model = hit_gam, count = count_use, plot_points = "S", 
			surv_prob = (1 - whiff_prob) * (1 - foul_prob), 
			main_title = paste("Probability of Hitting Safely Given", batter_name_use, 
				"Swings", sep=" "))
		plot(hit_prob3[[2]])
		dev.off()		
	}
	
	# Given batter swings, makes contact, and put ball into play, estimate the total number of bases 
	# the batter hits safely using GAM
	print("Fitting total bases model")
	tb_gam <- gam(total_bases ~ s(start_speed, bs = "ts", k = 30) + as.factor(strikecount) +
                  s(px, pz, bs = "ts", k = 30) + s(pfx_x, pfx_z, bs = "ts", k = 30),
	                family = "poisson", link = "log", sp = sp.tb, data = pitchfx_swing_id_sub2)
	if (verbose == T) {
		print(summary(tb_gam))
		pdf(paste("baseball_graphics/total_bases_gam_", batter_name_use, ".pdf", sep=""), 
			width = 16, height = 10)
		tb_prob <- model_diagnostic_graphics(data = pitchfx_swing_id_sub2, 
			outcome = pitchfx_swing_id_sub2$hit_safe, model = tb_gam, count = count_use, plot_points = "N",
			surv_prob = 1, main_title = paste("Expected Total Bases Given", batter_name_use, 
				"Puts Ball In-Play", sep=" "))
		plot(tb_prob[[2]])
		tb_prob <- tb_prob[[1]]
		dev.off()
		
		pdf(paste("baseball_graphics/expect_total_bases_", batter_name_use, ".pdf", sep=""), 
			width = 16, height = 10)
		tb_prob2 <- model_diagnostic_graphics(data = pitchfx_id, outcome = pitchfx_id$hit_safe, 
			model = tb_gam, count = count_use, plot_points = "S", 
			surv_prob = swing_prob * (1 - whiff_prob) * (1-foul_prob), 
			main_title = paste("Expected Total Bases from Hitting Safely by", batter_name_use, sep=" "))
		plot(tb_prob2[[2]])
		dev.off()
		
		pdf(paste("baseball_graphics/expect_total_bases_given_swing_", batter_name_use, ".pdf", sep=""), 
			width = 16, height = 10)
		tb_prob3 <- model_diagnostic_graphics(data = pitchfx_id, outcome = pitchfx_id$hit_safe, 
			model = tb_gam, count = count_use, plot_points = "S", 
			surv_prob = (1 - whiff_prob) * (1 - foul_prob), 
			main_title = paste("Expected Total Bases from Hitting Safely Given", batter_name_use, 
				"Swings", sep=" "))
		plot(tb_prob3[[2]])
		dev.off()		
	}

	
	# Given the batter does not swing at pitch, estimate the probability pitch is called a strike
	# using GAM
	print("Fitting strike model")
	strike_gam <- gam(called_strike ~ s(start_speed, bs = "ts", k = 30) + as.factor(count) +
	                 te(px, pz, pfx_x, pfx_z, bs = "ts", d = c(2, 2), k = c(15, 15)), 
	                 family = "binomial", link = "logit", sp = sp.strike, data = pitchfx_noswing_id)
	if (verbose == T) {
		print(summary(strike_gam))
		pdf(paste("baseball_graphics/called_strike_gam_", batter_name_use, ".pdf", sep=""), 
			width = 16, height = 10)
		strike_prob <- model_diagnostic_graphics(data = pitchfx_noswing_id, 
			outcome = pitchfx_noswing_id$strike, 
			model = strike_gam, count = count_use, plot_points = "N", surv_prob = 1,
			main_title = paste("Probability of Pitch Called Strike Given", batter_name_use, 
				"Does Not Swing", sep=" "))
		plot(strike_prob[[2]])
		strike_prob <- strike_prob[[1]]
		dev.off()
		
		pdf(paste("baseball_graphics/prob_called_strike_", batter_name_use, ".pdf", sep=""), 
			width = 16, height = 10)
		strike_prob2 <- model_diagnostic_graphics(data = pitchfx_id, outcome = pitchfx_id$called_strike, 
			model = strike_gam, count = count_use, plot_points = "S", surv_prob = 1  - swing_prob, 
			main_title = paste("Probability of Taking Called Strike by ", batter_name_use, sep=" "))
		plot(strike_prob2[[2]])
		dev.off()
		
		pdf(paste("baseball_graphics/prob_called_strike_given_noswing_", batter_name_use, ".pdf", sep=""), 
			width = 16, height = 10)
		strike_prob3 <- model_diagnostic_graphics(data = pitchfx_id, outcome = pitchfx_id$called_strike, 
			model = strike_gam, count = count_use, plot_points = "S", surv_prob = rep(1,length(swing_prob)),
			main_title = paste("Probability of Pitch Called Strike Given", batter_name_use, 
				"Does Not Swing", sep=" "))
		plot(strike_prob3[[2]])
		dev.off()
		
		pdf(paste("baseball_graphics/prob_ball_", batter_name_use, ".pdf", sep=""), 
			width = 16, height = 10)
		ball_prob2 <- model_diagnostic_graphics(data = pitchfx_id, outcome = pitchfx_id$ball, 
			model = strike_gam, count = count_use, plot_points = "S", surv_prob = 1  - swing_prob, 
			complement = "Y",
			main_title = paste("Probability of Taking Ball by ", batter_name_use, sep=" "))
		plot(ball_prob2[[2]])
		dev.off()
		
		pdf(paste("baseball_graphics/prob_ball_given_noswing_", batter_name_use, ".pdf", sep=""), 
			width = 16, height = 10)
		ball_prob3 <- model_diagnostic_graphics(data = pitchfx_id, outcome = pitchfx_id$ball, 
			model = strike_gam, count = count_use, plot_points = "S", surv_prob = rep(1,length(swing_prob)),
			complement = "Y",
			main_title = paste("Probability of Pitch Called Ball Given", batter_name_use, 
				"Does Not Swing", sep=" "))
		plot(ball_prob3[[2]])
		dev.off()

	}
	
	# Model results
	model_results <- list(whiff_gam, foul_gam, hit_gam, tb_gam, swing_gam, strike_gam)
	names(model_results) <- c("whiff", "foul", "hit", "total_bases", "swing", "strike")
	return(model_results)
}