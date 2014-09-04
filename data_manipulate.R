###################################
# File: ./data_manipulate.R
#   Purpose: Process analysis dataset for use in models and graphics.
#   Author: David M. Vock
#   Last Modified: Augst 10, 2014
#   Relies On: ./data_generate.R 
#   Files Created: ./pitchfx_processed.txt 
####################################

# Libraries used
library(dplyr)

# read-in data
pitchfx <- read.table("data/pitchfx_sc_am.txt",sep="\t",header=T)
pitchfx <- as.data.frame(pitchfx)

# Data manipulation

# Add additional variables
pitchfx <- mutate(pitchfx, 
	# Add indicator variable for Starlin Castro
	starlin = 1*(batter == 516770),
	# redefine pitch types categories
	# combine two-seam fastballs and sinkers into one category
	# combine curveballs, knuckle curves, and screwball (n=1) into one category
	# combine intentional balls with four-seam fastballs
	pitch_type_alt1 = ifelse(pitch_type %in% c("FT", "FS", "SI"), "FT", 
		ifelse(pitch_type %in% c("CU", "KC", "SC"), "CU",
			ifelse(pitch_type == "SL", "SL",
				ifelse(pitch_type == "FF", "FF",
					ifelse(pitch_type == "FC", "FC",
						ifelse(pitch_type == "CH", "CH", "FF")))))),
	# indicator for swinging at pitch
	swing = ifelse((grepl("Swinging Strike", des) == T) | 
									(grepl("Foul", des) ==T) |
									(grepl("In play", des) ==T), 1, 0),
	# indicator for hitting safely
	hit_safe = ifelse(grepl("In play", des) == T &
			(grepl("Single", event) == T | 
					(grepl("Double", event) == T & grepl("Double Play", event) == F) |
					grepl("Triple", event) == T |
			 		grepl("Home Run", event)==T), 1, 0),
	# total bases of pitch
	total_bases = ifelse(grepl("Single", event) == T, 1,
								ifelse((grepl("Double", event) == T & grepl("Double Play", event) == F), 2,
									ifelse(grepl("Triple", event), 3,
										ifelse(grepl("Home Run", event), 4, 0)))),
	# indicator for fouling off pitch
	foul = ifelse(grepl("Foul", des) == T, 1, 0),
	# indicator for whiffing at pitch
	whiff = ifelse(grepl("Swinging Strike", des) == T, 1, 0),
	# indicator for called strike
	called_strike = ifelse(type=="S" & swing == 0, 1, 0),
	# indicator for pitch called ball
	ball = ifelse(type == "B" & swing == 0, 1, 0),
	# indicator for pitch hit inplay and out 
	inplay_out = ifelse(type == "X" & hit_safe == 0, 1, 0),
	# variable which gives the result of pitch 
	result = ifelse(inplay_out == 1, "inplay_out",
		ifelse(hit_safe == 1, "hit_safe",
			ifelse(whiff == 1, "whiff",
				ifelse(foul == 1, "foul", 
					ifelse(ball == 1, "ball",
						ifelse(called_strike == 1,"called_strike", "wrong")))))),
	# numeric variable for the number of strikes 
	strikecount = as.numeric(substr(count,3,3)),
	# numeric variable for the number of balls 
	ballcount = as.numeric(substr(count,1,1)),
	# indicator for two strike count
	strike2 = ifelse(strikecount == 2, 1, 0),
	ball3 = ifelse(ballcount == 3, 1, 0),
	fullcount = ifelse(strikecount == 2 & ballcount == 3, 1, 0)
	)

# subset to pitches only from right-handed pitchers
pitchfx <- filter(pitchfx, p_throws == "R")

# subset to exclude sacrifices and hit by pitch
pitchfx <- filter(pitchfx, grepl("Sac Bunt", event) == F & grepl("Sac Fly", event) == F &
                            grepl("Hit By Pitch", event) == F)

# Write data to .txt file
write.table(pitchfx,'data/pitchfx_processed.txt',sep="\t",col.names=T,row.names=F)

# Clear workspace
rm(list=ls())

