###################################
# File: ./data_generate.R
#   Purpose: Extract the pitchf/x data needed for analysis from online MLB Gameday databases.
#   Author: David M. Vock
#   Last Modified: July 31, 2014
#   Relies On: NA 
#   Files Created: ./data/pitchfx.txt 
####################################

# libraries used
library(pitchRx)
library(plyr)
library(dplyr)

# Load in 2013 data for Starlin Castro 

# Obtain game ids for 2013 Chicago Cubs and Pitsburgh Pirates
gids_2013 <- grepl("2013", gids)
gids_chn <- grepl("chn", gids)
gids_use <- gids[gids_2013 & gids_chn == T]
gids_st <- c( grep("2013_02", gids_use), grep("2013_03", gids_use))  # Find spring training games 
gids_post <- c(grep("2013_10", gids_use))  # Find post-season games
gids_ppd <- c(grep("2013_04_10_milmlb_chnmlb", gids_use),
              grep("2013_04_17_texmlb_chnmlb", gids_use),
              grep("2013_05_28_chnmlb_chamlb", gids_use))  # Postponed game ids (found manually)  
gids_use <- gids_use[-c(gids_st, gids_post, gids_ppd)]  # Exclude spring training, post-season, ppd

# Obtain pitch information and at-bat for each of the game ids identified above
# Limit data to Starlin Castro
xmlGame_chc  <- scrape(game.ids=gids_use)
pitch_chc <- as.data.frame(xmlGame_chc$pitch)
pitch_chc <- select(pitch_chc, -des_es, -tfs, -tfs_zulu, -x, -y, -x0, -y0, -z0, -next_, -cc, -mt, 
                    -on_1b, -on_2b, -on_3b, -url)
atbat_chc <- as.data.frame(xmlGame_chc$atbat)
atbat_chc <- select(atbat_chc, -atbat_des, -atbat_des_es, -next_, -start_tfs, -event2, -event3, 
                    -score, -home_team_runs, -away_team_runs, -url, -date, -b, -s, -o, -next_)	
pitchfx_chc <- plyr::join(pitch_chc, atbat_chc, by = c("inning","inning_side","gameday_link", "num"), type = "inner")
pitchfx_sc <- filter(pitchfx_chc, batter == 516770)

# Load in 2013 data for Andrew McCutchen

# Obtain game ids for 2013 Pitsburgh Pirates
gids_2013 <- grepl("2013", gids)
gids_pit <- grepl("pit", gids)
gids_use <- gids[gids_2013 & gids_pit == T]
gids_st <- c( grep("2013_02", gids_use), grep("2013_03", gids_use))  # Find spring training games 
gids_post <- c(grep("2013_10", gids_use))  # Find post-season games
gids_ppd <- c(grep("2013_04_16_slnmlb_pitmlb", gids_use))  # Postponed game ids (found manually)  
gids_use <- gids_use[-c(gids_st, gids_post, gids_ppd)]  # Exclude spring training, post-season, ppd

# Obtain pitch information and at-bat for each of the game ids identified above
# Limit data to Andrew McCutchen
xmlGame_pit  <- scrape(game.ids=gids_use)
pitch_pit <- as.data.frame(xmlGame_pit$pitch)
pitch_pit <- select(pitch_pit, -des_es, -tfs, -tfs_zulu, -x, -y, -x0, -y0, -z0, -next_, -cc, -mt, 
                    -on_1b, -on_2b, -on_3b, -url)
atbat_pit <- as.data.frame(xmlGame_pit$atbat)
atbat_pit <- select(atbat_pit, -atbat_des, -atbat_des_es, -next_, -start_tfs, -event2, -event3, 
                    -score, -home_team_runs, -away_team_runs, -url, -date, -b, -s, -o, -next_)	
pitchfx_pit <- plyr::join(pitch_pit, atbat_pit, by = c("inning","inning_side","gameday_link", "num"), type = "inner")
pitchfx_am <- filter(pitchfx_pit, batter == 457705)

# Combine data together
pitchfx <- rbind(pitchfx_sc, pitchfx_am)
pitchfx$break_y <- as.numeric(pitchfx$break_y)
pitchfx$break_angle <- as.numeric(pitchfx$break_angle)
pitchfx$break_length <- as.numeric(pitchfx$break_length)
pitchfx <- arrange(pitchfx, batter, sv_id, num, id)

# Write data to .txt file
write.table(pitchfx,'data/pitchfx_sc_am.txt',sep="\t",col.names=T,row.names=F)

# Clear workspace
rm(list=ls())

