###################################
# File: ./data_generate_3yr.R
#   Purpose: Extract the pitchf/x data needed for analysis from online MLB Gameday databases.
#   Author: David M. Vock
#   Last Modified: August 20, 2015
#   Relies On: NA 
#   Files Created: ./data/pitchfx_sc_am_3yr.txt 
####################################

# libraries used
library(pitchRx)
library(plyr)
library(dplyr)

# Obtain game ids for 2013 Chicago Cubs and Pitsburgh Pirates
gids_2012 <- grepl("2012", gids)
gids_2013 <- grepl("2013", gids)
gids_2014 <- grepl("2014", gids)
gids_chn <- grepl("chn", gids)
gids_pit <- grepl("pit", gids)

# Obtain game ids for 2012 Chicago Cubs
gids_2012_chn_use <- gids[(gids_2012 == TRUE) & (gids_chn == TRUE)]
# Find spring training games
gids_2012_chn_st <- c( grep("2012_02", gids_2012_chn_use), grep("2012_03", gids_2012_chn_use),
  grep("2012_04_01", gids_2012_chn_use), grep("2012_04_02", gids_2012_chn_use), 
  grep("2012_04_03", gids_2012_chn_use))    
# Postponed game ids (found manually)
gids_2012_chn_ppd <- c(grep("2012_05_01_chnmlb_cinmlb", gids_2012_chn_use))    
# Exclude spring training, post-season, ppd
gids_2012_chn_use <- gids_2012_chn_use[-c(gids_2012_chn_st, gids_2012_chn_ppd)]  

# Obtain pitch information and at-bat for each of the game ids identified above
# Limit data to Starlin Castro
xmlGame_2012_chn  <- scrape(game.ids=gids_2012_chn_use)
pitch_2012_chn <- as.data.frame(xmlGame_2012_chn$pitch)
pitch_2012_chn <- select(pitch_2012_chn, -des_es, -tfs, -tfs_zulu, -x, -y, -x0, -y0, -z0, -next_,
  -cc, -mt, -on_1b, -on_2b, -on_3b, -url)
atbat_2012_chn <- as.data.frame(xmlGame_2012_chn$atbat)
atbat_2012_chn <- select(atbat_2012_chn, -atbat_des_es, -next_, -start_tfs, -event2,
  -event3, -score, -home_team_runs, -away_team_runs, -url, -date, -b, -s, -o, -next_)  
pitchfx_2012_chn <- plyr::join(pitch_2012_chn, atbat_2012_chn, by = c("inning", "inning_side", 
  "gameday_link", "num"), type = "inner")
pitchfx_2012_sc <- filter(pitchfx_2012_chn, batter == 516770)

# 2013 Chicago Cubs
gids_2013_chn_use <- gids[(gids_2013 == TRUE) & (gids_chn == TRUE)]
# Find spring training games 
gids_2013_chn_st <- c( grep("2013_02", gids_2013_chn_use), grep("2013_03", gids_2013_chn_use))  
# Find post-season games
gids_2013_chn_post <- c(grep("2013_10", gids_2013_chn_use))  
# Postponed game ids (found manually) 
gids_2013_chn_ppd <- c(grep("2013_04_10_milmlb_chnmlb", gids_2013_chn_use),
  grep("2013_04_17_texmlb_chnmlb", gids_2013_chn_use),
  grep("2013_05_28_chnmlb_chamlb", gids_2013_chn_use))   
# Exclude spring training, post-season, ppd
gids_2013_chn_use <- gids_2013_chn_use[-c(gids_2013_chn_st, gids_2013_chn_post, gids_2013_chn_ppd)]  

# Obtain pitch information and at-bat for each of the game ids identified above
# Limit data to Starlin Castro
xmlGame_2013_chn  <- scrape(game.ids=gids_2013_chn_use)
pitch_2013_chn <- as.data.frame(xmlGame_2013_chn$pitch)
pitch_2013_chn <- select(pitch_2013_chn, -des_es, -tfs, -tfs_zulu, -x, -y, -x0, -y0, -z0, -next_,
  -cc, -mt, -on_1b, -on_2b, -on_3b, -url, -play_guid, -event_num)
atbat_2013_chn <- as.data.frame(xmlGame_2013_chn$atbat)
atbat_2013_chn <- select(atbat_2013_chn, -atbat_des_es, -next_, -start_tfs, -event2,
  -event3, -event_es, -event2_es, -event3_es, -score, -home_team_runs, -away_team_runs, -url, 
	-date, -b, -s, -o, -next_, -event_num)  
pitchfx_2013_chn <- plyr::join(pitch_2013_chn, atbat_2013_chn, by = c("inning", "inning_side", 
  "gameday_link", "num"), type = "inner")
pitchfx_2013_sc <- filter(pitchfx_2013_chn, batter == 516770)

# 2014 Chicago Cubs
gids_2014_chn_use <- gids[(gids_2014 == TRUE) & (gids_chn == TRUE)]
# Find spring training games 
gids_2014_chn_st <- c( grep("2014_02", gids_2014_chn_use), grep("2014_03", gids_2014_chn_use))  
gids_2014_chn_st <- gids_2014_chn_st[-length(gids_2014_chn_st)] 
# Find post-season games
gids_2014_chn_post <- c(grep("2014_10", gids_2014_chn_use))  
# Postponed game ids (found manually) 
gids_2014_chn_ppd <- c(grep("2014_04_15_chnmlb_nyamlb", gids_2014_chn_use),
  grep("2014_04_28_chnmlb_cinmlb", gids_2014_chn_use),
  grep("2014_05_14_chnmlb_slnmlb", gids_2014_chn_use))   
# Double Header game ids (found manually)
gids_2014_chn_db <- c("gid_2014_04_16_chnmlb_nyamlb_2",
  "gid_2014_07_08_chnmlb_cinmlb_2",
  "gid_2014_08_30_chnmlb_stlmlb_2")
# Exclude spring training, post-season, ppd
gids_2014_chn_use <- c(gids_2014_chn_use[-c(gids_2014_chn_st, gids_2014_chn_post, 
  gids_2014_chn_ppd)], gids_2014_chn_db)  

# Obtain pitch information and at-bat for each of the game ids identified above
# Limit data to Starlin Castro
xmlGame_2014_chn  <- scrape(game.ids=gids_2014_chn_use)
pitch_2014_chn <- as.data.frame(xmlGame_2014_chn$pitch)
pitch_2014_chn <- select(pitch_2014_chn, -des_es, -tfs, -tfs_zulu, -x, -y, -x0, -y0, -z0, -next_,
  -cc, -mt, -on_1b, -on_2b, -on_3b, -url, -play_guid, -event_num)
atbat_2014_chn <- as.data.frame(xmlGame_2014_chn$atbat)
atbat_2014_chn <- select(atbat_2014_chn, -atbat_des_es, -next_, -start_tfs, -event2,
  -event3, -event_es, -event2_es, -event3_es, -score, -home_team_runs, -away_team_runs, -url, 
	-date, -b, -s, -o, -next_, -event_num)  
pitchfx_2014_chn <- plyr::join(pitch_2014_chn, atbat_2014_chn, by = c("inning", "inning_side", 
  "gameday_link", "num"), type = "inner")
pitchfx_2014_sc <- filter(pitchfx_2014_chn, batter == 516770)

# Obtain game ids for 2012 Pitsburgh Pirates
gids_2012_pit_use <- gids[(gids_2012 == TRUE) & (gids_pit == TRUE)]
# Find spring training games
gids_2012_pit_st <- c( grep("2012_02", gids_2012_pit_use), grep("2012_03", gids_2012_pit_use),
  grep("2012_04_01", gids_2012_pit_use), grep("2012_04_02", gids_2012_pit_use), 
  grep("2012_04_03", gids_2012_pit_use))    
# Postponed game ids (found manually)
gids_2012_pit_ppd <- c(grep("2012_04_23_colmlb_pitmlb", gids_2012_pit_use))    
# Exclude spring training, post-season, ppd
gids_2012_pit_use <- gids_2012_pit_use[-c(gids_2012_pit_st, gids_2012_pit_ppd)]  

# Obtain pitch information and at-bat for each of the game ids identified above
# Limit data to Andrew McCutchen
xmlGame_2012_pit  <- scrape(game.ids=gids_2012_pit_use)
pitch_2012_pit <- as.data.frame(xmlGame_2012_pit$pitch)
pitch_2012_pit <- select(pitch_2012_pit, -des_es, -tfs, -tfs_zulu, -x, -y, -x0, -y0, -z0, -next_,
  -cc, -mt, -on_1b, -on_2b, -on_3b, -url)
atbat_2012_pit <- as.data.frame(xmlGame_2012_pit$atbat)
atbat_2012_pit <- select(atbat_2012_pit, -atbat_des_es, -next_, -start_tfs, -event2,
  -event3, -score, -home_team_runs, -away_team_runs, -url, 
	-date, -b, -s, -o, -next_)  
pitchfx_2012_pit <- plyr::join(pitch_2012_pit, atbat_2012_pit, by = c("inning","inning_side",
  "gameday_link", "num"), type = "inner")
pitchfx_2012_am <- filter(pitchfx_2012_pit, batter == 457705)


# Obtain game ids for 2013 Pitsburgh Pirates
gids_2013_pit_use <- gids[(gids_2013 == TRUE) & (gids_pit == TRUE)]
# Find spring training games
gids_2013_pit_st <- c( grep("2013_02", gids_2013_pit_use), grep("2013_03", gids_2013_pit_use))   
# Find post-season games
gids_2013_pit_post <- c(grep("2013_10", gids_2013_pit_use))  
# Postponed game ids (found manually)
gids_2013_pit_ppd <- c(grep("2013_04_16_slnmlb_pitmlb", gids_2013_pit_use))    
# Exclude spring training, post-season, ppd
gids_2013_pit_use <- gids_2013_pit_use[-c(gids_2013_pit_st, gids_2013_pit_post, gids_2013_pit_ppd)]  

# Obtain pitch information and at-bat for each of the game ids identified above
# Limit data to Andrew McCutchen
xmlGame_2013_pit  <- scrape(game.ids=gids_2013_pit_use)
pitch_2013_pit <- as.data.frame(xmlGame_2013_pit$pitch)
pitch_2013_pit <- select(pitch_2013_pit, -des_es, -tfs, -tfs_zulu, -x, -y, -x0, -y0, -z0, -next_,
  -cc, -mt, -on_1b, -on_2b, -on_3b, -url, -play_guid, -event_num)
atbat_2013_pit <- as.data.frame(xmlGame_2013_pit$atbat)
atbat_2013_pit <- select(atbat_2013_pit, -atbat_des_es, -next_, -start_tfs, -event2,
  -event3, -event_es, -event2_es, -event3_es, -score, -home_team_runs, -away_team_runs, -url, 
	-date, -b, -s, -o, -next_, -event_num)  
pitchfx_2013_pit <- plyr::join(pitch_2013_pit, atbat_2013_pit, by = c("inning","inning_side",
  "gameday_link", "num"), type = "inner")
pitchfx_2013_am <- filter(pitchfx_2013_pit, batter == 457705)

# Obtain game ids for 2014 Pitsburgh Pirates
gids_2014_pit_use <- gids[(gids_2014 == TRUE) & (gids_pit == TRUE)]
# Find spring training games
gids_2014_pit_st <- c( grep("2014_02", gids_2014_pit_use), grep("2014_03", gids_2014_pit_use)) 
gids_2014_pit_st <- gids_2014_pit_st[-length(gids_2014_pit_st)] 
# Find post-season games
gids_2014_pit_post <- c(grep("2014_10", gids_2014_pit_use))  
# Postponed game ids (found manually)
gids_2014_pit_ppd <- c(grep("2014_04_29_pitmlb_balmlb", gids_2014_pit_use),
  grep("2014_04_30_pitmlb_balmlb", gids_2014_pit_use),
  grep("2014_05_16_pitmlb_nyamlb", gids_2014_pit_use)) 
# Double Header game ids (found manually)
gids_2014_pit_db <- c("gid_2014_05_01_pitmlb_balmlb_1", "gid_2014_05_01_pitmlb_balmlb_2",
  "gid_2014_05_18_pitmlb_nyamlb_2")
# Exclude spring training, post-season, ppd
gids_2014_pit_use <- c(gids_2014_pit_use[-c(gids_2014_pit_st, gids_2014_pit_post,
	gids_2014_pit_ppd)], gids_2014_pit_db)  

# Obtain pitch information and at-bat for each of the game ids identified above
# Limit data to Andrew McCutchen
xmlGame_2014_pit  <- scrape(game.ids=gids_2014_pit_use)
pitch_2014_pit <- as.data.frame(xmlGame_2014_pit$pitch)
pitch_2014_pit <- select(pitch_2014_pit, -des_es, -tfs, -tfs_zulu, -x, -y, -x0, -y0, -z0, -next_,
  -cc, -mt, -on_1b, -on_2b, -on_3b, -url, -play_guid, -event_num)
atbat_2014_pit <- as.data.frame(xmlGame_2014_pit$atbat)
atbat_2014_pit <- select(atbat_2014_pit, -atbat_des_es, -next_, -start_tfs, -event2,
  -event3, -event_es, -event2_es, -event3_es, -score, -home_team_runs, -away_team_runs, -url, 
	-date, -b, -s, -o, -next_, -event_num)  
pitchfx_2014_pit <- plyr::join(pitch_2014_pit, atbat_2014_pit, by = c("inning","inning_side",
  "gameday_link", "num"), type = "inner")
pitchfx_2014_am <- filter(pitchfx_2014_pit, batter == 457705)

# Combine data together
pitchfx <- rbind(pitchfx_2012_sc, pitchfx_2013_sc, pitchfx_2014_sc,
  pitchfx_2012_am, pitchfx_2013_am, pitchfx_2014_am)
pitchfx$break_y <- as.numeric(pitchfx$break_y)
pitchfx$break_angle <- as.numeric(pitchfx$break_angle)
pitchfx$break_length <- as.numeric(pitchfx$break_length)
pitchfx <- arrange(pitchfx, batter, sv_id, num, id)

# Write data to .txt file
write.table(pitchfx,'data/pitchfx_sc_am_3yr.txt',sep="\t",col.names=T,row.names=F)

# Clear workspace
rm(list=ls())
