###################################
# File: ./calibrating_models.R
#   Purpose: File which generates Table 1 from the paper; observed BA, OBP & SLG for this data subset
#   Author: Laura Boehm Vock
#   Last Modified: March 1, 2016
#   Relies On: ./data_generate.R ./data_manipulate.R 
#   Files Created: NA
####################################

# Determine if the modesl are well calibrated.
require(dplyr)
require(xtable)

# Read-in data
pitchfx <- read.table("data/pitchfx_processed_3yr_nomiss.txt",sep="\t",header=T)
pitchfx <- as.data.frame(pitchfx)

qual.tabAM <- with(filter(pitchfx, batter==457705), table(start_tfs_zulu, event))
qual.tabSC <- with(filter(pitchfx, batter==516770), table(start_tfs_zulu, event))

eventsAM <- apply(qual.tabAM>0, 2, sum)
hitsAM <- sum(eventsAM[c("Double", "Home Run", "Single", "Triple")])
bbAM <- sum(eventsAM[c("Intent Walk", "Walk")])
outAM <- sum(eventsAM)-hitsAM-bbAM
basesAM <- eventsAM["Single"] + 2*eventsAM["Double"] + 3*eventsAM["Triple"] + 4*eventsAM["Home Run"]
paAM <- hitsAM+bbAM+outAM

eventsSC <- apply(qual.tabSC>0, 2, sum)
hitsSC <- sum(eventsSC[c("Double", "Home Run", "Single", "Triple")])
bbSC <- sum(eventsSC[c("Intent Walk", "Walk")])
outSC <- sum(eventsSC)-hitsSC-bbSC
basesSC <- eventsSC["Single"] + 2*eventsSC["Double"] + 3*eventsSC["Triple"] + 4*eventsSC["Home Run"]
paSC <- hitsSC + bbSC +outSC

calibrate.tab <- cbind(c(hitsAM/paAM, outAM/paAM, bbAM/paAM, hitsAM/(hitsAM+outAM),
	(hitsAM+bbAM)/(hitsAM+bbAM+outAM), basesAM/(hitsAM+outAM)),
	c(hitsSC/paSC, outSC/paSC, bbSC/paSC, hitsSC/(hitsSC+outSC), 
		(hitsSC+bbSC)/(hitsSC+bbSC+outSC), basesSC/(hitsSC+outSC)))

colnames(calibrate.tab)<-c("McCutchen", "Castro")
rownames(calibrate.tab) <- c("outcome.hit_safe", "outcome.out", "outcome.walk", 
                             "batting_average", "on_base_percentage", "slugging_percentage")

print(calibrate.tab)

paper.tab1 <- cbind(c(hitsSC + outSC, paSC, hitsSC/(hitsSC+outSC), 
	(hitsSC+bbSC)/(hitsSC+bbSC+outSC), basesSC/(hitsSC+outSC)), 
	c(hitsAM + outAM, paAM, hitsAM/(hitsAM+outAM), 
		(hitsAM+bbAM)/(hitsAM+bbAM+outAM), basesAM/(hitsAM+outAM)))

xtable(paper.tab1,3)

# Clear workspace
rm(list=ls())



