###################################
# File: ./bootstrap.R
#   Purpose: File with function to implement bootstrap 
#   Author: David M. Vock
#   Last Modified: Augst 1, 2014
#   Relies On: NA
#   Files Created: NA
####################################


# B=number of bootstrap samples.

bootstrap.se <- function(pitchfx = pitchfx, B, seed){

  set.seed(seed)
  num.SC <- sum(pitchfx$starlin) 
  num.AM <- nrow(pitchfx) - num.SC 
  
  index.SC <- matrix(sample(1:num.SC, num.SC*B, replace=T), num.SC, B)
  index.AM <- matrix(sample(1:num.AM, num.AM*B, replace=T), num.AM, B)

  OUT<- array(NA, dim = c(6, 16, B))
  rownames(OUT) <- c("outcome.hit_safe", "outcome.out", "outcome.walk", "batting_average", 
  										"on_base_percentage", "slugging_percentage") 
  
  mccutchen_entries <- which(pitchfx$starlin == 0)
  castro_entries <- which(pitchfx$starlin == 1)
  
  
  for (i in 1:B){
  	print(paste("Starting Bootstrap Iteration",i,sep=" "))
  	pitchfx_i <- pitchfx[c(mccutchen_entries[index.AM[, i]], castro_entries[index.SC[, i]]), ]
  	castro_models_i <- gam_models(pitchfx_i, batter_id = 516770, verbose = F, 
  		sp.swing = (castro_models$swing)$sp,
  		sp.whiff = (castro_models$whiff)$sp,
  		sp.foul = (castro_models$foul)$sp,
  		sp.hit = (castro_models$hit)$sp,
  		sp.tb = (castro_models$total_bases)$sp,
  		sp.strike = (castro_models$strike)$sp)
		mccutchen_models_i <- gam_models(pitchfx_i, batter_id = 457705, verbose = F,
			sp.swing = (mccutchen_models$swing)$sp,
  		sp.whiff = (mccutchen_models$whiff)$sp,
  		sp.foul = (mccutchen_models$foul)$sp,
  		sp.hit = (mccutchen_models$hit)$sp,
  		sp.tb = (mccutchen_models$total_bases)$sp,
  		sp.strike = (mccutchen_models$strike)$sp) 
  	
  	model_outcomes(hitting_ability="Castro", plate_discipline="Castro", pitch_selection="Castro", 
               strikezone="Castro", mccutchen_models = mccutchen_models_i, 
               castro_models = castro_models_i, pitchfx = pitchfx_i) 

OUT.ITER <- NULL
for (j in c("Castro", "McCutchen")) {
for (k in c("Castro", "McCutchen")) {
for (l in c("Castro", "McCutchen")) {
for (m in c("Castro", "McCutchen")) {
    
    OUT.ITER <- cbind(OUT.ITER,
    		unlist(model_outcomes(hitting_ability = j, plate_discipline = k, pitch_selection = l, 
    			strikezone = m, mccutchen_models = mccutchen_models_i, castro_models = castro_models_i, 
    			pitchfx = pitchfx_i)))
}}}}
    OUT[, , i] <- OUT.ITER
    

}
  return(OUT)
}