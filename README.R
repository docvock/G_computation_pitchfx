###################################
# File: ./README.R
#   Purpose: Code needed to compile README.Rmd into markdown file
#   Author: David M. Vock
#   Last Modified: March 5, 2016
#   Relies On: ./README.Rmd ./data_generate.R 
#   Files Created: ./README.md 
####################################

library(knitr)
knit('README.Rmd')

