###################################
# File: ./Makefile
#   Purpose: Makefile to compile all R code, manuscript files, README markdown files
#   Author: David M. Vock
#   Last Modified: March 5, 2016
#   Relies On: NA 
#   Files Created: NA 
####################################

# Usually, only these lines need changing
TEXFILE= paper
RDIR= .
FIGDIR= ./figs

# list R files
# RFILES := $(wildcard $(RDIR)/*.R)
# Indicator files to show R file has run
OUT_FILES:= $(RDIR)/outfile/data_generate_3yr.Rout $(RDIR)/outfile/README.Rout $(RDIR)/outfile/data_manipulate_3yr.Rout $(RDIR)/outfile/pitchfx_models_3yr.Rout $(RDIR)/outfile/pitchfx_models_3yr.Rout $(RDIR)/outfile/manuscript_graphics.Rout $(RDIR)/outfile/counterfactual_outcomes_3yr.Rout $(RDIR)/outfile/calibrating_models.Rout

all: $(OUT_FILES)

# May need to add something here if some R files depend on others.
$(RDIR)/outfile/data_generate_3yr.Rout: $(RDIR)/data_generate_3yr.R 
	R CMD BATCH --slave --vanilla data_generate_3yr.R $(RDIR)/outfile/data_generate_3yr.Rout

$(RDIR)/outfile/README.Rout: $(RDIR)/README.R $(RDIR)/data_generate_3yr.R $(RDIR)/outfile/data_generate_3yr.Rout $(RDIR)/README.Rmd
	R CMD BATCH --slave --vanilla README.R $(RDIR)/outfile/README.Rout

$(RDIR)/outfile/data_manipulate_3yr.Rout: $(RDIR)/data_manipulate_3yr.R $(RDIR)/data_generate_3yr.R $(RDIR)/outfile/data_generate_3yr.Rout 
	R CMD BATCH --slave --vanilla data_manipulate_3yr.R $(RDIR)/outfile/data_manipulate_3yr.Rout

$(RDIR)/outfile/pitchfx_models_3yr.Rout: $(RDIR)/pitchfx_models_3yr.R $(RDIR)/functions/pitchfx_models_3yr_function.R $(RDIR)/functions/pitchfx_models_graphics.R $(RDIR)/data_manipulate_3yr.R $(RDIR)/data_generate_3yr.R $(RDIR)/outfile/data_manipulate_3yr.Rout 
	R CMD BATCH --slave --vanilla pitchfx_models_3yr.R $(RDIR)/outfile/pitchfx_models_3yr.Rout

$(RDIR)/outfile/manuscript_graphics.Rout: $(RDIR)/manuscript_graphics.R $(RDIR)/pitchfx_models_3yr.R $(RDIR)/functions/pitchfx_models_3yr_function.R $(RDIR)/functions/pitchfx_models_graphics.R $(RDIR)/data_manipulate_3yr.R $(RDIR)/data_generate_3yr.R $(RDIR)/outfile/pitchfx_models_3yr.Rout
	R CMD BATCH --slave --vanilla manuscript_graphics.R $(RDIR)/outfile/manuscript_graphics.Rout

$(RDIR)/outfile/calibrating_models.Rout: $(RDIR)/calibrating_models.R $(RDIR)/data_manipulate_3yr.R $(RDIR)/data_generate_3yr.R 
	R CMD BATCH --slave --vanilla calibrating_models.R $(RDIR)/outfile/calibrating_models.Rout

$(RDIR)/outfile/counterfactual_outcomes_3yr.Rout: $(RDIR)/counterfactual_outcomes_3yr.R $(RDIR)/pitchfx_models_3yr.R $(RDIR)/functions/pitchfx_models_3yr_function.R $(RDIR)/functions/pitchfx_models_graphics.R $(RDIR)/data_manipulate_3yr.R $(RDIR)/data_generate_3yr.R $(RDIR)/outfile/pitchfx_models_3yr.Rout
	R CMD BATCH --slave --vanilla counterfactual_outcomes_3yr.R $(RDIR)/outfile/counterfactual_outcomes_3yr.Rout

# Clean up stray files
clean:
	rm -fv $(OUT_FILES) models.Rdata
