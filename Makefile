###################################
# File: ./Makefile
#   Purpose: Makefile to compile all R code, manuscript files, README markdown files
#   Author: David M. Vock
#   Last Modified: August 24, 2014
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
OUT_FILES:= $(RDIR)/outfile/data_generate.Rout $(RDIR)/outfile/README.Rout $(RDIR)/outfile/data_manipulate.Rout $(RDIR)/outfile/pitchfx_models.Rout $(RDIR)/outfile/pitchfx_models.Rout $(RDIR)/outfile/pitchfx_graphics.Rout $(RDIR)/outfile/counterfactual_outcomes.Rout

all: $(OUT_FILES)

# May need to add something here if some R files depend on others.
$(RDIR)/outfile/data_generate.Rout: $(RDIR)/data_generate.R 
	R CMD BATCH --slave --vanilla data_generate.R $(RDIR)/outfile/data_generate.Rout

$(RDIR)/outfile/README.Rout: $(RDIR)/README.R $(RDIR)/data_generate.R $(RDIR)/outfile/data_generate.Rout $(RDIR)/README.Rmd
	R CMD BATCH --slave --vanilla README.R $(RDIR)/outfile/README.Rout

$(RDIR)/outfile/data_manipulate.Rout: $(RDIR)/data_manipulate.R $(RDIR)/data_generate.R $(RDIR)/outfile/data_generate.Rout 
	R CMD BATCH --slave --vanilla data_manipulate.R $(RDIR)/outfile/data_manipulate.Rout

$(RDIR)/outfile/pitchfx_models.Rout: $(RDIR)/pitchfx_models.R $(RDIR)/functions/pitchfx_models_function.R $(RDIR)/functions/pitchfx_models_graphics.R $(RDIR)/data_manipulate.R $(RDIR)/data_generate.R $(RDIR)/outfile/data_manipulate.Rout 
	R CMD BATCH --slave --vanilla pitchfx_models.R $(RDIR)/outfile/pitchfx_models.Rout

$(RDIR)/outfile/pitchfx_graphics.Rout: $(RDIR)/pitchfx_graphics.R $(RDIR)/pitchfx_models.R $(RDIR)/functions/pitchfx_models_function.R $(RDIR)/functions/pitchfx_models_graphics.R $(RDIR)/data_manipulate.R $(RDIR)/data_generate.R $(RDIR)/outfile/pitchfx_models.Rout
	R CMD BATCH --slave --vanilla pitchfx_graphics.R $(RDIR)/outfile/pitchfx_graphics.Rout

$(RDIR)/outfile/counterfactual_outcomes.Rout: $(RDIR)/counterfactual_outcomes.R $(RDIR)/pitchfx_models.R $(RDIR)/functions/pitchfx_models_function.R $(RDIR)/functions/pitchfx_models_graphics.R $(RDIR)/data_manipulate.R $(RDIR)/data_generate.R $(RDIR)/outfile/pitchfx_models.Rout
	R CMD BATCH --slave --vanilla counterfactual_outcomes.R $(RDIR)/outfile/counterfactual_outcomes.Rout

# Clean up stray files
clean:
	rm -fv $(OUT_FILES) models.Rdata
