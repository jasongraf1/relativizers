##############################################################################
# .Rprofile for MCMCglmm RC analysis
# Jason Grafmiller
##############################################################################

# Packages --------------------------------------------------------------

silent.load <- function(a.package){ # load without all the messages, etc.
	suppressWarnings(suppressPackageStartupMessages(
		library(a.package, character.only = TRUE)))
}

auto.loads <- c("coda", "gdata", "plyr", "dplyr", "reshape2", "xtable", "gtools", # packages for data manipulation
	       "grid", "ggplot2", "gridExtra", "ggthemes", "scales", "GGally", # packages for graphics
	       "knitr", "pander", "captioner", # markdown packages
	       "FactoMineR", "MCMCglmm", "MCMCpack" # packages fro analysis
	       )

# load packages
invisible(sapply(auto.loads, silent.load))

# Markdown stuff --------------------------------------------------------
# for creating and referencing tables and figures 
figs <- captioner(prefix = "Figure")
tbls <- captioner(prefix = "Table")

# MCMCglmm supplement ---------------------------------------------------
source("R/MCMCglmmExtensions.R")

# ggplot2 theme ---------------------------------------------------------
# define custom ggplot2 theme
theme_jg <- theme_set(theme_bw())
# increase size of axis text 
# adjust strip title bg and text size
# make title larger boldface and v-justified
# move legend to bottom
theme_jg <- theme_update(
	axis.text = element_text(size = rel(0.9), color="black"), 
	plot.title = element_text(size = rel(1.25), 
				margin = margin(0, 0, 10, 0)), 
	strip.background = element_rect(fill = 'grey95'), 
	strip.text = element_text(size = rel(.9)),
	legend.position = 'bottom'
	)

# Misc ------------------------------------------------------------------

# add in the path for perl
perl <- "C:/strawberry/perl/bin/perl.exe"

# Get info for session: 
info <- utils::sessionInfo()
r_ver <- paste(info$R.version$major, info$R.version$minor, sep = ".")

##############################################################################
