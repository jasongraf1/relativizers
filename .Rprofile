##############################################################################
# .Rprofile for MCMCglmm RC analysis
# Jason Grafmiller
##############################################################################

# Packages --------------------------------------------------------------
# taken in part from my standard .Rprofile

silent.load <- function(a.package){ # load without all the messages, etc.
	suppressWarnings(suppressPackageStartupMessages(
		library(a.package, character.only = TRUE)))
}

# packages for data manipulation
auto.loads <- c("coda", "gdata", "plyr", "dplyr", "reshape2", "xtable", "gtools")

# packages for graphics
auto.loads <- c(auto.loads, c("grid", "ggplot2", "gridExtra", "ggthemes", 
		"scales", "GGally"))

# some custom packages
auto.loads <- c(auto.loads, c("languageR"))

# markdown packages
auto.loads <- c(auto.loads, c("knitr", "pander", "captioner"))

# add necessary packages to auto.load
auto.loads <- c(auto.loads, "FactoMineR", "MCMCglmm", "MCMCpack")

# load packages
invisible(sapply(auto.loads, silent.load))

# Markdown stuff --------------------------------------------------------

# for creating and referencing tables and figures 
figs <- captioner(prefix = "Figure")
tbls <- captioner(prefix = "Table")

# general supplement ----------------------------------------------------
source("R/generalExtensions.R")
source("R/statsExtensions.R")

# MCMCglmm supplement ---------------------------------------------------
source("R/MCMCglmmExtensions.R")

# ggplot2 supplement ----------------------------------------------------
source("R/ggExtensions.R")

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

#scale_fill_discrete <- function(...) scale_fill_grey(...)

# misc ------------------------------------------------------------------

# add in the path for perl
perl <- "C:/strawberry/perl/bin/perl.exe"

# Get info for session: 
info <- utils::sessionInfo()
r_ver <- paste(info$R.version$major, info$R.version$minor, sep = ".")

##############################################################################
