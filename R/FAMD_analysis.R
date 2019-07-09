##############################################################################
#
# Factor Analysis of Mixed Data of English relativizers
# 
# Jason Grafmiller
##############################################################################

library(FactoMineR)


# Load data -------------------------------------------------------------

source("R/data_setup.R")

#--- 2 dataframes ---
# subjrel: the data from subject RCs
# objrel: the data from non=subject (object) RCs


# Functions -------------------------------------------------------------

# function for displaying
display.dims <- function(x) {
	for(j in 1:3){
		names(x) <- c("Conintuous variables", "Categorical variables",
				"Categories")
		cat(paste(names(x[j])), ":\n", sep = "")
		print(round(x[[j]], 3))
		cat('\n')
	}
}


# STYLISIC FACTORS ------------------------------------------------------

# Run two separate FAMDs for the stylistic features
# 1. subject FAMD
# 2 non-subject FAMD

# factors included:
style <- names(subjrel)[c(7, 25:29,31:32, 34:37)]

# calculate condition number of matrix
fmd.1.cnum <- languageR::collin.fnc(as.numeric2(subjrel, cols = style))$cnumber
# 108.6358

#=== subject RCs ===

fmd.1 <- FAMD(subjrel[, style], ncp = length(style))

#--- Eigenvalues ---

# Inspect the contributions of the variables and dimensions.
# Only use those dimensions that account for 5% of variance or more.
round(fmd.1$eig, 2)
round(fmd.1$var$contrib, 2)
round(fmd.1$var$cos2, 4)

#--- Correlations of the dimensions ---

# Use dimdesc function to inspect correlations of dimensions and original
# variables.
dd1 <- dimdesc(fmd.1, axes = 1:7)

dd1[[1]]
# + 
# heavy use of nouns, learned genre & press (somewhat)
# -
# use of pers pronouns and fiction
# informationally dense, 

dd1[[2]]
# +
# other markers of formality, e.g. sub conjs, passives and longer sentence length & also learned genre
# - 
# high TTR and nouniness, press genre
# dimension 2 is about elaborated, yet informationally sparse, style

# loop through and display all the dimensions
for(i in 1:7){
	d <- dd1[[i]]
	print(paste("Styistic Dimension", i))
	for(j in 1:3){
		print(round(d[[j]], 3))
	}
	cat('\n')
}

#=== object RCs =====================================

fmd.2 <- FAMD(objrel[, style], ncp = length(style))

# Calculate condition number of matrix.
fmd.2.cnum <- languageR::collin.fnc(as.numeric2(objrel, cols = style[-1]))$cnumber
# 130.0957

#--- Eigenvalues ---
fmd.2$eig
round(fmd.2$var$contrib, 2)
round(fmd.2$var$cos2, 4)

#--- Correlations of the dimensions ---

dd2 <- dimdesc(fmd.2, axes = 1:7)

# loop through and display all the dimensions
for(i in 1:7){
	d <- dd2[[i]]
	print(paste("Styistic Dimension", i))
	for(j in 1:3){
		print(round(d[[j]], 3))
	}
	cat('\n')
}


# INTERNAL FACTORS ------------------------------------------------------

names(subjrel) # same as objrel
# Fix names for transparency:
names(subjrel)[c(12, 19, 20, 22, 23, 42, 43, 18)] <- c(
	'prior.Rel', 'ant.Given', 'ant.Definite',
	'ant.to.RC.dist', 'RC.length',
	'ant.Number', 'ant.POS', 'ant.length')
names(objrel)[c(12, 19, 20, 22, 23, 42, 43, 18)] <- c(
	'prior.Rel', 'ant.Given', 'ant.Definite',
	'ant.to.RC.dist', 'RC.length',
	'ant.Number', 'ant.POS', 'ant.length')

internal <- names(subjrel)[c(22:23, 12, 14, 40, 42, 43, 18:20)]


#--- Relabel category levels for interpretation ---

# nestedness
levels(subjrel$nested) <- c("non-nested", "nested")
levels(objrel$nested) <- c("non-nested", "nested")

# givenness
levels(subjrel$ant.Given) <- c("new", "given")
levels(objrel$ant.Given) <- c("new", "given")

# persistence
levels(subjrel$prior.Rel) <- c('prior=none', 'prior=THAT',
															 'prior=WHICH', 'prior=ZERO')
levels(objrel$prior.Rel) <- c('prior=none', 'prior=THAT',
															 'prior=WHICH', 'prior=ZERO')

#=== subject RCs =====================================

fmd.3 <- FAMD(subjrel[, internal], ncp = length(internal))
summary(fmd.3)

# calculate condition number of matrix
fmd.3.cnum <- languageR::collin.fnc(as.numeric2(subjrel[, internal[c(1,2,8)]]))$cnumber


#--- Eigenvalues ---
fmd.3$eig # include all
round(fmd.3$var$contrib, 2)

#--- Correlations of the dimensions ---
# look at associations of the dimensions
dd3 <- dimdesc(fmd.3, axes = 1:9)

# loop through and display all the dimensions
for(i in 1:7){
	d <- dd3[[i]]
	print(paste("Styistic Dimension", i))
	for(j in 1:3){
		print(round(d[[j]], 3))
	}
	cat('\n')
	rm(d)
}


#=== object RCs =====================================

fmd.4 <- FAMD(objrel[, internal], ncp = length(internal))
summary(fmd.4)

# calculate condition number of matrix
fmd.4.cnum <- languageR::collin.fnc(as.numeric2(objrel[, internal]))$cnumber
# 44.27849

#--- Eigenvalues ---
fmd.4$eig # include all
round(fmd.4$var$contrib, 2)

#--- Correlations of the dimensions ---
dd4 <- dimdesc(fmd.4, axes = 1:9)

# loop through and display all:
for(i in 1:9){
	d <- dd4[[i]]
	print(paste("Internal Dimension", i))
	for(j in 1:3){
		print(round(d[[j]], 3))
	}
	cat('\n')
	rm(d)
}

# ADD TO DATAFRAMES -----------------------------------------------------

subj.style.dims <- fmd.1$ind$coord[, 1:7]
colnames(subj.style.dims) <- paste("Style", colnames(subj.style.dims), sep = '.')
subjrel <- cbind(subjrel, subj.style.dims)

obj.style.dims <- fmd.2$ind$coord[, 1:7]
colnames(obj.style.dims) <- paste("Style", colnames(obj.style.dims), sep = '.')
objrel <- cbind(objrel, obj.style.dims)

subj.int.dims <- fmd.3$ind$coord
colnames(subj.int.dims) <- paste("Int", colnames(subj.int.dims), sep = '.')
subjrel <- cbind(subjrel, subj.int.dims)

obj.int.dims <- fmd.4$ind$coord
colnames(obj.int.dims) <- paste("Int", colnames(obj.int.dims), sep = '.')
objrel <- cbind(objrel, obj.int.dims)


# ALL PREDCITORS COMBINED -----------------------------------------------

# Run two more FAMDs with all the predictors combined. This was not included
# in the Grafmiller et al. paper

n.vars <- length(c(style, internal)) 

#=== subject RCs =====================================

fmd.5 <- FAMD(subjrel[, c(style, internal)], ncp = n.vars)
summary(fmd.5)

#--- Eigenvalues ---
round(fmd.5$eig,3)
round(fmd.5$var$contrib, 2)

#--- Correlations of the dimensions ---
dd5 <- dimdesc(fmd.5, axes = 1:21)

#-- add to dataframe ---
subj.5.dims <- fmd.5$ind$coord[, 1:17]
subjrel <- cbind(subjrel, subj.5.dims)

#=== object RCs =====================================

fmd.6 <- FAMD(objrel[, c(style, internal)], ncp = n.vars)

#--- Eigenvalues ---
round(fmd.6$eig,3)
round(fmd.6$var$contrib,3)

#--- Eigenvalues ---
dd6 <- dimdesc(fmd.6, axes = 1:21)


#--- add to dataframe ----
obj.5.dims <- fmd.6$ind$coord[, 1:17]
objrel <- cbind(objrel, obj.5.dims)


# CREATE GRAPHICS -------------------------------------------------------

# Code for creating tables of associations for a given dimension with 
# knitr. Tables are uneven, so they are cumbersome to create automatically
# with markdown, pandoc, etc. I use tableGrobs in the gridExtra package to 
# create table graphics.

# function for vertically justifying the three grobs
justify <- function(x, hjust="center", vjust="top", draw = FALSE){
	w <- sum(x$widths)
	h <- sum(x$heights)
	xj <- switch(hjust, center = 0.5,
						left = 0.5 * w,
						right=unit(1,"npc") - 0.5 * w)
	yj <- switch(vjust, center = 0.5,
						bottom = 0.5*h,
						top=unit(1,"npc") - 0.5*h)
	x$vp <- viewport(x=xj, y=yj)
	if(draw) grid.draw(x)
	return(x)
}

# Make the tables of the significant variables from a dimdesc object,
famd.table <- function(dd, widths = c(.5,.2,.5)){
  # dd is one dimension from a dimdesc() object
	# Make the matrices for the tables (without p values).
  est1 <- as.matrix(data.frame(correlation = dd[[1]][, 1]))
	rownames(est1) <- rownames(dd[[1]])
	est2 <- as.matrix(data.frame(R2 = dd[[2]][, 1]))
	rownames(est2) <- rownames(dd[[2]])
	est3 <- as.matrix(data.frame(Estimate = dd[[3]][, 1]))
	rownames(est3) <- rownames(dd[[3]])
  
  # make tableGrobs
  a <- justify(tableGrob(round(est1, 3), theme = ttheme_minimal(), widths = unit(c(1), "mm")))
  b <- justify(tableGrob(round(est2, 3), theme = ttheme_minimal(), widths = unit(c(2), "cm")))
  d <- justify(tableGrob(round(est3, 3), theme = ttheme_minimal(), widths = unit(c(1), "cm")))
	# plot tables
	grid.arrange(a, b, d, nrow = 1, widths = widths)
}

# For the stylistic factors:
famd.table(dd1[[1]])
famd.table(dd1[[2]])

famd.table(dd2[[1]])
famd.table(dd2[[2]])
# and so on...

# For the internal factors:
famd.table(dd3[[1]])
famd.table(dd3[[2]])

famd.table(dd4[[1]])
famd.table(dd4[[2]])
# and so on...


# --- Scree plots ---

# Create the scree plots for each FAMD.

## stylistic factors

sty_src <- fmd.1$eig 
sty_src$dim <- factor(1:12, levels = 1:12)
names(sty_src)[2:3] <- c("percent", "cumulative") 
sty_nsrc <- fmd.2$eig
sty_nsrc$dim <- factor(1:12, levels = 1:12)
names(sty_nsrc)[2:3] <- c("percent", "cumulative") 

sty_src_p <- ggplot(sty_src, aes(dim, percent)) +
	geom_bar(stat = "identity", width = .7, fill = "steelblue2") +
	geom_point(size = rel(1.8)) + geom_line(aes(x = 1:12)) +
	labs(y = "percentage of variance", x = "dimension", title = "SRC data")

sty_nsrc_p <- ggplot(sty_nsrc, aes(dim, percent)) +
	geom_bar(stat = "identity", width = .7, fill = "steelblue2") +
	geom_point(size = rel(1.8)) + geom_line(aes(x = 1:12)) +
	labs(y = "", x = "dimension", title = "NSRC data")

# grid.arrange(sty_src_p, sty_nsrc_p, ncol = 2)

## internal factors

int_src <- fmd.3$eig 
int_src$dim <- factor(1:9, levels = 1:9)
names(int_src)[2:3] <- c("percent", "cumulative") 
int_nsrc <- fmd.4$eig
int_nsrc$dim <- factor(1:9, levels = 1:9)
names(int_nsrc)[2:3] <- c("percent", "cumulative") 

int_src_p <- ggplot(int_src, aes(dim, percent)) +
	geom_bar(stat = "identity", width = .7, fill = "steelblue2") +
	geom_point(size = rel(1.8)) + geom_line(aes(x = 1:9)) +
	labs(y = "percentage of variance", x = "dimension", title = "SRC data")

int_nsrc_p <- ggplot(int_nsrc, aes(dim, percent)) +
	geom_bar(stat = "identity", width = .7, fill = "steelblue2") +
	geom_point(size = rel(1.8)) + geom_line(aes(x = 1:9)) +
	labs(y = "", x = "dimension", title = "NSRC data")

# grid.arrange(int_src_p, int_nsrc_p, ncol = 2)

##############################################################################
