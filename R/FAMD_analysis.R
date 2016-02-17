##############################################################################
# Factor Analysis of Mixed Data of English relativizers 
# Jason Grafmiller
##############################################################################

library(FactoMineR)

# DATA ------------------------------------------------------------------

source("R/data_setup.R")

#--- 2 dataframes ---
# subjrel: the data from subject RCs
# objrel: the data from non=subject (object) RCs

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

# include the "stylistic" factors as well as the prescriptive factors 
names(subjrel)

# factors included:
style <- names(subjrel)[c(7, 25:29,31:32, 34:37)]

#=== subject RCs =====================================

fmd.1 <- FAMD(subjrel[, style], ncp = length(style))

summary(fmd.1)

#--- Eigenvalues ---

# include Dims 1-7. Only use those dimensions that account for 5% of variance 
# or more
round(fmd.1$eig, 2)

# inspect the contributions of the variables
round(fmd.1$var$contrib, 2)
round(fmd.1$var$cos2, 4)

#--- Correlations of the dimensions ---

# use dimdesc function
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

#=== object RCs =====================================

fmd.2 <- FAMD(objrel[, style], ncp = length(style))
summary(fmd.2)

#--- Eigenvalues ---
fmd.2$eig
# inspect the contributions of the variables
round(fmd.2$var$contrib, 2)

#--- Correlations of the dimensions ---
# include Dims 1-7. Only use those dimensions that account for 5% of variance 
# or more
dd2 <- dimdesc(fmd.2, axes = 1:7)

dd2[[1]] # noun-related factors

dd2[[2]] # sentence complexity

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
internal <- names(subjrel)[c(22:23, 12, 14, 40:42, 18:20)]

#=== subject RCs =====================================
fmd.3 <- FAMD(subjrel[, internal], ncp = length(internal))
summary(fmd.3)

#--- Eigenvalues ---
fmd.3$eig # include all
round(fmd.3$var$contrib, 2)

#--- Correlations of the dimensions ---
# look at associations of the dimensions
dd3 <- dimdesc(fmd.3, axes = 1:9)

# (I'm doing only the dims with big effects)
dd3[[1]]
# +
# distance of antecedent to RC (antH2Rc, antLN) & non adjacent

dd3[[2]]
# +
# properties of the antecedent (num = other & pers = other; EMPTY heads) 
# -
# antPOS = noun, antNum = singular or plural

dd3[[3]]
# + 
# RC length & nested = Y & precRel = which

dd3[[4]]
# mixed bag
# +
# precRel = ZERO & antNum = singular
# -
# precRel = that & antNum = plural

dd3[[7]]
# +
# precRel = ZERO
# - 
# prec rel = that

dd3[[9]]
# +
# RC length & nested = no
# -
# nested = yes

#=== object RCs =====================================

fmd.4 <- FAMD(objrel[, internal], ncp = length(internal))
summary(fmd.4)

#--- Eigenvalues ---
fmd.4$eig # include all
round(fmd.4$var$contrib, 2)

#--- Correlations of the dimensions ---
dd4 <- dimdesc(fmd.4, axes = 1:9)

dd4[[1]] # distance from ant to RC

dd4[[2]] # properties of the ant [non-EMPTY heads]

dd4[[3]] # RClength and nestedness and prec rel = WHICH

dd4[[4]] # preceding rel

dd4[[5]] # preceding rel

dd4[[6]] # antecedent number and short RCs

dd4[[7]] # preceding rel and nestedness

dd4[[8]] # nestedness and RC length, and indefinite ants

dd4[[9]] # adjacent RC and long RC

# loop through and display all:
for(i in 1:9){
	d <- dd4[[i]]
	print(paste("Internal Dimension", i))
	for(j in 1:3){
		print(round(d[[j]], 3))
	}
	cat('\n')
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
# Just for fun, run two more FAMDs with all the predictors combined
n.vars <- length(c(style, internal)) 

#=== subject RCs =====================================

fmd.5 <- FAMD(subjrel[, c(style, internal)], ncp = n.vars)
summary(fmd.5)

#--- Eigenvalues ---
round(fmd.5$eig,3)
round(fmd.5$var$contrib, 2)

#--- Correlations of the dimensions ---
dd5 <- dimdesc(fmd.5, axes = 1:21)

dd5[[1]]
# +
# noun-related style measures (nouniness, word length) & learned genre 
# -
# personal pronouns & fiction

dd5[[2]]
# clause-related style measures: passives, subordConj & genre

dd5[[3]]
# antecedent distance measures:

dd5[[4]]
# properties of antecedent

dd5[[5]]

dd5[[7]]
# +
# RC length

# and so on...

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

dd6[[1]] # information density
# +
# non-academic , informational style (nouniness, word len, also passives and sent len) + general prose & learned
# - 
# personal pronouns & fiction

dd6[[2]] # clause complexity
# +
# clausal complexity (subordConj, passives, sent length) & learned
# -
# information density (TTR, nouniness) & newspper

dd6[[3]] # distance from ant to RC
# + 
# antHeadToRC, antLN, non-adjacent

dd6[[4]] # atypical antecedents
# +
# antNum2 = "other", antPOS = "other", indef
# -
# antPOS = "noun"

dd6[[5]]

dd6[[6]] # RC length and nested = yes

dd6[[7]] # precRel = that, split Infitives

dd6[[11]]

dd6[[12]] # split Inf

dd6[[13]] # precRel = ZERO

dd6[[17]]

#--- add to dataframe ----
obj.5.dims <- fmd.6$ind$coord[, 1:17]
objrel <- cbind(objrel, obj.5.dims)


# CREATE GRAPHICS -------------------------------------------------------

# code for creating tables of associations for a given dimension. 

a <- tableGrob(round(dd3[[1]][[1]], 3), theme = ttheme_minimal(), 
				widths = unit(c(1, .5), "mm"))
b <- tableGrob(round(dd3[[1]][[2]], 3), theme = ttheme_minimal(), 
				widths = unit(c(2, .5), "cm"))
d <- tableGrob(round(dd3[[1]][[3]], 3), theme = ttheme_minimal(), 
				widths = unit(c(1, .5), "cm"))
grid.arrange(a, b, d, nrow = 1, widths = c(.5,.2,.5))


##############################################################################
