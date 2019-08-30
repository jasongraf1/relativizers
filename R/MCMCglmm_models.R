##############################################################################
#
# MCMCglmm analysis of English relativizers
#
# Jason Grafmiller
# last modified: 17/02/2016
##############################################################################

# The code here assumes that the FAMD analyses have already been run and the
# resulting dimensions added to the respective dataframes.
# See the file 'FAMD_analysis.R' for this


# LIBRARIES -------------------------------------------------------------

library(MCMCpack); library(MCMCglmm)

# The code below uses a few custom functions, which need to be loaded. These
# are in the `MCMCglmmExtensions.R` file:
source("R/MCMCglmmExtensions.R") # change the file path as necessary

#========================================================================
# NON-SUBJECT (OBJECT) RC MODEL -----------------------------------------

# start with the NSRC model.

# We are predicting the log odds of relativizer:
# Pr(rel = zero) + Pr(rel = 'that') + Pr(rel = 'which') = 1

# multinomial outcomes in MCMCglmm work similar to categorical predictors in
# standard regression. We define a reference level against which we predict
# the log odds of each of the other two levels. In this case, we will assign
# "that" to the reference level, so we be modeling "that" vs. "zero" and
# "that" vs. "which"

objrel$rel <- relevel(objrel$rel, ref = "THAT")

# for use of MCMCglmm, see J. Hadfield's documentation

# Hadfield's Short Tutorial:
# http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.160.5098&rep=rep1&type=pdf

# Hadfield's Course Notes:
# http://cran.r-project.org/web/packages/MCMCglmm/vignettes/CourseNotes.pdf

# see also these examples:
# http://www.natalialevshina.com/Documents/MCMCglmm_Tutorial.pdf
# https://hlplab.wordpress.com/2009/05/07/multinomial-random-effects-models-in-r/
# http://devillemereuil.legtux.org/wp-content/uploads/2012/12/tuto_en.pdf

# First set the priors for the model using some structures defined by
# MCMCglmm.

# 1. The R-structure: the variance-covariance matrix for the residuals.
# 2. The G-structure: the variance-covariance matrix for the random effects
#    (which is itself a list of sub-specifications, one per random effect).
# 3. The B-structure: the variance-covariance matrix for the fixed effects.

# These structures are defined on pages 16-19 of the tutorial.

# We will set the R-structure and priors according to the specifications in
# the tutorial, p. 23. For more detail, see pp. 95-101 of the course notes.

# residual covariance structure
k <- length(levels(objrel$rel))

I <- diag(k - 1) # identity matrix

J <- matrix(rep(1, (k - 1)^2), c(k - 1, k - 1)) # unit matrix

IJ <- (1/k) * (I + J)

obj.prior = list(R = list(V = IJ, fix = 1))

# set priors for random effects (G-structure). This is more complicated.
# Hadfield says that in Bayesian analyses both these parameter vectors are
# technically random, but we will stick with the frequentist terminology. The
# key difference between fixed and random effects is that the random effects
# are assumed to come from some distribution, the parameters of which are
# usually estimated, whereas the fixed effects are not. MCMCglmm assumes
# that the n random effects follow an n-dimensional multivariate normal
# distribution with null mean vector and a structured (co)variance matrix.
# This is the G-structure (see Tutorial p.17).

# When priors are not fixed, they are inverse-Wishart distributed. The HLP
# blog has a succinct description of how this works, I think:  "V is a
# hyperprior on the variance-covariance matrix (the inverse scale matrix),
# and n can be thought of as a degree-of-belief parameter. The tutorial
# suggests that n >= k to aid in convergence. Here we choose the lowest n
# that results in a proper prior, or the least-informative proper prior
# (but not an uninformative prior)."

# In the HLPlab example they specify two different priors:
#	G1 = list(V = diag(4), nu = 4)
# G2 = list(V = diag(2), nu = 2)

# The former is for the random intercepts by verb, and the latter is for the
# by-verb random slopes for the effect of modality (a binary predictor).

obj.prior$G <- list(G1 = list(V = diag(k - 1), n = k - 1),
	G2 = list(V = diag(4), n = 4), # for random slope of corpus
	G3 = list(V = diag(k - 1), n = k - 1)
)

# Since we are interested in comparing the object and subject functions, I'll
# include all the predictors that were shown to be significant by HiSzBo for
# BOTH models

# specification of model
# ----------------------
# random: random effects
# rcov: residual covariance structure
# burnin: number of initial MCMC iterations that are discarded
# nitt: number of MCMC iterations
# thin: interval defining number of stored iterations
# (N = (nitt - burnin)/thin. should be set such that N is between 1000-2000)

# more complex models require more iterations and larger burnins, but take
# longer to run

# the model was run with the whole dataset, and the subset of the data where
# RCLn >= 2. I found the model performs much better with these data points
# removed

# This will take some time. Run the following code all at once and a
# notification window will pop up when it is finished

# %%%% select and run %%%%
t1 <- proc.time() # set time
obj.m1a <- MCMCglmm(rel ~ -1 +
	trait +
	trait:(Style.Dim.1 +
		Style.Dim.2 +
		Style.Dim.3 +
		Style.Dim.4 +
		Style.Dim.5 +
		Style.Dim.6 +
		Style.Dim.7 +
		Int.Dim.1 +
		Int.Dim.2 +
		Int.Dim.3 +
		Int.Dim.4 +
		Int.Dim.5 +
		Int.Dim.6 +
		Int.Dim.7 +
		Int.Dim.8 +
		Int.Dim.9 +
		variety +
		time2 +
		variety:time2
		),
	random = ~ us(trait):category +
		us(corpus):category +
		us(trait):file,
	rcov = ~us(trait):units,
	pr = T, # store random effects
	prior = obj.prior,
	data = objrel2,
	family="categorical",
	verbose = F, burnin = 50000, nitt = 150000, thin = 100)

obj.m1a.runtime <- proc.time() - t1 # time to run (in seconds)

notify(paste('Model finished! Runtime:',
	round(obj.m1a.runtime[3]/60, 2), 'minutes'),
	invisible=FALSE, wait=FALSE)
# %%%%%%%%%

summary(obj.m1a)


#=== Get model predictions =========================

# This is not as easy as it seems, the built-in funciton does odd things and
# generates poor predictions. I'm going to derive predictions directly from
# the model formula, using the model matrix stored in the fit object. This
# is the matrix of observations with features converted to numerical values.

c <- length(levels(objrel$category))

# model matrix has (k -1 ) * n rows, where n is the number of tokens. This
# is because we have predictions for each of the two predicted outcomes,
# 'which' and 'zero'

# this is the matrix of values of the fixed effects
modMat2 <- as.data.frame(as.matrix(obj.m1a$X))
dim(modMat2)

N <- nrow(modMat2)/2 # number of observations

# the matrix for which
modWhich <- modMat2[1:4723,] # the fixed effects
# random effects
modWhich$category <- 0 # category
modWhich$catXcorpus <- 0 # cat * corpus
modWhich$file <- 0 # file

# the matrix for zero
modZero <- modMat2[4724:9446,]
modZero$category <- 0
modZero$catXcorpus <- 0
modZero$file <- 0

# get the posterior means across the draws for the fixed effects and each
# level of the random effects. The posterior estimates are stored in fit$Sol
means2 <- apply(obj.m1a$Sol, 2, mean)
# fixed effects
coefs3 <- means2[1:40]
# random effects
categoryIntsWhich <- means2[41:55]
categoryIntsZero <- means2[56:70]
categorySlopes <- means2[71:130]
filesWhich <- means2[131:1666]
filesZero <- means2[1667:3202]


# Now loop through each observation and multiply its features by the model
# formula and random group level adjustments

for (i in 1:N){
	# do which
	modWhich[i, 1:40] <- as.vector(modWhich[i, 1:40] * coefs3)
	ranef1w <- paste("category.rel.WHICH.category",
		objrel2[i, 6], sep = '.')
	modWhich[i, 41] <- as.vector(categoryIntsWhich[ranef1w])
	ranef2 <- paste("category", objrel2[i, 2], "category",
		objrel2[i, 6], sep = '.')
	modWhich[i, 42] <- as.vector(categorySlopes[ranef2])
	ranef3w <- paste("file.rel.WHICH.file",
		objrel2[i, 5], sep = '.')
	modWhich[i, 43] <- as.vector(filesWhich[ranef3w])

	# do zero
	modZero[i, 1:40] <- as.vector(modZero[i, 1:40] * coefs3)
	ranef1z <- paste("category.rel.ZERO.category", objrel2[i, 6], sep = '.')
	modZero[i, 41] <- as.vector(categoryIntsZero[ranef1z])
	modZero[i, 42] <- as.vector(categorySlopes[ranef2])
	ranef3z <- paste("file.rel.ZERO.file", objrel2[i, 5], sep = '.')
	modZero[i, 43] <- as.vector(filesZero[ranef3z])

}

head(modWhich)
head(modZero)

# add up the values along the rows to calculate the log odds for each
# observation
sW <- rowSums(modWhich)
sZ <- rowSums(modZero)

# get the predicted outcome for each observation
pred.W <- ifelse(sW > 0, "WHICH", "THAT")
pred.Z <- ifelse(sZ > 0, "ZERO", "THAT")

pred.rel <- c()
max.prob <- c()
for (i in 1:N){
	a <- sW[[i]]
	b <- sZ[[i]]
	# calculate predictions
	# I got this from post by Jarrod Hadfield:
	# https://stat.ethz.ch/pipermail/r-sig-mixed-models/2014q3/022422.html
	P.that <- 1/(1 + exp(a) + exp(b))
	P.which <- exp(a)/(1+exp(a) + exp(b))
	P.zero <- exp(b)/(1 + exp(a) + exp(b))
	max.prob <- c(max.prob, max(P.that, P.zero, P.which))
	#cat(i, a, b)
	#print(max(P.that, P.zero, P.which))
	# determine which relativizer has the highest probability
	if (max(P.that, P.zero, P.which) == P.that){
		pred.rel <- c(pred.rel, "THAT")
	}
	else if (max(P.that, P.zero, P.which) == P.zero){
		pred.rel <- c(pred.rel, "ZERO")
	}
	else if (max(P.that, P.zero, P.which) == P.which){
		pred.rel <- c(pred.rel, "WHICH")
	}
	else pred.rel <- c(pred.rel, "NA")
}

# distribution of outcome probabilities
summary(max.prob)
# for more than 75% of our observations, the model predicts one outcome to
# have at least a 65% probability

objrel2$predicted <- factor(pred.rel)
objrel2$observed <- objrel2$rel
objrel2$max.prob <- max.prob

# table of predictions vs observations
addmargins(xtabs(~ observed + predicted, objrel2))

# predictive accuracy
sum(objrel2$predicted == objrel2$observed)/N

# baseline accuracy
max(prop.table(table(objrel2$rel)))



# PLOT EFFECTS ----------------------------------------------------------

library(reshape2)

# create dataframes of fixed effects
fixed.obj <- as.data.frame(as.matrix(obj.m1a$Sol))[,1:40]

int.var1 <- melt(fixed.obj[, 17:34])
int.var1$rel <- rep(c("WHICH", "ZERO"), each = 1000)
int.var1$pred <- factor(rep(paste("Internal.Dim", 1:9, sep = '.'), each = 2000),
			levels = rev(paste("Internal.Dim", 1:9, sep = '.')))

style.var1 <- melt(fixed.obj[, 3:16])
style.var1$rel <- factor(rep(c("WHICH", "ZERO"), each = 1000))
style.var1$pred <- factor(rep(paste("Style.Dim", 1:7, sep = '.'), each = 2000),
			levels = rev(paste("Style.Dim", 1:7, sep = '.')))

ext.var1 <- melt(fixed.obj[, 35:38])
ext.var1$rel <- rep(c("WHICH", "ZERO"), each = 1000)
ext.var1$pred <- rep(c("variety (= BrE)", "time (= 1990s)"), each = 2000)


#--- Internal predictors ---
# make the dot plots with HPDI bars

# label dimensions transparently
dims <- c("1. RC = non-adjacent",
	"2. Antecedent head = non-empty",
	"3. RC length",
	"4. prior rel. = NONE",
	"5. prior rel. = THAT",
	"6. Antecedent = plural",
	"7. prior rel. = ZERO",
	"8. nested = NO",
	"9. RC = adjacent")

ggplot(int.var1, aes(pred, value, group = rel)) + coord_flip() +
	geom_hline(yintercept = 0, linetype = "dashed") +
	stat_summary(fun.data = median_hilow, aes(shape = rel),
		 geom = "errorbar", position = position_dodge(width = 0.5),
		 width = .1) +
	stat_summary(fun.y = mean, aes(shape = rel), size = 4,
		 geom = "point", position = position_dodge(width = 0.5)) +
	labs(x = "", y = "posterior log odds of relativizer\n(baseline = THAT)",
		 title = "Internal predictors") +
	scale_shape_manual(values = c(16, 2)) +
	scale_x_discrete(labels = rev(dims)) +
	theme(legend.justification=c(1,1), legend.position=c(1,1),
		legend.text = element_text(face = "italic"),
		legend.title = element_blank())


#--- Stylistic predictors ---
# make the dot plots with HPDI bars
dims2 <- c(expression(paste("1.", interpersonal %<->% informational)),
		expression(paste("2.", informal %<->% formal)))


ggplot(style.var1[1:4000,], aes(pred, value, group = rel)) + coord_flip() +
	geom_hline(yintercept = 0, linetype = "dashed") +
	stat_summary(fun.data = median_hilow, aes(shape = rel),
		 geom = "errorbar", position = position_dodge(width = 0.5),
		 width = .1) +
	stat_summary(fun.y = mean, aes(shape = rel), size = 4,
		 geom = "point", position = position_dodge(width = 0.5)) +
	labs(x = "", y = "posterior log odds of relativizer\n(baseline = THAT)",
		 title = "Stylistic predictors") +
	scale_shape_manual(values = c(16, 2)) + scale_x_discrete(labels = rev(dims2)) +
	theme(legend.justification=c(0,1), legend.position=c(0,1),
		legend.text = element_text(face = "italic"),
		axis.text.y  = element_text(hjust= 1), legend.title = element_blank())


#--- External predictors ---

# Not as simple as the others
# get posterior estimates from model
ext.df.obj <- data.frame(
	WhAmE1960 = fixed.obj[,1],
	WhBrE1960 = fixed.obj[,1] + fixed.obj[,35],
	WhAmE1990 = fixed.obj[,1] + fixed.obj[,37],
	WhBrE1990 = fixed.obj[,1] + fixed.obj[,35] + fixed.obj[,37] + fixed.obj[,39],
	ZeAmE1960 = fixed.obj[,2],
	ZeBrE1960 = fixed.obj[,2] + fixed.obj[,36],
	ZeAmE1990 = fixed.obj[,2] + fixed.obj[,38],
	ZeBrE1990 = fixed.obj[,2] + fixed.obj[,36] + fixed.obj[,38] + fixed.obj[,40]
)

# create useable dataframe for ggplot
ext.df.obj2 <- melt(ext.df.obj)
ext.df.obj2$variety = factor(ifelse(grepl("AmE", ext.df.obj2$variable),
				"AmE", "BrE"))
ext.df.obj2$time = factor(ifelse(grepl("1960", ext.df.obj2$variable),
				 "1960s", "1990s"))
ext.df.obj2$rel = factor(ifelse(grepl("Wh", ext.df.obj2$variable),
				"WHICH", "ZERO"))

confInt <- function(x){quantile(x, probs = c(.025, .975))}
# add lower and upper bounds for the HPDI
ext.df.obj3 <- cbind(aggregate(value ~ variety + time + rel, ext.df.obj2,
				 FUN = "mean"),
			aggregate(value ~ variety + time + rel, ext.df.obj2,
				FUN = "confInt")[,4][,1],
			aggregate(value ~ variety + time + rel, ext.df.obj2,
				FUN = "confInt")[,4][,2])
names(ext.df.obj3)[4:6] <- c("mean", "lower", "upper")

# plot
ggplot(ext.df.obj3, aes(time, mean, group = rel)) + facet_wrap(~ variety) +
	geom_errorbar(aes(ymin = lower, ymax = upper), width = .1,
			position = position_dodge(.1)) +
	geom_hline(yintercept = 0, linetype = "dashed") +
	geom_point(aes(shape = rel), position = position_dodge(.1), size = rel(5)) +
	geom_line(position = position_dodge(.1)) +
	scale_shape_manual(values = c(16, 2)) +
	labs(x = '', y = "posterior log odds of relativizer\n(baseline = THAT)",
		title = "External predictors: Time by Variety") +
	theme(legend.justification=c(0,0), legend.position=c(0,0),
		legend.title = element_blank())


# TABLES OF ESTIMATES ---------------------------------------------------

# create tables showing the posterior means, the upper and lower 95% HPDI
# bounds, and the probabilities of B being greater or less than 0.

# use the MCMCglmm.table() function in the 'MCMCglmmExtensions.R' file

# internal
a <- MCMCglmm.table(fixed.obj[,c(1, seq(17, 33,2))])
b <- MCMCglmm.table(fixed.obj[,c(2, seq(18, 34,2))])
ab <- rbind(a, b)
rownames(ab) <- gsub('^.*?\\.','', rownames(ab));
rownames(ab)[c(1, 11)] <- c("WHICH:Intercept", "ZERO:Intercept")

# stylistic
a <- MCMCglmm.table(fixed.obj[, seq(3, 15,2)])
b <- MCMCglmm.table(fixed.obj[, seq(4, 16, 2)])
ab <- rbind(a, b)
rownames(ab) <- gsub('^.*?\\.','', rownames(ab))

# external
a <- MCMCglmm.table(fixed.obj[, c(1, seq(35, 39, 2))])
b <- MCMCglmm.table(fixed.obj[, c(2, seq(36, 40, 2))])
ab <- rbind(a, b)
rownames(ab) <- gsub('^.*?\\.','', rownames(ab));
rownames(ab)[c(1,5)] <- c("WHICH:Intercept", "ZERO:Intercept")


# ===========================================================================
# SUBJECT RC MODEL ------------------------------------------------------

# number of possible outcomes is 2

# residual covariance structure:
Is <- diag(2 - 1) # identity matrix
Js <- matrix(rep(1, (2 - 1)^2), c(2 - 1, 2 - 1)) # unit matrix

# I and J will be used to set up constraints on the covariance structure of the residuals (p 97).
IJs = (1/2)*(Is + Js)

# residual prior:
subj.prior <- list(R = list(V = IJs, fix = 1))

# prior for random effects is slightly different than above.
subj.prior$G <- list(G1 = list(V = 1, nu = .002), # intercept for category
			G2 = list(V = diag(4), n = 4), # by-cat random slope for corpus
			G3 = list(V = 1, nu = .002)) # intercept for file

# Again, in order to deal with near separation issues, Hadfield suggests using a
# fixed effects prior that is flat on the probability scale (p55)
SRC.formula <- rel ~ variety * time2 +
		Style.Dim.1 +
		Style.Dim.2 +
		Style.Dim.3 +
		Style.Dim.4 +
		Style.Dim.5 +
		Style.Dim.6 +
		Style.Dim.7 +
		Int.Dim.1 +
		Int.Dim.2 +
		Int.Dim.3 +
		Int.Dim.4 +
		Int.Dim.5 +
		Int.Dim.6 +
		Int.Dim.7 +
		Int.Dim.8 +
		Int.Dim.9

fixSRC <- prior.scale(SRC.formula, subjrel)
subj.prior$B <- list(mu = rep(0, nrow(fixSRC)), V = fixSRC*(1 + pi^2/3))

src.data <- subjrel[, c(47:62, 5, 2, 6, 3, 43, 8)]
for(i in 1:16) {
  # standardize the dimensions
	src.data[, i] <- scale(src.data[, i])
}

# %%%% select all and run %%%%
t2 <- proc.time() #set time
subj.m1a <- MCMCglmm(SRC.formula,
		random = ~ category + us(corpus):category + file,
		#rcov = ~us(trait):units,
		pr = T,
		prior = subj.prior,
		data = src.data,
		family = "categorical",
		verbose = F, burnin = 50000, nitt = 200000, thin = 100)

subj.m1a.runtime <- proc.time() - t2 # time to run (in seconds)

notify(paste("Process finished! Model runtime:", subj.m1a.runtime[3]/60, "minutes"))
# %%%%%%

summary(subj.m1a)


#=== Get model predictions =========================

# this is simpler than in the NSRC model.

modMat <- as.data.frame(as.matrix(subj.m1a$X)) # model matrix
rows <- ncol(modMat)

# get the posterior means across the draws for the fixed effects and each
# level of the random effects. The posterior estimates are stored in fit$Sol
means <- apply(subj.m1a$Sol, 2, mean)
coefs2 <- means[1:rows] # fixed eff coefficients
categoryInts <- means[(rows+1):(rows+15)] # category random intercepts (N = 14)
categorySlopes <- means[(rows+16):(rows+75)] # by-corpus random slopes for category (N = 59)
files <- means[(rows+76):length(means)] # file random intercepts

# matrix for predictions
predMat <- modMat
predMat$category <- 0
predMat$catXcorpus <- 0
predMat$file <- 0

# Now loop through each observation and multiply its features by the model
# formula and random group level adjustments
for (i in 1:nrow(modMat)){
	predMat[i, 1:rows] <- as.vector(modMat[i, 1:rows] * coefs2)
	ranef1 <- paste("category", subjrel[i, 6], sep = '.')
	predMat[i, rows+1] <- as.vector(categoryInts[ranef1])
	ranef2 <- paste("corpus", subjrel[i, 2], ".category.", subjrel[i, 6], sep = '')
	predMat[i, rows+2] <- as.vector(categorySlopes[ranef2])
	ranef3 <- paste("file", subjrel[i, 5], sep = '.')
	predMat[i, rows+3] <- as.vector(files[ranef3])
}

summary(predMat)

# add up the values along the rows to calculate the log odds for each
# observation
s3 <- rowSums(predMat)
head(s3)

# add predictions to the dataframe
subjrel$pred.logodds <- s3 # predictions in log odds scale
subjrel$pred.probs <- inv.logit(s3) # predictions in probability scale

# get the predicted outcome
pred.dummy.subj <- ifelse(s3 > 0, "WHICH", "THAT")
subjrel$pred.rel <- factor(pred.dummy.subj)

# percent predicted correctly
sum(pred.dummy.subj == subjrel$rel)/nrow(subjrel)

# baseline accuracy
prop.table(table(subjrel$rel))

# get C and Somer's Dxy:
resp.subj <- as.numeric(subjrel$rel) -1
Hmisc::somers2(s2, resp.subj)

# confustion matrix table
addmargins(xtabs(~ rel + pred.rel, subjrel))



# PLOT EFFECTS ----------------------------------------------------------

library(reshape2)

# fixed effects estimates
fixed.src <- as.data.frame(as.matrix(subj.m1a$Sol))[, 1:20]

# internal, stylistic, external variables
int.var2 <- melt(fixed.src[, c(9:12, 15, 17)])
levels(int.var2$variable) <- gsub("Int", "Internal", levels(int.var2$variable))
ilevs <- levels(int.var2$variable)
int.var2$variable <- factor(int.var2$variable, levels = rev(ilevs))

style.var2 <- melt(fixed.src[, 2:3])
slevs <- levels(style.var2$variable)
style.var2$variable <- factor(style.var2$variable, levels = rev(slevs))

ext.var2 <- melt(fixed.src[, 18:20])


#--- Internal predictors ---
# make the dot plots with HPDI bars

# label dimensions transparently
dims3 <- c("1. RC = non-adjacent",
	"2. Antecedent head = empty",
	"3. RC length",
	"4. Antecedent = sing.",
	"7. prior rel. = ZERO/WHICH",
	"9. nested = no")

ggplot(int.var2, aes(variable, value)) + coord_flip() +
	geom_hline(yintercept = 0, linetype = "dashed") +
	stat_summary(fun.data = median_hilow, geom = "errorbar",
		width = .1) +
	stat_summary(fun.y = mean, size = 4,
		geom = "point") +
	scale_x_discrete(labels = rev(dims3)) +
	labs(x = "", y = "posterior log odds of WHICH",
		title = "Internal predictors")


#--- Stylistic predictors ---
# make the dot plots with HPDI bars

# use same dimension labels as before
ggplot(style.var2, aes(variable, value)) + coord_flip() +
	geom_hline(yintercept = 0, linetype = "dashed") +
	stat_summary(fun.data = median_hilow, geom = "errorbar",
			width = .1) +
	stat_summary(fun.y = mean, size = 4,
			geom = "point") +
	scale_x_discrete(labels = rev(dims2)) +
	labs(x = "", y = "posterior log odds of WHICH",
		title = "Stylistic predictors")


#--- External predictors ---
ext.df <- data.frame(
	AmE1960 = fixed.subj[,1],
	BrE1960 = fixed.subj[,1] + fixed.subj[,18],
	AmE1990 = fixed.subj[,1] + fixed.subj[,19],
	BrE1990 = fixed.subj[,1] + fixed.subj[,18] + fixed.subj[,19] + fixed.subj[,20]
)
ext.df2 <- melt(ext.df)
ext.df2$variety = factor(ifelse(grepl("AmE", ext.df2$variable), "AmE", "BrE"))
ext.df2$time = factor(ifelse(grepl("1960", ext.df2$variable), "1960s", "1990s"))

confInt <- function(x){quantile(x, probs = c(.025, .975))}
ext.df3 <- cbind(aggregate(value ~ variety + time, ext.df2, FUN = "mean"),
		aggregate(value ~ variety + time, ext.df2, FUN = "confInt")[,3][,1],
		aggregate(value ~ variety + time, ext.df2, FUN = "confInt")[,3][,2])
names(ext.df3)[3:5] <- c("mean", "lower", "upper")

ggplot(ext.df3, aes(time, mean, group = variety)) + facet_wrap(~variety) +
	geom_hline(yintercept = 0, linetype = "dashed") +
	geom_errorbar(aes(ymin = lower, ymax = upper), width = .1,
			position = position_dodge(.1)) +
	geom_point(position = position_dodge(.1), size = rel(5)) +
	geom_line(position = position_dodge(.1)) +
	scale_shape_manual(values = c(16)) +
	labs(x = '', y = "posterior log odds of WHICH",
		title = "External predictors: Time by Variety") +
	theme(legend.justification=c(0,0), legend.position=c(0,0),
		legend.title = element_blank())


# TABLES OF ESTIMATES ---------------------------------------------------

# internal
MCMCglmm.table(fixed.src[,c(1,9:17)])

# stylistic
MCMCglmm.table(fixed.src[, 2:8])

# external
MCMCglmm.table(fixed.src[, c(1, 18:20)])


