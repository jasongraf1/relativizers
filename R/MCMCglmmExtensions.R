#############################################################################
# Adapted functions for working with MCMCglmm objects
# v 0.2 Jason Grafmiller
#############################################################################

# prior setting ---------------------------------------------------------

# function for fixed effects priors from Jarrod Hadfield
# http://permalink.gmane.org/gmane.comp.lang.r.lme4.devel/8608

# It returns a covariance matrix that can be passed to prior$B$V.  If the predictors
# had been scaled according to the procedure outlined in Gelman et al.
# (2008) then this would induce an identity prior covariance matrix on
# the new regression coefficients (i.e. they're iid a priori). Gelman
# recommends a scaled Cauchy or scaled t prior on these new regression
# ceofficients. I don't think this is possible in MCMCglmm* but a normal
# with variance equal to the sum of the variance components + 1 (probit)
# pi^2/3 (logit) I think is not too unreasonable - but it does put
# less weight on very extreme probabilities than the Cauchy and t. To
# achieve this just multiply the output of prior.scale by the variance
# you want to use.
#
# Bear in mind that the inputs are scaled, not the columns of the design
# matrix - this means that interactions are penalised more than main
# effects. In my field just-so stories involving 'significant'
# interactions  are often used to resuscitate a `failed' experiment, so
# penalising them by default may be no bad thing.

# Gelman et al. A WEAKLY INFORMATIVE DEFAULT PRIOR DISTRIBUTION FOR
# LOGISTIC AND OTHER REGRESSION MODELS The Annals of Applied Statistics
# 2008, Vol. 2, No. 4, 1360–1383

prior.scale <- function(formula, data){
	X1 <- model.matrix(formula, data)
	X2 <- get_all_vars(formula, data)
	X2 <- as.data.frame(lapply(X2, 
		function(x){if(is.numeric(x)){scale(x,
			scale=sd(x)*2)} else{x}}))
	X2 <- model.matrix(formula, data = X2)
	X2[, -1] <- apply(X2[, -1], 2,
		function(x){if(any(!x%in%c(0,1))){x} else{scale(x,
			center = sum(x)/length(x), scale = 1)}})
	crossprod(t(solve(t(X1) %*% X1, t(X1) %*% X2)))
}

# model checking functions ----------------------------------------------

# these were taken from:
# https://github.com/aufrank/R-hacks/blob/master/MCMCglmm-utils.R

vif.MCMCglmm <- function (fit, intercept.columns = c(1)) {
	nF <- fit$Fixed$nfl
	v <- cov(as.matrix(fit$X[,1:nF]))
	nam <- colnames(fit$Sol[,1:nF])
	v <- v[-intercept.columns, -intercept.columns, drop = FALSE]
	nam <- nam[-intercept.columns]
	d <- diag(v)^0.5
	v <- diag(solve(v/(d %o% d)))
	names(v) <- nam
	v
	}


kappa.MCMCglmm <- function (fit, add.intercept = TRUE, scale = TRUE, 
		intercept.columns = c(1)) {
	nF <- fit$Fixed$nfl
	X <- fit$X[,1:nF]
	if (add.intercept & scale) {
		kappa(cBind(rep(1), scale(X[, -intercept.columns])))
	} else if (add.intercept & !scale) {
		kappa(X)
	} else if (!add.intercept & scale) {
		kappa(scale(X[,-intercept.columns]))
	} else {
		kappa(X[,-intercept.columns])
	}
}

# Displying model output ------------------------------------------------

MCMCglmm.table <- function(data, betas = TRUE){
	# functions
	lower <- function(x) quantile(x, probs = .025)
	upper <- function(x) quantile(x, probs = .975)
	above0 <- function(x) {length(which(x > 0))/length(x)}
	below0 <- function(x) {length(which(x < 0))/length(x)}
	
	df <- data.frame(mean = apply(data, 2, "mean"))
	df <- cbind(df,
		data.frame(lower = apply(data, 2, lower)))
	df <- cbind(df,
		data.frame(upper = apply(data, 2, upper)))
	if(betas){
		df <- cbind(df, data.frame(below = apply(data, 2, below0)))
		df <- cbind(df, data.frame(above = apply(data, 2, above0)))
		colnames(df) <- c("posterior mean", "l-95% HPDI", "u-95% HPDI", 
			"P(&beta; < 0)", "P(&beta; > 0)")
	}
	else colnames(df) <- c("posterior mean", "l-95% HPDI", "u-95% HPDI")
	return (df)
}
