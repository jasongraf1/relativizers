#############################################################################
# Functions for extending ggplot2 (v 1.X) graphics
# v 0.2 Jason Grafmiller
# Last updated: Jan 22, 2016
# Not fully checked for compatibility with ggplot2 version 2.0
#############################################################################


# GENERAL FUNCTIONS -----------------------------------------------------

ggAssoc.plot = function (data, x, y) {
	# function for creating association plots with ggplot2
	require(ggplot2, quietly = T)
	
	xvar <- deparse(substitute(x))
	yvar <- deparse(substitute(y))
	
	# z is a 2-dimensional contingency table
	z <- table(data[, c(xvar, yvar)])
	
	if (length(dim(z)) != 2L)
		stop("'x' must be a 2-d contingency table")
	if (any(z < 0) || any(is.na(z)))
		stop("all entries of 'x' must be nonnegative and finite")
	if ((n <- sum(z)) == 0L)
		stop("at least one entry of 'x' must be positive")
	#if (length(col) != 2L)
	#  stop("incorrect 'col': must be length 2")
	resids <- chisq.test(z)$residuals
	expect <- sqrt(chisq.test(z)$expected)
	df <- data.frame(c = as.vector(z),
                     r = as.vector(resids),
                     e = as.vector(expect))
	df$row <- rep(rownames(z), ncol(z))
	cols <- c()
	for (i in seq(1, ncol(z)))
		cols <- c(cols, rep(colnames(z)[i], nrow(z)))
	df$col <- cols
	df$col <- factor(df$col, levels = colnames(z))
	df$fill <- "pos"
	df$fill <- factor(df$fill,levels = c("pos", "neg"))
	df[df$r < 0, "fill"] = "neg"
	space <- max(df$e) + (max(df$e)/ncol(z))
	xpos <- seq(space, space*ncol(z), space)
	colpos <- c()
	for (i in seq(1, ncol(z)))
		colpos = c(colpos, rep(xpos[i], nrow(z)))
	df$ehalf = df$e/2
	df$xmin = colpos - df$ehalf
	df$xmax = colpos + df$ehalf
	df$ymin = df$r
	df[df$r > 0, "ymin"] = 0
	df$ymax = df$r
	df[df$r < 0, "ymax"] = 0
	# make the plot
	p <- ggplot(df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
		geom_rect(aes(fill = fill)) +
		ylim(c(min(df[, "r"]), max(df[, "r"]))) +
		facet_grid(row ~ .) +
		scale_x_continuous(breaks = xpos, labels = levels(df$col)) +
		theme(panel.grid.major.x = element_blank(), legend.position = "none")
	
	return(p)
}


ggBar.plot <- function(data, x, y, facet = NULL, percent = T,
		facet.cols = NULL, scales = "fixed",
		width = 0.7, size = 5, opp.cols = FALSE){
	# create barplot of proportions with counts superimposed over plots
	# allows for faceting by 1 or 2 groups entered as character vector
	
	# e.g. library(langaugeR); 
	#	ggBar.plot(dative, PronomOfTheme, RealizationOfRecipient, facet = "Modality")
	
	require(ggplot2)
	require(plyr)
	xvar <- deparse(substitute(x))
	yvar <- deparse(substitute(y))
	xlevs <- length(levels(data[, xvar]))
	ylevs <- length(levels(data[, yvar]))
	if(opp.cols) {
		ycols <- c("white", rep("black", ylevs - 1))
	}
	else ycols <- rep("black", ylevs)
	
	if (!is.null(facet)){
		if (length(facet) > 1){
			fvar1 <- facet[1]
			fvar2 <- facet[2]
			f1levs <- length(levels(data[, fvar1]))
			f2levs <- length(levels(data[, fvar2]))
			levs <- f1levs * f2levs * xlevs
			mydata <- data[,c(xvar, fvar1, fvar2, yvar)]
			mytable <- ftable(mydata)
			d <- as.data.frame(mytable)
			d$Prop <- as.data.frame(prop.table(mytable, 1))[, ncol(d)]
			d <- ddply(d, c(xvar, fvar1, fvar2), transform,
								 pos = cumsum(Prop) - (0.5 * Prop))
			# make plot
			p <- ggplot(d, aes_string(xvar, "Prop")) +
				geom_bar(aes_string(fill = yvar),
								 stat = "identity", width = .7, color = "black") +
				geom_text(aes(label = Freq, y = pos),
									color = rep(ycols, xlevs), size = size) +
				facet_grid(as.formula(paste(fvar1, "~", fvar2)),
									 scales = scales)
		}
		else{
			#fvar <- deparse(substitute(facet))
			fvar <- facet
			flevs <- length(levels(data[, fvar]))
			mydata <- data[,c(xvar, fvar, yvar)]
			mytable <- ftable(mydata)
			d <- as.data.frame(mytable)
			d$Prop <- as.data.frame(prop.table(mytable, 1))[, ncol(d)]
			d <- ddply(d, c(xvar, fvar),
								 transform, pos = cumsum(Prop) - (0.5 * Prop))
			p <- ggplot(d, aes_string(xvar, "Prop")) +
				geom_bar(aes_string(fill = yvar),
								 stat = "identity", width = width, col = "black") +
				geom_text(aes(label = Freq, y = pos), col = rep(ycols, xlevs*flevs), size = size) +
				facet_wrap(as.formula(paste("~", fvar)),
									 ncol = facet.cols, scales = scales)
		}
	}
	else{
		mydata <- data[c(xvar, yvar)]
		mytable <- table(mydata)
		d <- as.data.frame(mytable)
		d$Prop <- as.data.frame(prop.table(mytable, 1))[,3]
		d <- ddply(d, xvar, transform, pos = cumsum(Prop) - (0.5 * Prop))
		p <- ggplot(d, aes_string(xvar, "Prop")) +
			geom_bar(aes_string(fill = yvar),
							 stat = "identity", width = .7, color = "black") +
			geom_text(aes(label = Freq, y = pos),
								color = rep(ycols, xlevs), size = size)
	}
	if (percent){
		p <- p + scale_y_continuous(breaks = seq(0, 1, .25),
																labels = paste("%", seq(0,100,25), sep = "")) +
			labs(x = "", y = "percentage of tokens")
	}
	else {
		p <- p + labs(x = "", y = "proportion of tokens")
	}
	return(p)
}


ggBox.plot <- function(data, x, y, facet = NULL, fill.col = NULL,
											 facet.cols = NULL, notch = TRUE, scales = "fixed",
											 width = 0.7){
	# create barplot of proportions with counts superimposed over plots
	# allows for faceting by 1 or 2 groups entered as character vector
	
	# e.g. ggBox.plot(chickwts, feed, weight, notch = F)
	
	require(ggplot2)
	require(plyr)
	xvar <- deparse(substitute(x))
	yvar <- deparse(substitute(y))
	if (!is.numeric(data[, yvar])){
		stop("y variable must be numeric")
	}
	if(is.null(fill.col)) {
		fill.col <- "gray"
	}
	else fill.col <- fill.col # for allowing aes colors (tbc...)
	
	if (!is.null(facet)){
		if (length(facet) > 1){
			fvar1 <- facet[1]
			fvar2 <- facet[2]
			p <- ggplot(data, aes_string(xvar, yvar)) +
				geom_boxplot(fill = fill.col, notch = notch,
										 width = width) +
				facet_grid(as.formula(paste(fvar1, "~", fvar2)),
									 scales = scales)
		}
		else{
			#fvar <- deparse(substitute(facet))
			fvar <- facet
			p <- ggplot(data, aes_string(xvar, yvar)) +
				geom_boxplot(fill = fill.col, notch = notch,
										 width = width) +
				facet_wrap(as.formula(paste("~", fvar)),
									 ncol = facet.cols, scales = scales)
		}
	}
	else{
		p <- ggplot(data, aes_string(xvar, yvar)) +
			geom_boxplot(fill = fill.col, notch = notch,
									 width = width)
	}
	
	return(p)
}


ggDensity.plot <- function(data, y, facet = NULL,
													 fill.col = NULL, line.col = "black",
													 scales = "fixed",
													 bin.width = NULL, add.line = T,
													 alpha.h = 1, alpha.d = .2,
													 fill.d = "blue"){
	# create plot representing density of numeric vector
	# shows a histogram with a density curve overlayed
	
	# e.g.: ggDensity.plot(data.frame(x = rnorm(1000)), x)
	
	require(ggplot2)
	#xvar <- deparse(substitute(x))
	yvar <- deparse(substitute(y))
	if (!is.numeric(data[, yvar])){
		stop("y variable must be numeric")
	}
	if (is.null(bin.width)){
		bin.width <- (max(data[, yvar]) - min(data[, yvar]))/40
	}
	if (is.null(fill.col)) {
		fill.col <- "lightgray" # set default fill color
	}
	if (line.col == F) {
		line.col <- fill.col # set default line color to match fill color
	}
	if (!is.null(facet)){
		if (length(facet) > 1){
			fvar1 <- facet[1]
			fvar2 <- facet[2]
			p <- ggplot(data, aes_string(yvar)) +
				geom_histogram(aes(y = ..density..), binwidth = bin.width,
											 fill = fill.col, color = line.col,
											 alpha = alpha.h) +
				facet_grid(as.formula(paste(fvar1, "~", fvar2)),
									 scales = scales)
		}
		else {
			fvar1 <- facet
			p <- ggplot(data, aes_string(yvar)) +
				geom_histogram(aes(y = ..density..), binwidth = bin.width,
											 fill = fill.col, color = line.col,
											 alpha = alpha.h) +
				facet_wrap(as.formula(paste("~", fvar1)),
									 scales = scales)
		}
	}
	else {
		p <- ggplot(data, aes_string(yvar)) +
			geom_histogram(aes(y = ..density..), binwidth = bin.width,
										 fill = fill.col, color = line.col,
										 alpha = alpha.h)
	}
	if (add.line & fill.d == F){
		p <- p + geom_density()
	}
	else if (add.line){
		p <- p + geom_density(alpha = alpha.d, fill = fill.d)
	}
	return (p)
}


ggLogit.plot <- function (x, data, method = "cut",
													where = seq(0, 1, by = 0.1),
													scalesize = NA, r2 = FALSE,
													dot.shape = 19,
													dot.size = 3,
													col="black",
													add.se = FALSE)
{
	# Function for prettifying plot.logistic.fit.fnc. Uses ggplot2 and is
	# compatible with latest version of lme4 (1.1-7)
	# x is a model fit with glm, lrm, glmer, or averaged model

	require(lme4, quietly = TRUE)
	require(rms, quietly = TRUE)
	require(MuMIn, quietly = TRUE)
	require(ggplot2, quietly = TRUE)
	
	# e.g. library(languageR);
	# m <- glm(RealizationOfRecipient ~ PronomOfTheme + LengthOfRecipient, data = dative, family = binomial)
	# ggLogit.plot(m, data = dative)
	
	if (class(x)[1] == "glmerMod") {
		y = attr(x@frame, "terms")
		depvar <- names(attr(terms(y), "dataClasses")[attr(terms(y),"response")])
		probs <- fitted(x)
	}
	else if (class(x)[1] == "lrm") {
		depvar <- as.character(formula(x$call))[2]
		probs <- predict(x, type = "fitted")
	}
	else if (class(x)[1] == "glm") {
		depvar <- as.character(x$formula)[2]
		probs <- fitted(x)
	}
	else if (class(x)[1] == "averaging"){
		depvar <- as.character(x$formula)[2]
		probs <- predict(x, full = T, type = 'response')
	}
	else if (class(x) == "MCMCglmm"){
		depvar <- all.vars(object$Fixed$formula)[1]
		probs <- MCMCglmm.predict(mm2, data)$probs
	}
	else if (class(x) == "list"){
		depvar <- x[[2]]
		probs <- x[[1]]
	}
	else {
		stop("first argument is not a model object or list")
	}
	if (method == "cut") {
		classes = cut2(probs, where, levels.mean = TRUE)[drop = T]
		classCounts = table(classes)
		means = tapply(as.numeric(data[, depvar]) - 1, classes,
									 mean)
		means = means[!is.na(means)]
		DF = data.frame(pred.probs = as.numeric(names(means)),
										obs.props = means,
										errs = tapply(as.numeric(data[, depvar]), classes,se))
	}
	else {
		if (method == "shingle") {
			sh = equal.count(probs)
			means = rep(0, length(levels(sh)))
			midpoints = rep(0, length(means))
			for (i in 1:length(levels(sh))) {
				means[i] = mean(probs[probs > levels(sh)[[i]][1] &
																probs < levels(sh)[[i]][2]])
				midpoints[i] = as.character(mean(levels(sh)[[i]]))
			}
			names(means) = as.character(midpoints)
		}
	}
	p <- ggplot(DF, aes(pred.probs, obs.props)) + ylim(0,1) +
		geom_abline(intercept=0, slope=1, color="gray") +
		geom_point(size = dot.size, color = col, shape = dot.shape) +
		labs(x="mean predicted probabilities",y = "observed proportions")
	#if ((method == "cut") & (!is.na(scalesize))) {
	#    symbols(as.numeric(names(means)), as.numeric(means),
	#        circles = as.numeric(classCounts), inches = scalesize,
	#        main = " ", add = T)
	# }
	#else {
	#    points(as.numeric(names(means)), means,
	# type=type, pch = dot.pch,cex = dot.size,lty=lty,lwd=lwd)
	if(add.se){
		p <- p + geom_errorbarh(aes(xmin = pred.probs - errs,
																xmax = pred.probs + errs))
	}
	#}
	if(r2){
		p<- p + ggtitle(paste("R-squared: ",
													round(cor(DF$pred.probs, DF$obs.props)^2, 2), sep = ""))
	}
	
	return(p)
}


ggMCMC.plot <- function(data, resp = NULL){
	require(ggplot2)
	require(gridExtra)
	nvar <- ncol(data)
	nit <- nrow(data)
	
	p <- ggplot(data, aes_string(yvar)) +
		geom_histogram(aes(y = ..density..), binwidth = bin.width,
									 fill = fill.col, color = line.col,
									 alpha = alpha.h)
}



ggMCMChist.plot <- function(data,
														resp = NULL,
														fill.col = NULL, line.col = "black",
														scales = "fixed",
														bin.width = NULL, add.line = T,
														alpha.h = 1, alpha.d = .2,
														fill.d = "blue"){
	require(ggplot2)
	require(coda)
	
	# for HPD intervals
	hpd <- apply(data, 2, function(x) as.vector(HPDinterval(as.mcmc(x), prob=0.95)))
	hpd <- rbind(apply(data, 2, mean), hpd)
	rownames(hpd) <- c("mean", "upper", "lower")
	hpd <- data.frame(t(hpd))
	hpd$variable <- rownames(hpd)
	
	if (is.null(resp)) {
		resp <- 'outcome'
	}
	
	#dmax <- floor(max(apply(data, 2, function(x) max(density(x)$y))) * .1)
	
	mdata <- suppressMessages(melt(data))
	
	if (is.null(bin.width)){
		bin.width <- (max(mdata$value) - min(mdata$value))/40
	}
	if (is.null(fill.col)) {
		fill.col <- "lightgray" # set default fill color
	}
	if (line.col == F) {
		line.col <- fill.col # set default line color to match fill color
	}
	p <- ggplot(mdata, aes(value)) + facet_wrap(~ variable) +
		geom_vline(xintercept = 0, linetype = "dashed") +
		geom_histogram(aes(y = ..density..), binwidth = bin.width,
									 fill = fill.col, color = line.col,
									 alpha = alpha.h)
	if (add.line & fill.d == F){
		p <- p + geom_density()
	}
	else if (add.line){
		p <- p + geom_density(alpha = alpha.d, fill = fill.d)
	}
	p <- p + geom_rect(data = hpd,
										 aes(xmax = upper, xmin = lower,
										 		ymin = 0, ymax = 1),
										 fill = "blue", alpha = .6) +
		labs(x = paste("posterior log odds of", resp))
	return (p)
}


ggMosaic.plot <- function(data, x, y, myscale = 1, ...){
	# 2 dimensional mosaic plot for ggplot2
	# still needs some work....
	xvar <- deparse(substitute(x))
	yvar <- deparse(substitute(y))
	xlevs <- length(levels(data[, xvar]))
	ylevs <- length(levels(data[, yvar]))
	mydata <- data[c(xvar, yvar)];
	mytable <- table(mydata);
	widths <- c(0, cumsum(apply(mytable, 1, sum)));
	heights <- apply(mytable, 1, function(x){c(0, cumsum(x/sum(x)))});
	
	alldata <- data.frame();
	allnames <- data.frame();
	for(i in 1:nrow(mytable)){
		for(j in 1:ncol(mytable)){
			alldata <- rbind(alldata, c(widths[i], widths[i+1],
																	heights[j, i], heights[j+1, i]));
		}
	}
	colnames(alldata) <- c("xmin", "xmax", "ymin", "ymax")
	# add space between rectangles
	# on horizontal axis, add space scaled to the size of the dataset
	# (if N < 50, set to default 1)
	if(nrow(data) > 50){ myscale <- round(nrow(data)/50, 0) }
	else {myscale <- myscale}
	
	alldata$xmin <- alldata$xmin +
		rep(seq(0, (nrow(mytable) - 1)* myscale,
						by = myscale), each = ncol(mytable))
	
	alldata$xmax <- alldata$xmax +
		rep(seq(0, (nrow(mytable) - 1)*myscale,
						by = myscale), each = ncol(mytable))
	
	# on vertical axis
	yadj <- rep(0.03*0:(ncol(mytable) - 1), nrow(mytable))
	for(i in 1:nrow(alldata)){
		alldata[i, c("ymin", "ymax")] <- alldata[i, c("ymin", "ymax")] + yadj[i]
	}
	
	# add variable names
	alldata[[xvar]] <- factor(rep(dimnames(mytable)[[1]],
																rep(ncol(mytable), nrow(mytable))))
	
	alldata[[yvar]] <- factor(rep(dimnames(mytable)[[2]], nrow(mytable)))
	
	# add chi-squared residuals
	alldata$residuals <- as.data.frame(chisq.test(t(mytable))$residuals)[, "Freq"]
	
	# add columns for plotting the variable labels
	alldata$xvarCenter <- alldata$xmin + (alldata$xmax-alldata$xmin)/2
	ymargins <- prop.table(table(data[, yvar]))
	#alldata$yvarCenter <- c(0, cumsum(ymargins)[1:length(levels(data[, yvar])) - 1]) + ymargins/2
	alldata$yvarCenter <- alldata[1:ylevs, "ymin"] + (alldata[1:ylevs, "ymax"] - alldata[1:ylevs, "ymin"])/2
	
	# for labels
	xlabs <- seq(0, nrow(alldata) - 1, by = nrow(alldata)/xlevs) + 1
	ylabs <- seq(0, nrow(alldata) - 1, by = nrow(alldata)/ylevs) + 1
	
	# plot
	p <- ggplot(alldata, aes(xmin = xmin, xmax = xmax,
													 ymin = ymin, ymax = ymax)) +
		ylim(0, 0.1 + max(alldata$ymax)) +
		geom_rect(color = "black", aes(fill = residuals)) +
		geom_text(data = alldata[xlabs, ],
							aes_string(label = xvar,
												 x = "xvarCenter"), y = .05 + max(alldata$ymax)) +
		geom_text(data = alldata[ylabs, ],
							aes_string(label = as.character(yvar),
												 y = "yvarCenter"), x = - 0, hjust = 1.5) +
		labs(x = "", y = "") + # no axis labels
		scale_fill_gradient2(name = "Standardized\nresiduals",
												 limits = range(alldata$residuals)) +
		#xlim(- max(alldata$xmax)/5, max(alldata$xmax)) +
		theme(panel.grid = element_blank(), axis.text = element_blank(),
					axis.ticks = element_blank())
	
	return(p)
}


ggQQ.plot <- function (data, var) {
	# function for qqnorm plot in ggplot
	# var is a numeric vector
	# following four lines from base R's qqline()
	require(ggplot2)
	
	#e.g. ggQQ.plot(mtcars, wt)
	
	v <- deparse(substitute(var))
	y <- quantile(data[, v], c(0.25, 0.75))
	x <- qnorm(c(0.25, 0.75))
	slope <- diff(y)/diff(x)
	int <- y[1L] - slope * x[1L]
	p <- ggplot(data, aes_string(sample = v)) + stat_qq() +
		geom_abline(slope = slope, intercept = int) +
		labs(title="Normal Q-Q plot")
	return(p)
}


ggViolin.plot <- function(data, x, y, facet = NULL, fill.col = NULL,
													facet.cols = NULL, scales = "fixed", CI = .5){
	# create barplot of proportions with counts superimposed over plots
	# allows for faceting by 1 or 2 groups entered as character vector
	require(ggplot2)
	require(plyr)
	
	#e.g. ggViolin.plot(chickwts, feed, weight)
	
	xvar <- deparse(substitute(x))
	yvar <- deparse(substitute(y))
	if (!is.numeric(data[, yvar])){
		stop("y variable must be numeric")
	}
	if(is.null(fill.col)) {
		fill.col <- "gray"
	}
	else fill.col <- fill.col # for allowing aes colors (tbc...)
	
	if (!is.null(facet)){
		if (length(facet) > 1){
			fvar1 <- facet[1]
			fvar2 <- facet[2]
			p <- ggplot(data, aes_string(xvar, yvar)) +
				geom_violin(fill = fill.col) +
				stat_summary(fun.data = median_hilow,
										 geom = "pointrange",
										 conf.int = CI) +
				facet_grid(as.formula(paste(fvar1, "~", fvar2)),
									 scales = scales)
		}
		else{
			#fvar <- deparse(substitute(facet))
			fvar <- facet
			p <- ggplot(data, aes_string(xvar, yvar)) +
				geom_violin(fill = fill.col) +
				stat_summary(fun.data = median_hilow,
										 geom = "pointrange",
										 conf.int = CI) +
				facet_wrap(as.formula(paste("~", fvar)),
									 ncol = facet.cols, scales = scales)
		}
	}
	else{
		p <- ggplot(data, aes_string(xvar, yvar)) +
			geom_violin(fill = fill.col) +
			stat_summary(fun.data = median_hilow,
									 geom = "pointrange",
									 conf.int = CI)
	}
	
	return(p)
}



#############################################################################
