############################################################################
#
# Data setup for MCMCglmm modeling of Brown relativizers
# 
# Jason Grafmiller
############################################################################


# Load raw data ---------------------------------------------------------

# load data file:
relativizers <- read.delim("data/relative_clauses.txt", strip.white = T,
			  stringsAsFactors = T)

# Custom functions ------------------------------------------------------

filter.infrequent <- function(words, threshold = 5, dummy = "OTHER") {
  # code from WBRS for recoding infrequent factor levels (default is <= 5
  # observations)
  return(relevel(
    as.factor(
      ifelse(words %in% levels(as.factor(words))[table(words) >= threshold],
        as.character(words), dummy)), dummy))
}

as.numeric2 <- function(x, cols = NULL, as.char = FALSE) {
  # same as 'as.numeric()' but can be applied to an entire dataframe or matrix
  # will return NAs for character vectors that are not numbers
  if (is.null(cols)) {
    cols <- 1:ncol(x)
  } else {
    cols <- cols
  }
  if (as.char) {
    df <- as.data.frame(
      lapply(x[cols], FUN = function(v) as.numeric(as.character(v)))
    )
  }
  else {
    df <- as.data.frame(
      lapply(x[cols], FUN = function(v) as.numeric(v))
    )
  }
  return(df)
}

# Prepare dataframe -----------------------------------------------------

### NOTE: the dataset that we create here is not 100% identical to the 
# dataset analyzed in Hinrichs et al. (2015)
# (there we weeded out garbage in a slightly more ad-hoc fashion ...)

# recode stuff
# order corpus levels
relativizers$corpus <- factor(relativizers$corpus,
	levels = c("Brown", "LOB", "Frown", "FLOB"))

relativizers$genre <- relevel(relativizers$genre, ref = "press")

# antNum2: new factor of antecedent number
relativizers$antNum2 <- car::recode(as.factor(relativizers$antNum), 
	" '0' = 'other';
	'1' = 'singular';
	'2' = 'plural' ")

# antPOS2: binary factor for Ant POS 
relativizers$antPOS2 <- factor(ifelse(relativizers$antPOS == 'N',
	'noun', 'other'))


# time2: binary factor of time
relativizers$time2 <- as.factor(relativizers$time)

relativizers$prunedAntHead <- filter.infrequent(relativizers$antHead, 4)

# get the plain text of the construction:
relativizers$plainText <- gsub("<.*?>", "", as.character(relativizers$construction))
relativizers$plainText <- gsub("[[:punct:]]", "", relativizers$plainText)
relativizers$plainText <- trim(relativizers$plainText)

# renames some things
names(relativizers)[c(26:27, 32, 35)] <- c("meanWordLen", "meanSentLen", 
	"passiveActiveRatio", "nounVerbRatio")


# Split data by relativizer function ------------------------------------

# obj function RC dataset
objrel <- droplevels(subset(relativizers, relFct == "Obj"))
# prunedAntHead
objrel$prunedAntHead <- filter.infrequent(objrel$antHead, 4)

# subj RC dataset
subjrel <- droplevels(subset(relativizers, relFct == "Subj"))
subjrel$prunedAntHead <- filter.infrequent(subjrel$antHead, 4)

# remove construction and context columns
objrel <- objrel[, -c(42)]
subjrel <- subjrel[, -c(42)]

# remove unnecessary data
# rm(list = c("relativizers")) 


############################################################################
############################################################################
