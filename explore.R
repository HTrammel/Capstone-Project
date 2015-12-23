#---------------------------------------------------------------------
#
# Explore.R
#
# Purpose: Explore text files provided for class
# NOTE: rJava requires 32-bit R because I have 32-bit Java
#
#---------------------------------------------------------------------
require(quanteda)
require(dplyr)

#source("load_corpus.R")
us_corpus_dfm_rds <- "Data/us_corpus_dfm.Rds"

if (!file.exists(us_corpus_dfm_rds)) {
    us_dfm <- dfm(us_corpus,
                toLower = TRUE,
                removeSeparators = TRUE,
                concatenator = " ",
                ignoredFeatures = stopwords("english"),
                ngrams = 1:3
                )
    saveRDS(us_dfm, us_corpus_dfm_rds)
} else {
    us_dfm <- readRDS(us_corpus_rds)
}

topWords <- topfeatures(us_dfm, n = 100)

plot(topfeatures(us_dfm, 100), log = "y", cex = .6, ylab = "Term frequency")