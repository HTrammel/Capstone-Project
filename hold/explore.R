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

if (exists("us_corpus") == FALSE) {
    source("load_corpus_v1r3.R")
}

print("Processing blogs DFM")

blog_dfm_rds <- "Data/blog_corpus_dfm.Rds"

if (!file.exists(blog_dfm_rds)) {
    blog_dfm <- dfm(blog_corpus,
                toLower = FALSE,
                removeSeparators = TRUE,
                concatenator = " ",
                #ignoredFeatures = stopwords("english"),
                ngrams = 1:3
                )
    saveRDS(blog_dfm, blog_dfm_rds)
} else if (exists("blog_dfm") == FALSE) {
    blog_dfm <- readRDS(blog_dfm_rds)
}

print("Processing news DFM")
news_dfm_rds <- "Data/news_corpus_dfm.Rds"

if (!file.exists(news_dfm_rds)) {
    news_dfm <- dfm(news_corpus,
                toLower = FALSE,
                removeSeparators = TRUE,
                concatenator = " ",
                #ignoredFeatures = stopwords("english"),
                ngrams = 1:3
                )
    saveRDS(news_dfm, news_dfm_rds)
} else if (exists("news_dfm") == FALSE) {
    news_dfm <- readRDS(news_dfm_rds)
}

print("Processing twitter DFM")
twit_dfm_rds <- "Data/twit_corpus_dfm.Rds"

if (!file.exists(twit_dfm_rds)) {
    twit_dfm <- dfm(twit_corpus,
                toLower = FALSE,
                removeSeparators = TRUE,
                simplify = TRUE,
                concatenator = " ",
                #ignoredFeatures = stopwords("english"),
                ngrams = 1:3
                )
    saveRDS(twit_dfm, twit_dfm_rds)
} else if (exists("twit_dfm") == FALSE) {
    twit_dfm <- readRDS(twit_dfm_rds)
}

# topBlogWords <- topfeatures(blog_dfm, n = 100)
# topNewsWords <- topfeatures(news_dfm, n = 100)
# topTwitterWords <- topfeatures(twit_dfm, n = 100)


# plot(topfeatures(blog_dfm, 100), log = "y", cex = .6, ylab = "Term frequency")
# plot(topfeatures(news_dfm, 100), log = "y", cex = .6, ylab = "Term frequency")
# plot(topfeatures(twit_dfm, 100), log = "y", cex = .6, ylab = "Term frequency")