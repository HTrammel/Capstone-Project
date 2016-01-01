#---------------------------------------------------------------------
#
# Explore.R
#
# Purpose: Explore text files provided for class
# 
#---------------------------------------------------------------------
require(quanteda)
require(dplyr)

if (exists("all_corpus") == FALSE) {
    source("load_corpus_mini.R")
}

#------------------------------------------------
# topWords
#------------------------------------------------

topBlogWords <- topfeatures(blog_dfm, n = 500)
topNewsWords <- topfeatures(news_dfm, n = 500)
topTwitterWords <- topfeatures(twit_dfm, n = 500)
topWords <- topfeatures(all_dfm, n = 500)

#------------------------------------------------
# topFeatures
#------------------------------------------------

# plot(topfeatures(blog_dfm, 100), log = "y", cex = .6, ylab = "Term frequency")
# plot(topfeatures(news_dfm, 100), log = "y", cex = .6, ylab = "Term frequency")
# plot(topfeatures(twit_dfm, 100), log = "y", cex = .6, ylab = "Term frequency")
# plot(topfeatures(all_dfm, 100), log = "y", cex = .6, ylab = "Term frequency")
