#---------------------------------------------------------------------
#
# load_corpus.R
#
# Purpose: Explore text files provided for class
#
#---------------------------------------------------------------------

require(quanteda)
require(dplyr)

if (!file.exists("Data")) { dir.create("Data") }

# sources
data_dir <- "Data"
en_dir <- "Data/final/en_US"

#---------------------------------------------------
# build data frame to hold variables for processing
#---------------------------------------------------
src_df <- NULL
src_df <- data.frame(cbind(src_df,
                           src_type = c("blog","news","twitter")),
                           stringsAsFactors = F)
src_df <- cbind(src_df,
                src_file = c("Data/final/en_US/en_US.blogs.txt",
                             "Data/final/en_US/en_US.news.txt",
                             "Data/final/en_US/en_US.twitter.txt"),
                stringsAsFactors = F)
src_df <- cbind(src_df,
                src_name = c("en_blog","en_news","en_twit"),
                stringsAsFactors = F)
src_df <- cbind(src_df,
                src_lines = c(899288L,1010242L,2360148L))
src_df <- cbind(src_df,
                src_rds = c("Data/USblogs.Rds",
                            "Data/USnews.Rds",
                            "Data/UStwitter.Rds"),
                stringsAsFactors = F)

#-----------------------------------------------
# read files and save as RDS
#-----------------------------------------------
for (i in 1:3) {
    if (!file.exists(src_df[i,]$src_rds)) {
        r <- src_df[i,]
        maxlines <- r$src_lines * 0.05
        con <- file(r$src_file, open="rb")
        tmp <- readLines(con, n=maxlines, encoding="UTF-8", skipNul=TRUE)
        tmp <- gsub("&", "and", tmp)
        saveRDS(tmp, r$src_rds)
        close(con)
        rm("r","tmp","con","maxlines")
    }
}
rm("src_df","i")

print("Processing text files")

if (exists("en_blog") == FALSE) {
    print("   Blog text")
    en_blog <- readRDS("Data/USblogs.Rds")
}
if (exists("en_news") == FALSE) {
    print("   News text")
    en_news <- readRDS("Data/USnews.Rds")
}
if (exists("en_twit") == FALSE) {
    print("   Twitter text")
    en_twit <- readRDS("Data/UStwitter.Rds")
}

#-----------------------------------------------
# corpus processing
#-----------------------------------------------
print("Building corpus files")

all_rds <- paste(data_dir, "all_corpus.Rds", sep="/")
blog_rds <- paste(data_dir, "blog_corpus.Rds", sep="/")
news_rds <- paste(data_dir, "news_corpus.Rds", sep="/")
twitter_rds <- paste(data_dir, "twitter_corpus.Rds", sep="/")

if (!file.exists(blog_rds)) {
    print("   Blog corpus")
    blog_corpus <- corpus(en_blog)
    saveRDS(blog_corpus, blog_rds)
} else if (exists("blog_corpus") == FALSE) {
    blog_corpus <- readRDS(blog_rds)
}

if (!file.exists(news_rds)) {
    print("   News corpus")
    news_corpus <- corpus(en_news)
    saveRDS(news_corpus, news_rds)
} else if (exists("news_corpus") == FALSE) {
    news_corpus <- readRDS(news_rds)
}

if (!file.exists(twitter_rds)) {
    print("   Twitter corpus")
    twitter_corpus <- corpus(en_twit)
    saveRDS(twitter_corpus, twitter_rds)
} else if (exists("twitter_corpus") == FALSE) {
    twitter_corpus <- readRDS(twitter_rds)
}

if (!file.exists(all_rds)) {
    print("   Composite corpus")
    all_corpus <- blog_corpus + news_corpus + twitter_corpus
    saveRDS(all_corpus, all_rds)
} else if (exists("all_corpus") == FALSE) {
    all_corpus <- readRDS(all_rds)
}

#-----------------------------------------------
# build dfm's
#-----------------------------------------------
blog_dfm_rds <- "Data/blog_dfm.Rds"
blog_tok_rds <- "Data/blog_tok.Rds"
news_dfm_rds <- "Data/news_dfm.Rds"
news_tok_rds <- "Data/news_tok.Rds"
twit_dfm_rds <- "Data/twitter_dfm.Rds"
twit_tok_rds <- "Data/twitter_tok.Rds"
all_dfm_rds <- "Data/all_dfm.Rds"
all_tok_rds <- "Data/all_tok.Rds"

print("Processing DFMs")
if (!file.exists(blog_dfm_rds)) {
    print("   blogs tokens")
    blog_tok <- tokenize(blog_corpus,
                         removeSeparators = TRUE,
                         removePunct = TRUE,
                         removeTwitter = TRUE,
                         concatenator = " ",
                         verbose = TRUE,
                         ngrams = 1:3)
    print("   blogs DFM")
    blog_dfm <- dfm(blog_tok,
                    toLower = FALSE)
    saveRDS(blog_tok, blog_tok_rds)
    saveRDS(blog_dfm, blog_dfm_rds)
} else if (exists("blog_dfm") == FALSE) {
    print("   blogs DFM")
    blog_tok <- readRDS(blog_tok_rds)
    blog_dfm <- readRDS(blog_dfm_rds)
}


if (!file.exists(news_dfm_rds)) {
    print("   news tokens")
    news_tok <- tokenize(news_corpus,
                         removeSeparators = TRUE,
                         removePunct = TRUE,
                         removeTwitter = TRUE,
                         concatenator = " ",
                         verbose = TRUE,
                         ngrams = 1:4)
    print("   news DFM")
    news_dfm <- dfm(news_tok,
                    toLower = FALSE)
    saveRDS(news_tok, news_tok_rds)
    saveRDS(news_dfm, news_dfm_rds)
} else if (exists("news_dfm") == FALSE) {
    print("   news DFM")
    news_tok <- readRDS(news_tok_rds)
    news_dfm <- readRDS(news_dfm_rds)
}


if (!file.exists(twit_dfm_rds)) {
    print("   twitter tokens")
    twitter_tok <- tokenize(twitter_corpus,
                         removeSeparators = TRUE,
                         removePunct = TRUE,
                         removeTwitter = TRUE,
                         concatenator = " ",
                         verbose = TRUE,
                         ngrams = 1:4)
    print("   twitter DFM")
    twitter_dfm <- dfm(twitter_tok,
                    toLower = FALSE)
    saveRDS(twitter_tok, twit_tok_rds)
    saveRDS(twitter_dfm, twit_dfm_rds)
} else if (exists("twitter_dfm") == FALSE) {
    print("   twitter DFM")
    twitter_tok <- readRDS(twit_tok_rds)
    twitter_dfm <- readRDS(twit_dfm_rds)
}

if (!file.exists(all_dfm_rds)) {
    print("   all tokens")
    all_tok <- tokenize(all_corpus,
                        removeSeparators = TRUE,
                        removePunct = TRUE,
                        removeTwitter = TRUE,
                        concatenator = " ",
                        verbose = TRUE,
                        ngrams = 1:4)
    print("   all DFM")
    all_dfm <- dfm(all_tok,
                    toLower = FALSE)
    saveRDS(all_tok, all_tok_rds)
    saveRDS(all_dfm, all_dfm_rds)
} else if (exists("all_dfm") == FALSE) {
    print("   all DFM")
    all_tok <- readRDS(all_tok_rds)
    all_dfm <- readRDS(all_dfm_rds)
}

