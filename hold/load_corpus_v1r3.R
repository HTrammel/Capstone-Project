hea#---------------------------------------------------------------------
#
# load_corpus.R
#
# Purpose: Explore text files provided for class
# NOTE: rJava requires 32-bit R because I have 32-bit Java
#
#---------------------------------------------------------------------

require(quanteda)
require(dplyr)

if (!file.exists("Data")) { dir.create("Data") }

# sources
data_dir <- "Data"
en_dir <- "Data/final/en_US"

# build data frame to hold variables for processing
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
src_df <- cbind(src_df,
                src_tf = c("Data/USblogs_tf.Rds",
                            "Data/USnews_tf.Rds",
                            "Data/UStwitter_tf.Rds"),
                stringsAsFactors = F)

print("Processing text files")
# read files and save as RDS
for (i in 1:3) {
    if (!file.exists(src_df[i,]$src_tf)) {
        r <- src_df[i,]
        tmp <- textfile(r$src_file)
        saveRDS(tmp, r$src_tf)
        rm("r","tmp")
    }
}
rm("src_df","i")

print("   Blog text")
if (exists("en_blog_tf") == FALSE) {
    en_blog_tf <- readRDS("Data/USblogs_tf.Rds")
}
print("   News text")
if (exists("en_news_tf") == FALSE) {
    en_news_tf <- readRDS("Data/USnews_tf.Rds")
}
print("   Twitter text")
if (exists("en_twit_tf") == FALSE) {
    en_twit_tf <- readRDS("Data/UStwitter_tf.Rds")
}


print("Building corpus files")
#corpus processing
us_corpus_rds <- paste(data_dir, "us_corpus.Rds", sep="/")
blog_corpus_rds <- paste(data_dir, "blog_corpus.Rds", sep="/")
news_corpus_rds <- paste(data_dir, "news_corpus.Rds", sep="/")
twitter_corpus_rds <- paste(data_dir, "twitter_corpus.Rds", sep="/")

print("   Blog corpus")
if (!file.exists(blog_corpus_rds)) {
    blog_corpus <- corpus(en_blog_tf)
    saveRDS(blog_corpus, blog_corpus_rds)
} else if (exists("blog_corpus") == FALSE) {
    blog_corpus <- readRDS(blog_corpus_rds)
}
print("   News corpus")
if (!file.exists(news_corpus_rds)) {
    news_corpus <- corpus(en_news_tf)
    saveRDS(news_corpus, news_corpus_rds)
} else if (exists("news_corpus") == FALSE) {
    news_corpus <- readRDS(news_corpus_rds)
}
print("   Twitter corpus")
if (!file.exists(twitter_corpus_rds)) {
    twit_corpus <- corpus(en_twit_tf)
    saveRDS(twit_corpus, twitter_corpus_rds)
} else if (exists("twit_corpus") == FALSE) {
    twit_corpus <- readRDS(twitter_corpus_rds)
}
print("   Composite corpus")
if (!file.exists(us_corpus_rds)) {
    us_corpus <- blog_corpus + news_corpus + twit_corpus
    saveRDS(us_corpus, us_corpus_rds)
} else if (exists("us_corpus") == FALSE) {
    us_corpus <- readRDS(us_corpus_rds)
}


