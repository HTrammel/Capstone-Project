#---------------------------------------------------------------------
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

# read files and save as RDS
for (i in 1:3) {
    if (!file.exists(src_df[i,]$src_rds)) {
        r <- src_df[i,]
        maxlines <- r$src_lines * 0.05
        con <- file(r$src_file, open="rb")
        tmp <- readLines(con, n=maxlines, encoding="UTF-8", skipNul=TRUE)
        saveRDS(tmp, r$src_rds)
        close(con)
        rm("r","tmp","con","maxlines")
    }
}
rm("src_df","i")


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


#corpus processing
print("Building corpus files")
#corpus processing
us_mini_rds <- paste(data_dir, "us_mini_corpus.Rds", sep="/")
blog_mini_rds <- paste(data_dir, "blog_mini_corpus.Rds", sep="/")
news_mini_rds <- paste(data_dir, "news_mini_corpus.Rds", sep="/")
twitter_mini_rds <- paste(data_dir, "twitter_mini_corpus.Rds", sep="/")

if (!file.exists(blog_mini_rds)) {
    print("   Blog corpus")
    blog_mini_corpus <- corpus(en_blog)
    saveRDS(blog_mini_corpus, blog_mini_rds)
} else if (exists("blog_mini_corpus") == FALSE) {
    blog_mini_corpus <- readRDS(blog_mini_rds)
}
if (!file.exists(news_mini_rds)) {
    print("   News corpus")
    news_mini_corpus <- corpus(en_news)
    saveRDS(news_mini_corpus, news_mini_rds)
} else if (exists("news_mini_corpus") == FALSE) {
    news_mini_corpus <- readRDS(news_mini_rds)
}
if (!file.exists(twitter_mini_rds)) {
    print("   Twitter corpus")
    twit_mini_corpus <- corpus(en_twit)
    saveRDS(twit_mini_corpus, twitter_mini_rds)
} else if (exists("twit_mini_corpus") == FALSE) {
    twit_mini_corpus <- readRDS(twitter_mini_rds)
}
if (!file.exists(us_mini_rds)) {
    print("   Composite corpus")
    us_mini_corpus <- blog_mini_corpus + news_mini_corpus + twit_mini_corpus
    saveRDS(us_mini_corpus, us_mini_rds)
} else if (exists("us_mini_corpus") == FALSE) {
    us_mini_corpus <- readRDS(us_mini_rds)
}


