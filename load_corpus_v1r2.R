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

en_blog <- readRDS("Data/USblogs.Rds")
en_news <- readRDS("Data/USnews.Rds")
en_twit <- readRDS("Data/UStwitter.Rds")

#corpus processing
us_corpus_rds <- paste(data_dir, "us_corpus.Rds", sep="/")

if (!file.exists(us_corpus_rds)) {
    blog_corpus <- corpus(en_blog)
    news_corpus <- corpus(en_news)
    twit_corpus <- corpus(en_twit)

    us_corpus <- blog_corpus + news_corpus + twit_corpus
    saveRDS(us_corpus, us_corpus_rds)
} else {
    us_corpus <- readRDS(us_corpus_rds)
}


