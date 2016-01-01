#---------------------------------------------------------------------
#
# Explore_Backoff.R
#
# Purpose: Implementation of "stupid backoff"
#
#---------------------------------------------------------------------
require(quanteda)y
require(dplyr)

if (!file.exists("Data")) { dir.create("Data") }

bottom <- 5
training <- 0.5

# sources
data_dir <- "Data"
en_dir <- "Data/final/en_US"


map_vocabulary <- function(lines) {
    print("Making vocabulary")
    lines <- toLower(lines)
    chunks <- tokenize(lines,
                       what = "word",
                       removeSeparators = TRUE,
                       removeNumbers = TRUE,
                       removePunct = TRUE,
                       removeTwitter = TRUE
                       )
    words <- do.call(c, chunks)
    words <- as.factor(words)
}

map_ngrams <- function(lines) {
    print("Making ngrams")
    lines <- toLower(lines)
    chunks <- tokenize(lines,
                       what = "word",
                       removeSeparators = TRUE,
                       removeNumbers = TRUE,
                       removePunct = TRUE,
                       removeTwitter = TRUE,
                       concatenator = " ",
                       ngrams = 1:5
                    )
    words <- do.call(c, chunks)
    words <- as.factor(words)
}

reduce_vocab <- function(c_factor) {
    word_freq <- table(c_factor)
    df <- data.frame(word_freq)
}


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
en_all = NULL
for (i in 1:3) {
    if (!file.exists(src_df[i,]$src_rds)) {
        r <- src_df[i,]
        maxlines <- r$src_lines * training
        con <- file(r$src_file, open="rb")
        tmp <- readLines(con,
                         n=maxlines,
                         encoding="UTF-8",
                         skipNul=TRUE)
        tmp <- gsub("&", "and", tmp)
        tmp <- gsub("_", " ", tmp)
        saveRDS(tmp, r$src_rds)
        close(con)
        en_all <- c(en_all, tmp)
        saveRDS(en_all, "Data/US_all.Rds")
        rm("r","tmp","con","maxlines")
    }
}
rm("src_df","i")

print("Processing text files")

if (!exists("en_blog")) {
    print("   Blog text")
    en_blog <- readRDS("Data/USblogs.Rds")
}
if (!exists("en_news")) {
    print("   News text")
    en_news <- readRDS("Data/USnews.Rds")
}
if (!exists("en_twit")) {
    print("   Twitter text")
    en_twit <- readRDS("Data/UStwitter.Rds")
}

# en_all <- readRDS("Data/US_all.Rds")


if (!file.exists("Data/vocab.Rds")) {
  vocab <- map_vocabulary(en_all)
  saveRDS(vocab, "Data/vocab.Rds")
} else {
  vocab <- readRDS("Data/vocab.Rds")
}

if (!file.exists("Data/blog_ng.Rds")) {
  blog_ng <- map_ngrams(en_blog)
  saveRDS(blog_ng, "Data/blog_ng.Rds")
  rm("en_blog")
}

if (!file.exists("Data/news_ng.Rds")) {
  news_ng <- map_ngrams(en_news)
  saveRDS(news_ng, "Data/news_ng.Rds")
  rm("en_news")
}

if (!file.exists("Data/twit_ng.Rds")) {
  twit_ng <- map_ngrams(en_twit)
  saveRDS(twit_ng, "Data/twit_ng.Rds")
  rm("en_twit")
}

if (!"blog_ng") {
  blog_ng <- readRDS("Data/blog_ng.Rds")
}

if (!"news_ng") {
  news_ng <- readRDS("Data/news_ng.Rds")
}

if (!"twit_ng") {
  twit_ng <- readRDS("Data/twit_ng.Rds")
}

# r_vocab <- reduce_vocab(vocab)
# r_ngrams <- reduce_vocab(n_grams)
