#---------------------------------------------------------------------
#
# Explore_Backoff.R
#
# Purpose: Implementation of "stupid backoff"
#
#---------------------------------------------------------------------
require(quanteda)
require(stringr)
require(dplyr)
require(data.table)

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
#en_all = NULL
for (i in 1:3) {
    if (!file.exists(src_df[i,]$src_rds)) {
        r <- src_df[i,]
        maxlines <- r$src_lines * 0.05
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

en_all <- readRDS("Data/US_all.Rds")
# if (!exists("en_all")) {
#     if (is.null("en_all")) {
#         print("   all text")
#         if (!file.exists("Data/US_all.Rds")) {
#             en_all <- NULL
#             en_all <- c(en_blog, en_news, en_twit)
#             saveRDS(en_all, "Data/US_all.Rds")
#         } else {
#             en_all <- readRDS("Data/US_all.Rds")
#         }
#     }
# }

make_vocabulary <- function(lines) {
    print("Making vocabulary")
    chunks <- tokenize(lines,
                       what = "word",
                       removeSeparators = TRUE,
                       removeNumbers = TRUE,
                       removePunct = TRUE,
                       removeTwitter = TRUE
                       )
    words <- do.call(c, chunks)
    word_freq <- table(words)
    dt <- data.table(word_freq)
    setkey(dt, N)
    dt <- dt[N > 5]
}

make_ngrams <- function(lines) {
    print("Making ngrams")
    chunks <- tokenize(lines,
                       what = "word",
                       removeSeparators = TRUE,
                       removeNumbers = TRUE,
                       removePunct = TRUE,
                       removeTwitter = TRUE,
                       concatenator = " ",
                       ngrams = 1:4
                    )
    words <- do.call(c, chunks)
    word_freq <- table(words)
    dt <- data.table(word_freq)
    setkey(dt, N)
    dt <- dt[N > 5]
}

vocab <- make_vocabulary(en_all)

n_grams <- make_ngrams(en_all)
