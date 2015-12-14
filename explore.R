#---------------------------------------------------------------------
#
# Explore.R
#
# Purpose: Explore text files provided for class
# NOTE: rJava requires 32-bit R because I have 32-bit Java
#
#---------------------------------------------------------------------

require(tm)
require(stringr)

if (!file.exists("Data")) { dir.create("Data") }

en_dir <- "Data/final/en_US"

en_blogs <- paste(en_dir, "en_US.blogs.txt", sep="/")
en_news <- paste(en_dir, "en_US.news.txt", sep="/")
en_twit <- paste(en_dir, "en_US.twitter.txt", sep="/")

en_blg_rds <- paste(en_dir, "USblogs.Rds", sep="/")
en_nws_rds <- paste(en_dir, "USnews.Rds", sep="/")
en_twt_rds <- paste(en_dir, "UStwitter.Rds", sep="/")

if (!file.exists(en_blg_rds)) {
    con <- file(en_news, open="rb")
    USblogs <- readLines(con, encoding="UTF-8")
    close(con)
    saveRDS(USblogs, en_blg_rds)
} else {
    USblogs <- readRDS(en_blg_rds)
}

if (!file.exists(en_nws_rds)) {
    con <- file(en_news, open="rb")
    USnews <- readLines(con, encoding="UTF-8")
    close(con)
    saveRDS(USnews, en_nws_rds)
} else {
    USnews <- readRDS(en_nws_rds)
}

if (!file.exists(en_twt_rds)) {
    con <- file(en_news, open="rb")
    UStwitter <- readLines(con, encoding="UTF-8")
    close(con)
    saveRDS(UStwitter, en_twt_rds)
} else {
    UStwitter <- readRDS(en_twt_rds)
}

blog_len <- length(USblogs)
news_len <- length(USnews)
twit_len <- length(UStwitter)





#-----------------------------------------------------------
# ru_dir <- "Data/final/ru_RU"
# ru_blogs <- paste(ru_dir, "ru_RU.blogs.txt", sep="/")
# ru_news <- paste(ru_dir, "ru_RU.news.txt", sep="/")
# ru_twit <- paste(ru_dir, "ru_RU.twitter.txt", sep="/")
#
# fi_dir <- "Data/final/fi_FI"
# fi_blogs <- paste(fi_dir, "fi_FI.blogs.txt", sep="/")
# fi_news <- paste(fi_dir, "fi_FI.news.txt", sep="/")
# fi_twit <- paste(fi_dir, "fi_FI.twitter.txt", sep="/")
#
# de_dir <- "Data/final/de_DE"
# de_blogs <- paste(de_dir, "de_DE.blogs.txt", sep="/")
# de_news <- paste(de_dir, "de_DE.news.txt", sep="/")
# de_twit <- paste(de_dir, "de_DE.twitter.txt", sep="/")

