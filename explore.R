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

data_dir <- "Data"
en_blg_rds <- paste(data_dir, "USblogs.Rds", sep="/")
en_nws_rds <- paste(data_dir, "USnews.Rds", sep="/")
en_twt_rds <- paste(data_dir, "UStwitter.Rds", sep="/")
us_corpus_rds <- paste(data_dir, "us_corpus.Rds", sep="/")

if (!file.exists(en_blg_rds)) {
    con <- file(en_news, open="rb")
    tmp <- readLines(con, encoding="UTF-8")
    close(con)
    USblogs <- data.frame(1:length(tmp),tmp,stringsAsFactors = F)
    names(USblogs) <- c("txt_num","txt_val")
    saveRDS(USblogs, en_blg_rds)
    rm(tmp)
    rm(con)
} else {
    USblogs <- readRDS(en_blg_rds)
}

if (!file.exists(en_nws_rds)) {
    con <- file(en_news, open="rb")
    tmp <- readLines(con, encoding="UTF-8")
    close(con)
    USnews  <- data.frame(1:length(tmp),tmp,stringsAsFactors = F)
    names(USnews) <- c("txt_num","txt_val")
    saveRDS(USnews, en_nws_rds)
    rm(tmp)
    rm(con)
} else {
    USnews <- readRDS(en_nws_rds)
}

if (!file.exists(en_twt_rds)) {
    con <- file(en_news, open="rb")
    tmp <- readLines(con, encoding="UTF-8")
    close(con)
    UStwitter <- data.frame(1:length(tmp),tmp,stringsAsFactors = F)
    names(UStwitter) <- c("txt_num","txt_val")
    saveRDS(UStwitter, en_twt_rds)
    rm(tmp)
    rm(con)
} else {
    UStwitter <- readRDS(en_twt_rds)
}

if (!file.exists(us_corpus_rds)) {
    us_corpus <- Corpus(ZipSource("Data/Coursera-SwiftKey.zip", pattern="en.US", recursive = T), readerControl = list(language = "en"))
    saveRDS(us_corpus, us_corpus_rds)
} else {
    us_corpus <- readRDS(us_corpus_rds)
}


#rm(c(tmp,con))

# adData = data.frame(diagnosis,predictors)
# inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
# training = adData[ inTrain,]
# testing = adData[-inTrain,]
