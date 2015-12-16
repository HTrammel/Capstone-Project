#---------------------------------------------------------------------
#
# load_corpus.R
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
tdm_rds <- paste(data_dir, "tdm.Rds", sep="/")

play_set <- function(df) {
    set.seed(12345)
    inPlay <- rbinom(nrow(df),1,0.1)
    df[inPlay,]
}


if (!file.exists(en_blg_rds)) {
    con <- file(en_news, open="rb")
    tmp <- readLines(con, encoding="UTF-8")
    close(con)
    df <- data.frame(1:length(tmp),tmp,stringsAsFactors = F)
    names(df) <- c("txt_num","txt_val")
    USblogs <- play_set(df)
    blog_corpus <- Corpus(DataframeSource(USblogs))
    saveRDS(USblogs, en_blg_rds)
    rm(list=c("tmp","con","df"))
} else {
    USblogs <- readRDS(en_blg_rds)
}

if (!file.exists(en_nws_rds)) {
    con <- file(en_news, open="rb")
    tmp <- readLines(con, encoding="UTF-8")
    close(con)
    df  <- data.frame(1:length(tmp),tmp,stringsAsFactors = F)
    names(df) <- c("txt_num","txt_val")
    USnews <- play_set(df)
    news_corpus <- Corpus(DataframeSource(USnews))
    saveRDS(USnews, en_nws_rds)
    rm(list=c("tmp","con","df"))
} else {
    USnews <- readRDS(en_nws_rds)
}

if (!file.exists(en_twt_rds)) {
    con <- file(en_news, open="rb")
    tmp <- readLines(con, encoding="UTF-8")
    close(con)
    df  <- data.frame(1:length(tmp),tmp,stringsAsFactors = F)
    names(df) <- c("txt_num","txt_val")
    UStwitter <- play_set(df)
    twit_corpus <- Corpus(DataframeSource(UStwitter))
    saveRDS(UStwitter, en_twt_rds)
    rm(list=c("tmp","con","df"))
} else {
    UStwitter <- readRDS(en_twt_rds)
}

us_corpus <- c(blog_corpus, news_corpus, twit_corpus)
saveRDS(us_corpus, us_corpus_rds)

tdm <- TermDocumentMatrix(us_corpus)
saveRDS(tdm, tdm_rds)

#rm(c(tmp,con))

# adData = data.frame(diagnosis,predictors)
# inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
# training = adData[ inTrain,]
# testing = adData[-inTrain,]
