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

blogs_txt <- "en_US.blogs.txt"
news_txt <- "en_US.news.txt"
twit_txt <- "en_US.twitter.txt"

en_blogs <- paste(en_dir, blogs_txt, sep="/")
en_news <- paste(en_dir, news_txt, sep="/")
en_twit <- paste(en_dir, twit_txt, sep="/")

data_dir <- "Data"
en_blg_rds <- paste(data_dir, "USblogs.Rds", sep="/")
en_nws_rds <- paste(data_dir, "USnews.Rds", sep="/")
en_twt_rds <- paste(data_dir, "UStwitter.Rds", sep="/")
us_corpus_rds <- paste(data_dir, "us_corpus.Rds", sep="/")
tdm_rds <- paste(data_dir, "tdm.Rds", sep="/")
dtm_rds <- paste(data_dir, "dtm.Rds", sep="/")

# strsplit_space_tokenizer <- function(x)
#     unlist(strsplit(as.character(x), "[[:space:]]+"))

# ctrl <- list(tokenize = strsplit_space_tokenizer,
#              removePunctuation = list(preserve_intra_word_dashes = TRUE),
#              stemming = FALSE)

get_lines <- function(c) {
    maxlines <- 20000
    out <- readLines(con, n=maxlines, encoding="UTF-8")
}


if (!file.exists(en_blg_rds)) {
    con <- file(en_news, open="rb")
    tmp <- get_lines(con)
    close(con)
    blog_corpus <- VCorpus(DirSource(directory=en_dir, encoding="UTF-8", pattern=blogs_txt),
                            readerControl = list(reader = readPlain))
    saveRDS(blog_corpus, en_blg_rds)
    rm(list=c("tmp","con"))
} else {
    blog_corpus <- readRDS(en_blg_rds)
}

if (!file.exists(en_nws_rds)) {
    con <- file(en_news, open="rb")
    tmp <- get_lines(con)
    close(con)
    news_corpus <- VCorpus(DirSource(directory=en_dir, encoding="UTF-8", pattern=news_txt),
                            readerControl = list(reader = readPlain))
    saveRDS(news_corpus, en_nws_rds)
    rm(list=c("tmp","con"))
} else {
    news_corpus <- readRDS(en_nws_rds)
}

if (!file.exists(en_twt_rds)) {
    con <- file(en_news, open="rb")
    tmp <- get_lines(con)
    close(con)
    twit_corpus <- VCorpus(DirSource(directory=en_dir, encoding="UTF-8", pattern=twit_txt),
                            readerControl = list(reader = readPlain))
    saveRDS(twit_corpus, en_twt_rds)
    rm(list=c("tmp","con"))
} else {
    twit_corpus <- readRDS(en_twt_rds)
}

if (!file.exists(us_corpus_rds)) {
    us_corpus <- c(blog_corpus, news_corpus, twit_corpus)
    saveRDS(us_corpus, us_corpus_rds)
} else {
    us_corpus <- readRDS(us_corpus_rds)
}

# if (!file.exists(tdm_rds)) {
#     tdm <- TermDocumentMatrix(us_corpus, control = ctrl)
#     saveRDS(tdm, tdm_rds)
# } else {
#     us_corpus <- readRDS(tdm_rds)
# }

if (!file.exists(dtm_rds)) {
    dtm <- DocumentTermMatrix(us_corpus)
    saveRDS(dtm, dtm_rds)
} else {
    us_corpus <- readRDS(dtm_rds)
}


#rm(c(tmp,con))

# adData = data.frame(diagnosis,predictors)
# inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
# training = adData[ inTrain,]
# testing = adData[-inTrain,]
